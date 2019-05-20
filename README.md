was supposed to make event handling less annoying in brick, made it harder instead x)
```haskell
handleEvent ui e =
  case e of
    VtyEvent (V.EvKey (V.KChar k) []) ->
      case k of
        'l' -> exec (shift Right) ui
        'h' -> exec (shift Left) ui
        'j' -> exec (shift Down) ui
        'k' -> exec (shift Up) ui
        'r' -> restart ui
        'p' -> guarded (not . view locked) (over paused not) ui
        'q' -> halt ui
        ' ' ->
          guarded
            (not . view paused)
            (over game (execTetris hardDrop) . set locked True)
            ui
        _ -> continue ui
    VtyEvent (V.EvKey k []) ->
      case k of
        V.KRight -> exec (shift Right) ui
        V.KLeft -> exec (shift Left) ui
        V.KDown -> exec (shift Down) ui
        V.KUp -> exec (shift Up) ui
        _ -> continue ui
    AppEvent Tick -> handleTick ui
    _ -> continue ui

handleEvent ui k =
  ($ ui) $
  fromMaybe continue $
  flip runPattern k  $
  foldr (<|>) empty  $
  [ appEvent Tick handleTick
  , keySpecial V.KRight [] $ exec (shift Right)
  , keySpecial V.KLeft  [] $ exec (shift Left)
  , keySpecial V.KDown  [] $ exec (shift Down)
  , keySpecial V.KUp    [] $ exec (shift Up)
  , key 'l' [] $ exec (shift Right)
  , key 'h' [] $ exec (shift Left)
  , key 'j' [] $ exec (shift Down)
  , key 'k' [] $ exec (shift Up)
  , key ' ' [] $
    guarded
      (not . view paused)
      (over game (execTetris hardDrop) . set locked True)
  , key 'p' [] $ guarded (not . view locked) (over paused not)
  , key 'r' [] restart
  , key 'q' [] halt
  ]
```
