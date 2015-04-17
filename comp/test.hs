{-# LANGUAGE LambdaCase #-}

module Main where

instance FromText LifecycleState where
    parser = do
        x <- takeLowerText
        case x of
            "detached" -> pure Detached
            _          -> fail ("Failure parsing LifecycleState from " ++ show x)

instance ToText SomeEnum where
    toText x = case x of
        SomeCtor -> "text"
