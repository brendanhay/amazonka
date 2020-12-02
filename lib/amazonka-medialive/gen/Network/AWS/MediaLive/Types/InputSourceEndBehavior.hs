{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSourceEndBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSourceEndBehavior where

import Network.AWS.Prelude

-- | Input Source End Behavior
data InputSourceEndBehavior
  = Continue
  | Loop
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText InputSourceEndBehavior where
  parser =
    takeLowerText >>= \case
      "continue" -> pure Continue
      "loop" -> pure Loop
      e ->
        fromTextError $
          "Failure parsing InputSourceEndBehavior from value: '" <> e
            <> "'. Accepted values: continue, loop"

instance ToText InputSourceEndBehavior where
  toText = \case
    Continue -> "CONTINUE"
    Loop -> "LOOP"

instance Hashable InputSourceEndBehavior

instance NFData InputSourceEndBehavior

instance ToByteString InputSourceEndBehavior

instance ToQuery InputSourceEndBehavior

instance ToHeader InputSourceEndBehavior

instance ToJSON InputSourceEndBehavior where
  toJSON = toJSONText

instance FromJSON InputSourceEndBehavior where
  parseJSON = parseJSONText "InputSourceEndBehavior"
