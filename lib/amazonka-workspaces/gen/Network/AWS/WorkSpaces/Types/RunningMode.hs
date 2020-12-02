{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RunningMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.RunningMode where

import Network.AWS.Prelude

data RunningMode
  = AlwaysOn
  | AutoStop
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

instance FromText RunningMode where
  parser =
    takeLowerText >>= \case
      "always_on" -> pure AlwaysOn
      "auto_stop" -> pure AutoStop
      e ->
        fromTextError $
          "Failure parsing RunningMode from value: '" <> e
            <> "'. Accepted values: always_on, auto_stop"

instance ToText RunningMode where
  toText = \case
    AlwaysOn -> "ALWAYS_ON"
    AutoStop -> "AUTO_STOP"

instance Hashable RunningMode

instance NFData RunningMode

instance ToByteString RunningMode

instance ToQuery RunningMode

instance ToHeader RunningMode

instance ToJSON RunningMode where
  toJSON = toJSONText

instance FromJSON RunningMode where
  parseJSON = parseJSONText "RunningMode"
