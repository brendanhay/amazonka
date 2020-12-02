{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TargetSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TargetSelection where

import Network.AWS.Prelude

data TargetSelection
  = Continuous
  | Snapshot
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

instance FromText TargetSelection where
  parser =
    takeLowerText >>= \case
      "continuous" -> pure Continuous
      "snapshot" -> pure Snapshot
      e ->
        fromTextError $
          "Failure parsing TargetSelection from value: '" <> e
            <> "'. Accepted values: continuous, snapshot"

instance ToText TargetSelection where
  toText = \case
    Continuous -> "CONTINUOUS"
    Snapshot -> "SNAPSHOT"

instance Hashable TargetSelection

instance NFData TargetSelection

instance ToByteString TargetSelection

instance ToQuery TargetSelection

instance ToHeader TargetSelection

instance ToJSON TargetSelection where
  toJSON = toJSONText

instance FromJSON TargetSelection where
  parseJSON = parseJSONText "TargetSelection"
