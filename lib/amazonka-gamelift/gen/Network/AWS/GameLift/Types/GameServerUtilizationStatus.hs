{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerUtilizationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerUtilizationStatus where

import Network.AWS.Prelude

data GameServerUtilizationStatus
  = Available
  | Utilized
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

instance FromText GameServerUtilizationStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "utilized" -> pure Utilized
      e ->
        fromTextError $
          "Failure parsing GameServerUtilizationStatus from value: '" <> e
            <> "'. Accepted values: available, utilized"

instance ToText GameServerUtilizationStatus where
  toText = \case
    Available -> "AVAILABLE"
    Utilized -> "UTILIZED"

instance Hashable GameServerUtilizationStatus

instance NFData GameServerUtilizationStatus

instance ToByteString GameServerUtilizationStatus

instance ToQuery GameServerUtilizationStatus

instance ToHeader GameServerUtilizationStatus

instance ToJSON GameServerUtilizationStatus where
  toJSON = toJSONText

instance FromJSON GameServerUtilizationStatus where
  parseJSON = parseJSONText "GameServerUtilizationStatus"
