{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.PointInTimeRecoveryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.PointInTimeRecoveryStatus where

import Network.AWS.Prelude

data PointInTimeRecoveryStatus
  = PITRSDisabled
  | PITRSEnabled
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

instance FromText PointInTimeRecoveryStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure PITRSDisabled
      "enabled" -> pure PITRSEnabled
      e ->
        fromTextError $
          "Failure parsing PointInTimeRecoveryStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText PointInTimeRecoveryStatus where
  toText = \case
    PITRSDisabled -> "DISABLED"
    PITRSEnabled -> "ENABLED"

instance Hashable PointInTimeRecoveryStatus

instance NFData PointInTimeRecoveryStatus

instance ToByteString PointInTimeRecoveryStatus

instance ToQuery PointInTimeRecoveryStatus

instance ToHeader PointInTimeRecoveryStatus

instance FromJSON PointInTimeRecoveryStatus where
  parseJSON = parseJSONText "PointInTimeRecoveryStatus"
