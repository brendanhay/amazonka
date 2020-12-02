{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContributorInsightsStatus where

import Network.AWS.Prelude

data ContributorInsightsStatus
  = CISDisabled
  | CISDisabling
  | CISEnabled
  | CISEnabling
  | CISFailed
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

instance FromText ContributorInsightsStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure CISDisabled
      "disabling" -> pure CISDisabling
      "enabled" -> pure CISEnabled
      "enabling" -> pure CISEnabling
      "failed" -> pure CISFailed
      e ->
        fromTextError $
          "Failure parsing ContributorInsightsStatus from value: '" <> e
            <> "'. Accepted values: disabled, disabling, enabled, enabling, failed"

instance ToText ContributorInsightsStatus where
  toText = \case
    CISDisabled -> "DISABLED"
    CISDisabling -> "DISABLING"
    CISEnabled -> "ENABLED"
    CISEnabling -> "ENABLING"
    CISFailed -> "FAILED"

instance Hashable ContributorInsightsStatus

instance NFData ContributorInsightsStatus

instance ToByteString ContributorInsightsStatus

instance ToQuery ContributorInsightsStatus

instance ToHeader ContributorInsightsStatus

instance FromJSON ContributorInsightsStatus where
  parseJSON = parseJSONText "ContributorInsightsStatus"
