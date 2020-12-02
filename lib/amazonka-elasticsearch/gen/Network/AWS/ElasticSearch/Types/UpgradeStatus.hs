{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.UpgradeStatus where

import Network.AWS.Prelude

data UpgradeStatus
  = USFailed
  | USInProgress
  | USSucceeded
  | USSucceededWithIssues
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

instance FromText UpgradeStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure USFailed
      "in_progress" -> pure USInProgress
      "succeeded" -> pure USSucceeded
      "succeeded_with_issues" -> pure USSucceededWithIssues
      e ->
        fromTextError $
          "Failure parsing UpgradeStatus from value: '" <> e
            <> "'. Accepted values: failed, in_progress, succeeded, succeeded_with_issues"

instance ToText UpgradeStatus where
  toText = \case
    USFailed -> "FAILED"
    USInProgress -> "IN_PROGRESS"
    USSucceeded -> "SUCCEEDED"
    USSucceededWithIssues -> "SUCCEEDED_WITH_ISSUES"

instance Hashable UpgradeStatus

instance NFData UpgradeStatus

instance ToByteString UpgradeStatus

instance ToQuery UpgradeStatus

instance ToHeader UpgradeStatus

instance FromJSON UpgradeStatus where
  parseJSON = parseJSONText "UpgradeStatus"
