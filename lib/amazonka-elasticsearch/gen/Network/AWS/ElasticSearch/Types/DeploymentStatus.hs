{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DeploymentStatus where

import Network.AWS.Prelude

data DeploymentStatus
  = Completed
  | Eligible
  | InProgress
  | NotEligible
  | PendingUpdate
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

instance FromText DeploymentStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure Completed
      "eligible" -> pure Eligible
      "in_progress" -> pure InProgress
      "not_eligible" -> pure NotEligible
      "pending_update" -> pure PendingUpdate
      e ->
        fromTextError $
          "Failure parsing DeploymentStatus from value: '" <> e
            <> "'. Accepted values: completed, eligible, in_progress, not_eligible, pending_update"

instance ToText DeploymentStatus where
  toText = \case
    Completed -> "COMPLETED"
    Eligible -> "ELIGIBLE"
    InProgress -> "IN_PROGRESS"
    NotEligible -> "NOT_ELIGIBLE"
    PendingUpdate -> "PENDING_UPDATE"

instance Hashable DeploymentStatus

instance NFData DeploymentStatus

instance ToByteString DeploymentStatus

instance ToQuery DeploymentStatus

instance ToHeader DeploymentStatus

instance FromJSON DeploymentStatus where
  parseJSON = parseJSONText "DeploymentStatus"
