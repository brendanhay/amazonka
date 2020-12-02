{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DomainStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DomainStatus where

import Network.AWS.Prelude

data DomainStatus
  = DeleteFailed
  | Deleting
  | Failed
  | InService
  | Pending
  | UpdateFailed
  | Updating
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

instance FromText DomainStatus where
  parser =
    takeLowerText >>= \case
      "delete_failed" -> pure DeleteFailed
      "deleting" -> pure Deleting
      "failed" -> pure Failed
      "inservice" -> pure InService
      "pending" -> pure Pending
      "update_failed" -> pure UpdateFailed
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing DomainStatus from value: '" <> e
            <> "'. Accepted values: delete_failed, deleting, failed, inservice, pending, update_failed, updating"

instance ToText DomainStatus where
  toText = \case
    DeleteFailed -> "Delete_Failed"
    Deleting -> "Deleting"
    Failed -> "Failed"
    InService -> "InService"
    Pending -> "Pending"
    UpdateFailed -> "Update_Failed"
    Updating -> "Updating"

instance Hashable DomainStatus

instance NFData DomainStatus

instance ToByteString DomainStatus

instance ToQuery DomainStatus

instance ToHeader DomainStatus

instance FromJSON DomainStatus where
  parseJSON = parseJSONText "DomainStatus"
