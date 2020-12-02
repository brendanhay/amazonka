{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserProfileStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserProfileStatus where

import Network.AWS.Prelude

data UserProfileStatus
  = UPSDeleteFailed
  | UPSDeleting
  | UPSFailed
  | UPSInService
  | UPSPending
  | UPSUpdateFailed
  | UPSUpdating
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

instance FromText UserProfileStatus where
  parser =
    takeLowerText >>= \case
      "delete_failed" -> pure UPSDeleteFailed
      "deleting" -> pure UPSDeleting
      "failed" -> pure UPSFailed
      "inservice" -> pure UPSInService
      "pending" -> pure UPSPending
      "update_failed" -> pure UPSUpdateFailed
      "updating" -> pure UPSUpdating
      e ->
        fromTextError $
          "Failure parsing UserProfileStatus from value: '" <> e
            <> "'. Accepted values: delete_failed, deleting, failed, inservice, pending, update_failed, updating"

instance ToText UserProfileStatus where
  toText = \case
    UPSDeleteFailed -> "Delete_Failed"
    UPSDeleting -> "Deleting"
    UPSFailed -> "Failed"
    UPSInService -> "InService"
    UPSPending -> "Pending"
    UPSUpdateFailed -> "Update_Failed"
    UPSUpdating -> "Updating"

instance Hashable UserProfileStatus

instance NFData UserProfileStatus

instance ToByteString UserProfileStatus

instance ToQuery UserProfileStatus

instance ToHeader UserProfileStatus

instance FromJSON UserProfileStatus where
  parseJSON = parseJSONText "UserProfileStatus"
