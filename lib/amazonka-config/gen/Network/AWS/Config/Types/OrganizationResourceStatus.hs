{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationResourceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationResourceStatus where

import Network.AWS.Prelude

data OrganizationResourceStatus
  = ORSCreateFailed
  | ORSCreateInProgress
  | ORSCreateSuccessful
  | ORSDeleteFailed
  | ORSDeleteInProgress
  | ORSDeleteSuccessful
  | ORSUpdateFailed
  | ORSUpdateInProgress
  | ORSUpdateSuccessful
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

instance FromText OrganizationResourceStatus where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure ORSCreateFailed
      "create_in_progress" -> pure ORSCreateInProgress
      "create_successful" -> pure ORSCreateSuccessful
      "delete_failed" -> pure ORSDeleteFailed
      "delete_in_progress" -> pure ORSDeleteInProgress
      "delete_successful" -> pure ORSDeleteSuccessful
      "update_failed" -> pure ORSUpdateFailed
      "update_in_progress" -> pure ORSUpdateInProgress
      "update_successful" -> pure ORSUpdateSuccessful
      e ->
        fromTextError $
          "Failure parsing OrganizationResourceStatus from value: '" <> e
            <> "'. Accepted values: create_failed, create_in_progress, create_successful, delete_failed, delete_in_progress, delete_successful, update_failed, update_in_progress, update_successful"

instance ToText OrganizationResourceStatus where
  toText = \case
    ORSCreateFailed -> "CREATE_FAILED"
    ORSCreateInProgress -> "CREATE_IN_PROGRESS"
    ORSCreateSuccessful -> "CREATE_SUCCESSFUL"
    ORSDeleteFailed -> "DELETE_FAILED"
    ORSDeleteInProgress -> "DELETE_IN_PROGRESS"
    ORSDeleteSuccessful -> "DELETE_SUCCESSFUL"
    ORSUpdateFailed -> "UPDATE_FAILED"
    ORSUpdateInProgress -> "UPDATE_IN_PROGRESS"
    ORSUpdateSuccessful -> "UPDATE_SUCCESSFUL"

instance Hashable OrganizationResourceStatus

instance NFData OrganizationResourceStatus

instance ToByteString OrganizationResourceStatus

instance ToQuery OrganizationResourceStatus

instance ToHeader OrganizationResourceStatus

instance FromJSON OrganizationResourceStatus where
  parseJSON = parseJSONText "OrganizationResourceStatus"
