{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageStatus where

import Network.AWS.Prelude

data ImageStatus
  = ISCreateFailed
  | ISCreated
  | ISCreating
  | ISDeleteFailed
  | ISDeleting
  | ISUpdateFailed
  | ISUpdating
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

instance FromText ImageStatus where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure ISCreateFailed
      "created" -> pure ISCreated
      "creating" -> pure ISCreating
      "delete_failed" -> pure ISDeleteFailed
      "deleting" -> pure ISDeleting
      "update_failed" -> pure ISUpdateFailed
      "updating" -> pure ISUpdating
      e ->
        fromTextError $
          "Failure parsing ImageStatus from value: '" <> e
            <> "'. Accepted values: create_failed, created, creating, delete_failed, deleting, update_failed, updating"

instance ToText ImageStatus where
  toText = \case
    ISCreateFailed -> "CREATE_FAILED"
    ISCreated -> "CREATED"
    ISCreating -> "CREATING"
    ISDeleteFailed -> "DELETE_FAILED"
    ISDeleting -> "DELETING"
    ISUpdateFailed -> "UPDATE_FAILED"
    ISUpdating -> "UPDATING"

instance Hashable ImageStatus

instance NFData ImageStatus

instance ToByteString ImageStatus

instance ToQuery ImageStatus

instance ToHeader ImageStatus

instance FromJSON ImageStatus where
  parseJSON = parseJSONText "ImageStatus"
