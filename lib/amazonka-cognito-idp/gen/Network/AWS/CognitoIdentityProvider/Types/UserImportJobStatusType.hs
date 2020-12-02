{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType where

import Network.AWS.Prelude

data UserImportJobStatusType
  = Created
  | Expired
  | Failed
  | InProgress
  | Pending
  | Stopped
  | Stopping
  | Succeeded
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

instance FromText UserImportJobStatusType where
  parser =
    takeLowerText >>= \case
      "created" -> pure Created
      "expired" -> pure Expired
      "failed" -> pure Failed
      "inprogress" -> pure InProgress
      "pending" -> pure Pending
      "stopped" -> pure Stopped
      "stopping" -> pure Stopping
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing UserImportJobStatusType from value: '" <> e
            <> "'. Accepted values: created, expired, failed, inprogress, pending, stopped, stopping, succeeded"

instance ToText UserImportJobStatusType where
  toText = \case
    Created -> "Created"
    Expired -> "Expired"
    Failed -> "Failed"
    InProgress -> "InProgress"
    Pending -> "Pending"
    Stopped -> "Stopped"
    Stopping -> "Stopping"
    Succeeded -> "Succeeded"

instance Hashable UserImportJobStatusType

instance NFData UserImportJobStatusType

instance ToByteString UserImportJobStatusType

instance ToQuery UserImportJobStatusType

instance ToHeader UserImportJobStatusType

instance FromJSON UserImportJobStatusType where
  parseJSON = parseJSONText "UserImportJobStatusType"
