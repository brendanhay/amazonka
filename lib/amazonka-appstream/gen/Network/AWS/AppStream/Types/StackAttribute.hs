{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StackAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StackAttribute where

import Network.AWS.Prelude

data StackAttribute
  = AccessEndpoints
  | EmbedHostDomains
  | FeedbackURL
  | IAMRoleARN
  | RedirectURL
  | StorageConnectorGoogleDrive
  | StorageConnectorHomefolders
  | StorageConnectorOneDrive
  | StorageConnectors
  | ThemeName
  | UserSettings
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

instance FromText StackAttribute where
  parser =
    takeLowerText >>= \case
      "access_endpoints" -> pure AccessEndpoints
      "embed_host_domains" -> pure EmbedHostDomains
      "feedback_url" -> pure FeedbackURL
      "iam_role_arn" -> pure IAMRoleARN
      "redirect_url" -> pure RedirectURL
      "storage_connector_google_drive" -> pure StorageConnectorGoogleDrive
      "storage_connector_homefolders" -> pure StorageConnectorHomefolders
      "storage_connector_one_drive" -> pure StorageConnectorOneDrive
      "storage_connectors" -> pure StorageConnectors
      "theme_name" -> pure ThemeName
      "user_settings" -> pure UserSettings
      e ->
        fromTextError $
          "Failure parsing StackAttribute from value: '" <> e
            <> "'. Accepted values: access_endpoints, embed_host_domains, feedback_url, iam_role_arn, redirect_url, storage_connector_google_drive, storage_connector_homefolders, storage_connector_one_drive, storage_connectors, theme_name, user_settings"

instance ToText StackAttribute where
  toText = \case
    AccessEndpoints -> "ACCESS_ENDPOINTS"
    EmbedHostDomains -> "EMBED_HOST_DOMAINS"
    FeedbackURL -> "FEEDBACK_URL"
    IAMRoleARN -> "IAM_ROLE_ARN"
    RedirectURL -> "REDIRECT_URL"
    StorageConnectorGoogleDrive -> "STORAGE_CONNECTOR_GOOGLE_DRIVE"
    StorageConnectorHomefolders -> "STORAGE_CONNECTOR_HOMEFOLDERS"
    StorageConnectorOneDrive -> "STORAGE_CONNECTOR_ONE_DRIVE"
    StorageConnectors -> "STORAGE_CONNECTORS"
    ThemeName -> "THEME_NAME"
    UserSettings -> "USER_SETTINGS"

instance Hashable StackAttribute

instance NFData StackAttribute

instance ToByteString StackAttribute

instance ToQuery StackAttribute

instance ToHeader StackAttribute

instance ToJSON StackAttribute where
  toJSON = toJSONText
