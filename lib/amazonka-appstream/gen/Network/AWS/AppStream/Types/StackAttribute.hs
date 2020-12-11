-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StackAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StackAttribute
  ( StackAttribute
      ( StackAttribute',
        AccessEndpoints,
        EmbedHostDomains,
        FeedbackURL,
        IAMRoleARN,
        RedirectURL,
        StorageConnectorGoogleDrive,
        StorageConnectorHomefolders,
        StorageConnectorOneDrive,
        StorageConnectors,
        ThemeName,
        UserSettings
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StackAttribute = StackAttribute' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AccessEndpoints :: StackAttribute
pattern AccessEndpoints = StackAttribute' "ACCESS_ENDPOINTS"

pattern EmbedHostDomains :: StackAttribute
pattern EmbedHostDomains = StackAttribute' "EMBED_HOST_DOMAINS"

pattern FeedbackURL :: StackAttribute
pattern FeedbackURL = StackAttribute' "FEEDBACK_URL"

pattern IAMRoleARN :: StackAttribute
pattern IAMRoleARN = StackAttribute' "IAM_ROLE_ARN"

pattern RedirectURL :: StackAttribute
pattern RedirectURL = StackAttribute' "REDIRECT_URL"

pattern StorageConnectorGoogleDrive :: StackAttribute
pattern StorageConnectorGoogleDrive = StackAttribute' "STORAGE_CONNECTOR_GOOGLE_DRIVE"

pattern StorageConnectorHomefolders :: StackAttribute
pattern StorageConnectorHomefolders = StackAttribute' "STORAGE_CONNECTOR_HOMEFOLDERS"

pattern StorageConnectorOneDrive :: StackAttribute
pattern StorageConnectorOneDrive = StackAttribute' "STORAGE_CONNECTOR_ONE_DRIVE"

pattern StorageConnectors :: StackAttribute
pattern StorageConnectors = StackAttribute' "STORAGE_CONNECTORS"

pattern ThemeName :: StackAttribute
pattern ThemeName = StackAttribute' "THEME_NAME"

pattern UserSettings :: StackAttribute
pattern UserSettings = StackAttribute' "USER_SETTINGS"

{-# COMPLETE
  AccessEndpoints,
  EmbedHostDomains,
  FeedbackURL,
  IAMRoleARN,
  RedirectURL,
  StorageConnectorGoogleDrive,
  StorageConnectorHomefolders,
  StorageConnectorOneDrive,
  StorageConnectors,
  ThemeName,
  UserSettings,
  StackAttribute'
  #-}
