{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        StackAttributeStorageConnectors,
        StackAttributeStorageConnectorHomefolders,
        StackAttributeStorageConnectorGoogleDrive,
        StackAttributeStorageConnectorOneDrive,
        StackAttributeRedirectUrl,
        StackAttributeFeedbackUrl,
        StackAttributeThemeName,
        StackAttributeUserSettings,
        StackAttributeEmbedHostDomains,
        StackAttributeIamRoleArn,
        StackAttributeAccessEndpoints,
        fromStackAttribute
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype StackAttribute = StackAttribute'
  { fromStackAttribute ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern StackAttributeStorageConnectors :: StackAttribute
pattern StackAttributeStorageConnectors = StackAttribute' "STORAGE_CONNECTORS"

pattern StackAttributeStorageConnectorHomefolders :: StackAttribute
pattern StackAttributeStorageConnectorHomefolders = StackAttribute' "STORAGE_CONNECTOR_HOMEFOLDERS"

pattern StackAttributeStorageConnectorGoogleDrive :: StackAttribute
pattern StackAttributeStorageConnectorGoogleDrive = StackAttribute' "STORAGE_CONNECTOR_GOOGLE_DRIVE"

pattern StackAttributeStorageConnectorOneDrive :: StackAttribute
pattern StackAttributeStorageConnectorOneDrive = StackAttribute' "STORAGE_CONNECTOR_ONE_DRIVE"

pattern StackAttributeRedirectUrl :: StackAttribute
pattern StackAttributeRedirectUrl = StackAttribute' "REDIRECT_URL"

pattern StackAttributeFeedbackUrl :: StackAttribute
pattern StackAttributeFeedbackUrl = StackAttribute' "FEEDBACK_URL"

pattern StackAttributeThemeName :: StackAttribute
pattern StackAttributeThemeName = StackAttribute' "THEME_NAME"

pattern StackAttributeUserSettings :: StackAttribute
pattern StackAttributeUserSettings = StackAttribute' "USER_SETTINGS"

pattern StackAttributeEmbedHostDomains :: StackAttribute
pattern StackAttributeEmbedHostDomains = StackAttribute' "EMBED_HOST_DOMAINS"

pattern StackAttributeIamRoleArn :: StackAttribute
pattern StackAttributeIamRoleArn = StackAttribute' "IAM_ROLE_ARN"

pattern StackAttributeAccessEndpoints :: StackAttribute
pattern StackAttributeAccessEndpoints = StackAttribute' "ACCESS_ENDPOINTS"

{-# COMPLETE
  StackAttributeStorageConnectors,
  StackAttributeStorageConnectorHomefolders,
  StackAttributeStorageConnectorGoogleDrive,
  StackAttributeStorageConnectorOneDrive,
  StackAttributeRedirectUrl,
  StackAttributeFeedbackUrl,
  StackAttributeThemeName,
  StackAttributeUserSettings,
  StackAttributeEmbedHostDomains,
  StackAttributeIamRoleArn,
  StackAttributeAccessEndpoints,
  StackAttribute'
  #-}
