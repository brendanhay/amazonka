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
        SAStorageConnectors,
        SAStorageConnectorHomefolders,
        SAStorageConnectorGoogleDrive,
        SAStorageConnectorOneDrive,
        SARedirectURL,
        SAFeedbackURL,
        SAThemeName,
        SAUserSettings,
        SAEmbedHostDomains,
        SAIAMRoleARN,
        SAAccessEndpoints
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

pattern SAStorageConnectors :: StackAttribute
pattern SAStorageConnectors = StackAttribute' "STORAGE_CONNECTORS"

pattern SAStorageConnectorHomefolders :: StackAttribute
pattern SAStorageConnectorHomefolders = StackAttribute' "STORAGE_CONNECTOR_HOMEFOLDERS"

pattern SAStorageConnectorGoogleDrive :: StackAttribute
pattern SAStorageConnectorGoogleDrive = StackAttribute' "STORAGE_CONNECTOR_GOOGLE_DRIVE"

pattern SAStorageConnectorOneDrive :: StackAttribute
pattern SAStorageConnectorOneDrive = StackAttribute' "STORAGE_CONNECTOR_ONE_DRIVE"

pattern SARedirectURL :: StackAttribute
pattern SARedirectURL = StackAttribute' "REDIRECT_URL"

pattern SAFeedbackURL :: StackAttribute
pattern SAFeedbackURL = StackAttribute' "FEEDBACK_URL"

pattern SAThemeName :: StackAttribute
pattern SAThemeName = StackAttribute' "THEME_NAME"

pattern SAUserSettings :: StackAttribute
pattern SAUserSettings = StackAttribute' "USER_SETTINGS"

pattern SAEmbedHostDomains :: StackAttribute
pattern SAEmbedHostDomains = StackAttribute' "EMBED_HOST_DOMAINS"

pattern SAIAMRoleARN :: StackAttribute
pattern SAIAMRoleARN = StackAttribute' "IAM_ROLE_ARN"

pattern SAAccessEndpoints :: StackAttribute
pattern SAAccessEndpoints = StackAttribute' "ACCESS_ENDPOINTS"

{-# COMPLETE
  SAStorageConnectors,
  SAStorageConnectorHomefolders,
  SAStorageConnectorGoogleDrive,
  SAStorageConnectorOneDrive,
  SARedirectURL,
  SAFeedbackURL,
  SAThemeName,
  SAUserSettings,
  SAEmbedHostDomains,
  SAIAMRoleARN,
  SAAccessEndpoints,
  StackAttribute'
  #-}
