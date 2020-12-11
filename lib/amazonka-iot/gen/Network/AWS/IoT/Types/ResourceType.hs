-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ResourceType
  ( ResourceType
      ( ResourceType',
        RTAccountSettings,
        RTCaCertificate,
        RTClientId,
        RTCognitoIdentityPool,
        RTDeviceCertificate,
        RTIAMRole,
        RTIotPolicy,
        RTRoleAlias
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ResourceType = ResourceType' Lude.Text
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

pattern RTAccountSettings :: ResourceType
pattern RTAccountSettings = ResourceType' "ACCOUNT_SETTINGS"

pattern RTCaCertificate :: ResourceType
pattern RTCaCertificate = ResourceType' "CA_CERTIFICATE"

pattern RTClientId :: ResourceType
pattern RTClientId = ResourceType' "CLIENT_ID"

pattern RTCognitoIdentityPool :: ResourceType
pattern RTCognitoIdentityPool = ResourceType' "COGNITO_IDENTITY_POOL"

pattern RTDeviceCertificate :: ResourceType
pattern RTDeviceCertificate = ResourceType' "DEVICE_CERTIFICATE"

pattern RTIAMRole :: ResourceType
pattern RTIAMRole = ResourceType' "IAM_ROLE"

pattern RTIotPolicy :: ResourceType
pattern RTIotPolicy = ResourceType' "IOT_POLICY"

pattern RTRoleAlias :: ResourceType
pattern RTRoleAlias = ResourceType' "ROLE_ALIAS"

{-# COMPLETE
  RTAccountSettings,
  RTCaCertificate,
  RTClientId,
  RTCognitoIdentityPool,
  RTDeviceCertificate,
  RTIAMRole,
  RTIotPolicy,
  RTRoleAlias,
  ResourceType'
  #-}
