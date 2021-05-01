{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EKS.Types.UpdateParamType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.UpdateParamType
  ( UpdateParamType
      ( ..,
        UpdateParamType_AddonVersion,
        UpdateParamType_ClusterLogging,
        UpdateParamType_DesiredSize,
        UpdateParamType_EncryptionConfig,
        UpdateParamType_EndpointPrivateAccess,
        UpdateParamType_EndpointPublicAccess,
        UpdateParamType_IdentityProviderConfig,
        UpdateParamType_LabelsToAdd,
        UpdateParamType_LabelsToRemove,
        UpdateParamType_MaxSize,
        UpdateParamType_MinSize,
        UpdateParamType_PlatformVersion,
        UpdateParamType_PublicAccessCidrs,
        UpdateParamType_ReleaseVersion,
        UpdateParamType_ResolveConflicts,
        UpdateParamType_ServiceAccountRoleArn,
        UpdateParamType_Version
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype UpdateParamType = UpdateParamType'
  { fromUpdateParamType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern UpdateParamType_AddonVersion :: UpdateParamType
pattern UpdateParamType_AddonVersion = UpdateParamType' "AddonVersion"

pattern UpdateParamType_ClusterLogging :: UpdateParamType
pattern UpdateParamType_ClusterLogging = UpdateParamType' "ClusterLogging"

pattern UpdateParamType_DesiredSize :: UpdateParamType
pattern UpdateParamType_DesiredSize = UpdateParamType' "DesiredSize"

pattern UpdateParamType_EncryptionConfig :: UpdateParamType
pattern UpdateParamType_EncryptionConfig = UpdateParamType' "EncryptionConfig"

pattern UpdateParamType_EndpointPrivateAccess :: UpdateParamType
pattern UpdateParamType_EndpointPrivateAccess = UpdateParamType' "EndpointPrivateAccess"

pattern UpdateParamType_EndpointPublicAccess :: UpdateParamType
pattern UpdateParamType_EndpointPublicAccess = UpdateParamType' "EndpointPublicAccess"

pattern UpdateParamType_IdentityProviderConfig :: UpdateParamType
pattern UpdateParamType_IdentityProviderConfig = UpdateParamType' "IdentityProviderConfig"

pattern UpdateParamType_LabelsToAdd :: UpdateParamType
pattern UpdateParamType_LabelsToAdd = UpdateParamType' "LabelsToAdd"

pattern UpdateParamType_LabelsToRemove :: UpdateParamType
pattern UpdateParamType_LabelsToRemove = UpdateParamType' "LabelsToRemove"

pattern UpdateParamType_MaxSize :: UpdateParamType
pattern UpdateParamType_MaxSize = UpdateParamType' "MaxSize"

pattern UpdateParamType_MinSize :: UpdateParamType
pattern UpdateParamType_MinSize = UpdateParamType' "MinSize"

pattern UpdateParamType_PlatformVersion :: UpdateParamType
pattern UpdateParamType_PlatformVersion = UpdateParamType' "PlatformVersion"

pattern UpdateParamType_PublicAccessCidrs :: UpdateParamType
pattern UpdateParamType_PublicAccessCidrs = UpdateParamType' "PublicAccessCidrs"

pattern UpdateParamType_ReleaseVersion :: UpdateParamType
pattern UpdateParamType_ReleaseVersion = UpdateParamType' "ReleaseVersion"

pattern UpdateParamType_ResolveConflicts :: UpdateParamType
pattern UpdateParamType_ResolveConflicts = UpdateParamType' "ResolveConflicts"

pattern UpdateParamType_ServiceAccountRoleArn :: UpdateParamType
pattern UpdateParamType_ServiceAccountRoleArn = UpdateParamType' "ServiceAccountRoleArn"

pattern UpdateParamType_Version :: UpdateParamType
pattern UpdateParamType_Version = UpdateParamType' "Version"

{-# COMPLETE
  UpdateParamType_AddonVersion,
  UpdateParamType_ClusterLogging,
  UpdateParamType_DesiredSize,
  UpdateParamType_EncryptionConfig,
  UpdateParamType_EndpointPrivateAccess,
  UpdateParamType_EndpointPublicAccess,
  UpdateParamType_IdentityProviderConfig,
  UpdateParamType_LabelsToAdd,
  UpdateParamType_LabelsToRemove,
  UpdateParamType_MaxSize,
  UpdateParamType_MinSize,
  UpdateParamType_PlatformVersion,
  UpdateParamType_PublicAccessCidrs,
  UpdateParamType_ReleaseVersion,
  UpdateParamType_ResolveConflicts,
  UpdateParamType_ServiceAccountRoleArn,
  UpdateParamType_Version,
  UpdateParamType'
  #-}
