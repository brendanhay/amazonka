{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EKS.Types.UpdateParamType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.UpdateParamType
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
        UpdateParamType_LaunchTemplateName,
        UpdateParamType_LaunchTemplateVersion,
        UpdateParamType_MaxSize,
        UpdateParamType_MaxUnavailable,
        UpdateParamType_MaxUnavailablePercentage,
        UpdateParamType_MinSize,
        UpdateParamType_PlatformVersion,
        UpdateParamType_PublicAccessCidrs,
        UpdateParamType_ReleaseVersion,
        UpdateParamType_ResolveConflicts,
        UpdateParamType_ServiceAccountRoleArn,
        UpdateParamType_TaintsToAdd,
        UpdateParamType_TaintsToRemove,
        UpdateParamType_Version
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UpdateParamType = UpdateParamType'
  { fromUpdateParamType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

pattern UpdateParamType_LaunchTemplateName :: UpdateParamType
pattern UpdateParamType_LaunchTemplateName = UpdateParamType' "LaunchTemplateName"

pattern UpdateParamType_LaunchTemplateVersion :: UpdateParamType
pattern UpdateParamType_LaunchTemplateVersion = UpdateParamType' "LaunchTemplateVersion"

pattern UpdateParamType_MaxSize :: UpdateParamType
pattern UpdateParamType_MaxSize = UpdateParamType' "MaxSize"

pattern UpdateParamType_MaxUnavailable :: UpdateParamType
pattern UpdateParamType_MaxUnavailable = UpdateParamType' "MaxUnavailable"

pattern UpdateParamType_MaxUnavailablePercentage :: UpdateParamType
pattern UpdateParamType_MaxUnavailablePercentage = UpdateParamType' "MaxUnavailablePercentage"

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

pattern UpdateParamType_TaintsToAdd :: UpdateParamType
pattern UpdateParamType_TaintsToAdd = UpdateParamType' "TaintsToAdd"

pattern UpdateParamType_TaintsToRemove :: UpdateParamType
pattern UpdateParamType_TaintsToRemove = UpdateParamType' "TaintsToRemove"

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
  UpdateParamType_LaunchTemplateName,
  UpdateParamType_LaunchTemplateVersion,
  UpdateParamType_MaxSize,
  UpdateParamType_MaxUnavailable,
  UpdateParamType_MaxUnavailablePercentage,
  UpdateParamType_MinSize,
  UpdateParamType_PlatformVersion,
  UpdateParamType_PublicAccessCidrs,
  UpdateParamType_ReleaseVersion,
  UpdateParamType_ResolveConflicts,
  UpdateParamType_ServiceAccountRoleArn,
  UpdateParamType_TaintsToAdd,
  UpdateParamType_TaintsToRemove,
  UpdateParamType_Version,
  UpdateParamType'
  #-}
