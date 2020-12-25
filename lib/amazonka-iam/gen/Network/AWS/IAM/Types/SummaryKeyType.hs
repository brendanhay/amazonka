{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SummaryKeyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SummaryKeyType
  ( SummaryKeyType
      ( SummaryKeyType',
        SummaryKeyTypeUsers,
        SummaryKeyTypeUsersQuota,
        SummaryKeyTypeGroups,
        SummaryKeyTypeGroupsQuota,
        SummaryKeyTypeServerCertificates,
        SummaryKeyTypeServerCertificatesQuota,
        SummaryKeyTypeUserPolicySizeQuota,
        SummaryKeyTypeGroupPolicySizeQuota,
        SummaryKeyTypeGroupsPerUserQuota,
        SummaryKeyTypeSigningCertificatesPerUserQuota,
        SummaryKeyTypeAccessKeysPerUserQuota,
        SummaryKeyTypeMFADevices,
        SummaryKeyTypeMFADevicesInUse,
        SummaryKeyTypeAccountMFAEnabled,
        SummaryKeyTypeAccountAccessKeysPresent,
        SummaryKeyTypeAccountSigningCertificatesPresent,
        SummaryKeyTypeAttachedPoliciesPerGroupQuota,
        SummaryKeyTypeAttachedPoliciesPerRoleQuota,
        SummaryKeyTypeAttachedPoliciesPerUserQuota,
        SummaryKeyTypePolicies,
        SummaryKeyTypePoliciesQuota,
        SummaryKeyTypePolicySizeQuota,
        SummaryKeyTypePolicyVersionsInUse,
        SummaryKeyTypePolicyVersionsInUseQuota,
        SummaryKeyTypeVersionsPerPolicyQuota,
        SummaryKeyTypeGlobalEndpointTokenVersion,
        fromSummaryKeyType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SummaryKeyType = SummaryKeyType'
  { fromSummaryKeyType ::
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

pattern SummaryKeyTypeUsers :: SummaryKeyType
pattern SummaryKeyTypeUsers = SummaryKeyType' "Users"

pattern SummaryKeyTypeUsersQuota :: SummaryKeyType
pattern SummaryKeyTypeUsersQuota = SummaryKeyType' "UsersQuota"

pattern SummaryKeyTypeGroups :: SummaryKeyType
pattern SummaryKeyTypeGroups = SummaryKeyType' "Groups"

pattern SummaryKeyTypeGroupsQuota :: SummaryKeyType
pattern SummaryKeyTypeGroupsQuota = SummaryKeyType' "GroupsQuota"

pattern SummaryKeyTypeServerCertificates :: SummaryKeyType
pattern SummaryKeyTypeServerCertificates = SummaryKeyType' "ServerCertificates"

pattern SummaryKeyTypeServerCertificatesQuota :: SummaryKeyType
pattern SummaryKeyTypeServerCertificatesQuota = SummaryKeyType' "ServerCertificatesQuota"

pattern SummaryKeyTypeUserPolicySizeQuota :: SummaryKeyType
pattern SummaryKeyTypeUserPolicySizeQuota = SummaryKeyType' "UserPolicySizeQuota"

pattern SummaryKeyTypeGroupPolicySizeQuota :: SummaryKeyType
pattern SummaryKeyTypeGroupPolicySizeQuota = SummaryKeyType' "GroupPolicySizeQuota"

pattern SummaryKeyTypeGroupsPerUserQuota :: SummaryKeyType
pattern SummaryKeyTypeGroupsPerUserQuota = SummaryKeyType' "GroupsPerUserQuota"

pattern SummaryKeyTypeSigningCertificatesPerUserQuota :: SummaryKeyType
pattern SummaryKeyTypeSigningCertificatesPerUserQuota = SummaryKeyType' "SigningCertificatesPerUserQuota"

pattern SummaryKeyTypeAccessKeysPerUserQuota :: SummaryKeyType
pattern SummaryKeyTypeAccessKeysPerUserQuota = SummaryKeyType' "AccessKeysPerUserQuota"

pattern SummaryKeyTypeMFADevices :: SummaryKeyType
pattern SummaryKeyTypeMFADevices = SummaryKeyType' "MFADevices"

pattern SummaryKeyTypeMFADevicesInUse :: SummaryKeyType
pattern SummaryKeyTypeMFADevicesInUse = SummaryKeyType' "MFADevicesInUse"

pattern SummaryKeyTypeAccountMFAEnabled :: SummaryKeyType
pattern SummaryKeyTypeAccountMFAEnabled = SummaryKeyType' "AccountMFAEnabled"

pattern SummaryKeyTypeAccountAccessKeysPresent :: SummaryKeyType
pattern SummaryKeyTypeAccountAccessKeysPresent = SummaryKeyType' "AccountAccessKeysPresent"

pattern SummaryKeyTypeAccountSigningCertificatesPresent :: SummaryKeyType
pattern SummaryKeyTypeAccountSigningCertificatesPresent = SummaryKeyType' "AccountSigningCertificatesPresent"

pattern SummaryKeyTypeAttachedPoliciesPerGroupQuota :: SummaryKeyType
pattern SummaryKeyTypeAttachedPoliciesPerGroupQuota = SummaryKeyType' "AttachedPoliciesPerGroupQuota"

pattern SummaryKeyTypeAttachedPoliciesPerRoleQuota :: SummaryKeyType
pattern SummaryKeyTypeAttachedPoliciesPerRoleQuota = SummaryKeyType' "AttachedPoliciesPerRoleQuota"

pattern SummaryKeyTypeAttachedPoliciesPerUserQuota :: SummaryKeyType
pattern SummaryKeyTypeAttachedPoliciesPerUserQuota = SummaryKeyType' "AttachedPoliciesPerUserQuota"

pattern SummaryKeyTypePolicies :: SummaryKeyType
pattern SummaryKeyTypePolicies = SummaryKeyType' "Policies"

pattern SummaryKeyTypePoliciesQuota :: SummaryKeyType
pattern SummaryKeyTypePoliciesQuota = SummaryKeyType' "PoliciesQuota"

pattern SummaryKeyTypePolicySizeQuota :: SummaryKeyType
pattern SummaryKeyTypePolicySizeQuota = SummaryKeyType' "PolicySizeQuota"

pattern SummaryKeyTypePolicyVersionsInUse :: SummaryKeyType
pattern SummaryKeyTypePolicyVersionsInUse = SummaryKeyType' "PolicyVersionsInUse"

pattern SummaryKeyTypePolicyVersionsInUseQuota :: SummaryKeyType
pattern SummaryKeyTypePolicyVersionsInUseQuota = SummaryKeyType' "PolicyVersionsInUseQuota"

pattern SummaryKeyTypeVersionsPerPolicyQuota :: SummaryKeyType
pattern SummaryKeyTypeVersionsPerPolicyQuota = SummaryKeyType' "VersionsPerPolicyQuota"

pattern SummaryKeyTypeGlobalEndpointTokenVersion :: SummaryKeyType
pattern SummaryKeyTypeGlobalEndpointTokenVersion = SummaryKeyType' "GlobalEndpointTokenVersion"

{-# COMPLETE
  SummaryKeyTypeUsers,
  SummaryKeyTypeUsersQuota,
  SummaryKeyTypeGroups,
  SummaryKeyTypeGroupsQuota,
  SummaryKeyTypeServerCertificates,
  SummaryKeyTypeServerCertificatesQuota,
  SummaryKeyTypeUserPolicySizeQuota,
  SummaryKeyTypeGroupPolicySizeQuota,
  SummaryKeyTypeGroupsPerUserQuota,
  SummaryKeyTypeSigningCertificatesPerUserQuota,
  SummaryKeyTypeAccessKeysPerUserQuota,
  SummaryKeyTypeMFADevices,
  SummaryKeyTypeMFADevicesInUse,
  SummaryKeyTypeAccountMFAEnabled,
  SummaryKeyTypeAccountAccessKeysPresent,
  SummaryKeyTypeAccountSigningCertificatesPresent,
  SummaryKeyTypeAttachedPoliciesPerGroupQuota,
  SummaryKeyTypeAttachedPoliciesPerRoleQuota,
  SummaryKeyTypeAttachedPoliciesPerUserQuota,
  SummaryKeyTypePolicies,
  SummaryKeyTypePoliciesQuota,
  SummaryKeyTypePolicySizeQuota,
  SummaryKeyTypePolicyVersionsInUse,
  SummaryKeyTypePolicyVersionsInUseQuota,
  SummaryKeyTypeVersionsPerPolicyQuota,
  SummaryKeyTypeGlobalEndpointTokenVersion,
  SummaryKeyType'
  #-}
