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
        Users,
        UsersQuota,
        Groups,
        GroupsQuota,
        ServerCertificates,
        ServerCertificatesQuota,
        UserPolicySizeQuota,
        GroupPolicySizeQuota,
        GroupsPerUserQuota,
        SigningCertificatesPerUserQuota,
        AccessKeysPerUserQuota,
        MFADevices,
        MFADevicesInUse,
        AccountMFAEnabled,
        AccountAccessKeysPresent,
        AccountSigningCertificatesPresent,
        AttachedPoliciesPerGroupQuota,
        AttachedPoliciesPerRoleQuota,
        AttachedPoliciesPerUserQuota,
        Policies,
        PoliciesQuota,
        PolicySizeQuota,
        PolicyVersionsInUse,
        PolicyVersionsInUseQuota,
        VersionsPerPolicyQuota,
        GlobalEndpointTokenVersion
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SummaryKeyType = SummaryKeyType' Lude.Text
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

pattern Users :: SummaryKeyType
pattern Users = SummaryKeyType' "Users"

pattern UsersQuota :: SummaryKeyType
pattern UsersQuota = SummaryKeyType' "UsersQuota"

pattern Groups :: SummaryKeyType
pattern Groups = SummaryKeyType' "Groups"

pattern GroupsQuota :: SummaryKeyType
pattern GroupsQuota = SummaryKeyType' "GroupsQuota"

pattern ServerCertificates :: SummaryKeyType
pattern ServerCertificates = SummaryKeyType' "ServerCertificates"

pattern ServerCertificatesQuota :: SummaryKeyType
pattern ServerCertificatesQuota = SummaryKeyType' "ServerCertificatesQuota"

pattern UserPolicySizeQuota :: SummaryKeyType
pattern UserPolicySizeQuota = SummaryKeyType' "UserPolicySizeQuota"

pattern GroupPolicySizeQuota :: SummaryKeyType
pattern GroupPolicySizeQuota = SummaryKeyType' "GroupPolicySizeQuota"

pattern GroupsPerUserQuota :: SummaryKeyType
pattern GroupsPerUserQuota = SummaryKeyType' "GroupsPerUserQuota"

pattern SigningCertificatesPerUserQuota :: SummaryKeyType
pattern SigningCertificatesPerUserQuota = SummaryKeyType' "SigningCertificatesPerUserQuota"

pattern AccessKeysPerUserQuota :: SummaryKeyType
pattern AccessKeysPerUserQuota = SummaryKeyType' "AccessKeysPerUserQuota"

pattern MFADevices :: SummaryKeyType
pattern MFADevices = SummaryKeyType' "MFADevices"

pattern MFADevicesInUse :: SummaryKeyType
pattern MFADevicesInUse = SummaryKeyType' "MFADevicesInUse"

pattern AccountMFAEnabled :: SummaryKeyType
pattern AccountMFAEnabled = SummaryKeyType' "AccountMFAEnabled"

pattern AccountAccessKeysPresent :: SummaryKeyType
pattern AccountAccessKeysPresent = SummaryKeyType' "AccountAccessKeysPresent"

pattern AccountSigningCertificatesPresent :: SummaryKeyType
pattern AccountSigningCertificatesPresent = SummaryKeyType' "AccountSigningCertificatesPresent"

pattern AttachedPoliciesPerGroupQuota :: SummaryKeyType
pattern AttachedPoliciesPerGroupQuota = SummaryKeyType' "AttachedPoliciesPerGroupQuota"

pattern AttachedPoliciesPerRoleQuota :: SummaryKeyType
pattern AttachedPoliciesPerRoleQuota = SummaryKeyType' "AttachedPoliciesPerRoleQuota"

pattern AttachedPoliciesPerUserQuota :: SummaryKeyType
pattern AttachedPoliciesPerUserQuota = SummaryKeyType' "AttachedPoliciesPerUserQuota"

pattern Policies :: SummaryKeyType
pattern Policies = SummaryKeyType' "Policies"

pattern PoliciesQuota :: SummaryKeyType
pattern PoliciesQuota = SummaryKeyType' "PoliciesQuota"

pattern PolicySizeQuota :: SummaryKeyType
pattern PolicySizeQuota = SummaryKeyType' "PolicySizeQuota"

pattern PolicyVersionsInUse :: SummaryKeyType
pattern PolicyVersionsInUse = SummaryKeyType' "PolicyVersionsInUse"

pattern PolicyVersionsInUseQuota :: SummaryKeyType
pattern PolicyVersionsInUseQuota = SummaryKeyType' "PolicyVersionsInUseQuota"

pattern VersionsPerPolicyQuota :: SummaryKeyType
pattern VersionsPerPolicyQuota = SummaryKeyType' "VersionsPerPolicyQuota"

pattern GlobalEndpointTokenVersion :: SummaryKeyType
pattern GlobalEndpointTokenVersion = SummaryKeyType' "GlobalEndpointTokenVersion"

{-# COMPLETE
  Users,
  UsersQuota,
  Groups,
  GroupsQuota,
  ServerCertificates,
  ServerCertificatesQuota,
  UserPolicySizeQuota,
  GroupPolicySizeQuota,
  GroupsPerUserQuota,
  SigningCertificatesPerUserQuota,
  AccessKeysPerUserQuota,
  MFADevices,
  MFADevicesInUse,
  AccountMFAEnabled,
  AccountAccessKeysPresent,
  AccountSigningCertificatesPresent,
  AttachedPoliciesPerGroupQuota,
  AttachedPoliciesPerRoleQuota,
  AttachedPoliciesPerUserQuota,
  Policies,
  PoliciesQuota,
  PolicySizeQuota,
  PolicyVersionsInUse,
  PolicyVersionsInUseQuota,
  VersionsPerPolicyQuota,
  GlobalEndpointTokenVersion,
  SummaryKeyType'
  #-}
