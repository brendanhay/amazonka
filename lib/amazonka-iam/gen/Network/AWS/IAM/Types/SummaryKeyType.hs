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
        AccessKeysPerUserQuota,
        AccountAccessKeysPresent,
        AccountMFAEnabled,
        AccountSigningCertificatesPresent,
        AttachedPoliciesPerGroupQuota,
        AttachedPoliciesPerRoleQuota,
        AttachedPoliciesPerUserQuota,
        GlobalEndpointTokenVersion,
        GroupPolicySizeQuota,
        Groups,
        GroupsPerUserQuota,
        GroupsQuota,
        MFADevices,
        MFADevicesInUse,
        Policies,
        PoliciesQuota,
        PolicySizeQuota,
        PolicyVersionsInUse,
        PolicyVersionsInUseQuota,
        ServerCertificates,
        ServerCertificatesQuota,
        SigningCertificatesPerUserQuota,
        UserPolicySizeQuota,
        Users,
        UsersQuota,
        VersionsPerPolicyQuota
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

pattern AccessKeysPerUserQuota :: SummaryKeyType
pattern AccessKeysPerUserQuota = SummaryKeyType' "AccessKeysPerUserQuota"

pattern AccountAccessKeysPresent :: SummaryKeyType
pattern AccountAccessKeysPresent = SummaryKeyType' "AccountAccessKeysPresent"

pattern AccountMFAEnabled :: SummaryKeyType
pattern AccountMFAEnabled = SummaryKeyType' "AccountMFAEnabled"

pattern AccountSigningCertificatesPresent :: SummaryKeyType
pattern AccountSigningCertificatesPresent = SummaryKeyType' "AccountSigningCertificatesPresent"

pattern AttachedPoliciesPerGroupQuota :: SummaryKeyType
pattern AttachedPoliciesPerGroupQuota = SummaryKeyType' "AttachedPoliciesPerGroupQuota"

pattern AttachedPoliciesPerRoleQuota :: SummaryKeyType
pattern AttachedPoliciesPerRoleQuota = SummaryKeyType' "AttachedPoliciesPerRoleQuota"

pattern AttachedPoliciesPerUserQuota :: SummaryKeyType
pattern AttachedPoliciesPerUserQuota = SummaryKeyType' "AttachedPoliciesPerUserQuota"

pattern GlobalEndpointTokenVersion :: SummaryKeyType
pattern GlobalEndpointTokenVersion = SummaryKeyType' "GlobalEndpointTokenVersion"

pattern GroupPolicySizeQuota :: SummaryKeyType
pattern GroupPolicySizeQuota = SummaryKeyType' "GroupPolicySizeQuota"

pattern Groups :: SummaryKeyType
pattern Groups = SummaryKeyType' "Groups"

pattern GroupsPerUserQuota :: SummaryKeyType
pattern GroupsPerUserQuota = SummaryKeyType' "GroupsPerUserQuota"

pattern GroupsQuota :: SummaryKeyType
pattern GroupsQuota = SummaryKeyType' "GroupsQuota"

pattern MFADevices :: SummaryKeyType
pattern MFADevices = SummaryKeyType' "MFADevices"

pattern MFADevicesInUse :: SummaryKeyType
pattern MFADevicesInUse = SummaryKeyType' "MFADevicesInUse"

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

pattern ServerCertificates :: SummaryKeyType
pattern ServerCertificates = SummaryKeyType' "ServerCertificates"

pattern ServerCertificatesQuota :: SummaryKeyType
pattern ServerCertificatesQuota = SummaryKeyType' "ServerCertificatesQuota"

pattern SigningCertificatesPerUserQuota :: SummaryKeyType
pattern SigningCertificatesPerUserQuota = SummaryKeyType' "SigningCertificatesPerUserQuota"

pattern UserPolicySizeQuota :: SummaryKeyType
pattern UserPolicySizeQuota = SummaryKeyType' "UserPolicySizeQuota"

pattern Users :: SummaryKeyType
pattern Users = SummaryKeyType' "Users"

pattern UsersQuota :: SummaryKeyType
pattern UsersQuota = SummaryKeyType' "UsersQuota"

pattern VersionsPerPolicyQuota :: SummaryKeyType
pattern VersionsPerPolicyQuota = SummaryKeyType' "VersionsPerPolicyQuota"

{-# COMPLETE
  AccessKeysPerUserQuota,
  AccountAccessKeysPresent,
  AccountMFAEnabled,
  AccountSigningCertificatesPresent,
  AttachedPoliciesPerGroupQuota,
  AttachedPoliciesPerRoleQuota,
  AttachedPoliciesPerUserQuota,
  GlobalEndpointTokenVersion,
  GroupPolicySizeQuota,
  Groups,
  GroupsPerUserQuota,
  GroupsQuota,
  MFADevices,
  MFADevicesInUse,
  Policies,
  PoliciesQuota,
  PolicySizeQuota,
  PolicyVersionsInUse,
  PolicyVersionsInUseQuota,
  ServerCertificates,
  ServerCertificatesQuota,
  SigningCertificatesPerUserQuota,
  UserPolicySizeQuota,
  Users,
  UsersQuota,
  VersionsPerPolicyQuota,
  SummaryKeyType'
  #-}
