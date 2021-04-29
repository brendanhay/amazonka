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
-- Module      : Network.AWS.IAM.Types.SummaryKeyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SummaryKeyType
  ( SummaryKeyType
      ( ..,
        SummaryKeyType_AccessKeysPerUserQuota,
        SummaryKeyType_AccountAccessKeysPresent,
        SummaryKeyType_AccountMFAEnabled,
        SummaryKeyType_AccountSigningCertificatesPresent,
        SummaryKeyType_AttachedPoliciesPerGroupQuota,
        SummaryKeyType_AttachedPoliciesPerRoleQuota,
        SummaryKeyType_AttachedPoliciesPerUserQuota,
        SummaryKeyType_GlobalEndpointTokenVersion,
        SummaryKeyType_GroupPolicySizeQuota,
        SummaryKeyType_Groups,
        SummaryKeyType_GroupsPerUserQuota,
        SummaryKeyType_GroupsQuota,
        SummaryKeyType_MFADevices,
        SummaryKeyType_MFADevicesInUse,
        SummaryKeyType_Policies,
        SummaryKeyType_PoliciesQuota,
        SummaryKeyType_PolicySizeQuota,
        SummaryKeyType_PolicyVersionsInUse,
        SummaryKeyType_PolicyVersionsInUseQuota,
        SummaryKeyType_ServerCertificates,
        SummaryKeyType_ServerCertificatesQuota,
        SummaryKeyType_SigningCertificatesPerUserQuota,
        SummaryKeyType_UserPolicySizeQuota,
        SummaryKeyType_Users,
        SummaryKeyType_UsersQuota,
        SummaryKeyType_VersionsPerPolicyQuota
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SummaryKeyType = SummaryKeyType'
  { fromSummaryKeyType ::
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

pattern SummaryKeyType_AccessKeysPerUserQuota :: SummaryKeyType
pattern SummaryKeyType_AccessKeysPerUserQuota = SummaryKeyType' "AccessKeysPerUserQuota"

pattern SummaryKeyType_AccountAccessKeysPresent :: SummaryKeyType
pattern SummaryKeyType_AccountAccessKeysPresent = SummaryKeyType' "AccountAccessKeysPresent"

pattern SummaryKeyType_AccountMFAEnabled :: SummaryKeyType
pattern SummaryKeyType_AccountMFAEnabled = SummaryKeyType' "AccountMFAEnabled"

pattern SummaryKeyType_AccountSigningCertificatesPresent :: SummaryKeyType
pattern SummaryKeyType_AccountSigningCertificatesPresent = SummaryKeyType' "AccountSigningCertificatesPresent"

pattern SummaryKeyType_AttachedPoliciesPerGroupQuota :: SummaryKeyType
pattern SummaryKeyType_AttachedPoliciesPerGroupQuota = SummaryKeyType' "AttachedPoliciesPerGroupQuota"

pattern SummaryKeyType_AttachedPoliciesPerRoleQuota :: SummaryKeyType
pattern SummaryKeyType_AttachedPoliciesPerRoleQuota = SummaryKeyType' "AttachedPoliciesPerRoleQuota"

pattern SummaryKeyType_AttachedPoliciesPerUserQuota :: SummaryKeyType
pattern SummaryKeyType_AttachedPoliciesPerUserQuota = SummaryKeyType' "AttachedPoliciesPerUserQuota"

pattern SummaryKeyType_GlobalEndpointTokenVersion :: SummaryKeyType
pattern SummaryKeyType_GlobalEndpointTokenVersion = SummaryKeyType' "GlobalEndpointTokenVersion"

pattern SummaryKeyType_GroupPolicySizeQuota :: SummaryKeyType
pattern SummaryKeyType_GroupPolicySizeQuota = SummaryKeyType' "GroupPolicySizeQuota"

pattern SummaryKeyType_Groups :: SummaryKeyType
pattern SummaryKeyType_Groups = SummaryKeyType' "Groups"

pattern SummaryKeyType_GroupsPerUserQuota :: SummaryKeyType
pattern SummaryKeyType_GroupsPerUserQuota = SummaryKeyType' "GroupsPerUserQuota"

pattern SummaryKeyType_GroupsQuota :: SummaryKeyType
pattern SummaryKeyType_GroupsQuota = SummaryKeyType' "GroupsQuota"

pattern SummaryKeyType_MFADevices :: SummaryKeyType
pattern SummaryKeyType_MFADevices = SummaryKeyType' "MFADevices"

pattern SummaryKeyType_MFADevicesInUse :: SummaryKeyType
pattern SummaryKeyType_MFADevicesInUse = SummaryKeyType' "MFADevicesInUse"

pattern SummaryKeyType_Policies :: SummaryKeyType
pattern SummaryKeyType_Policies = SummaryKeyType' "Policies"

pattern SummaryKeyType_PoliciesQuota :: SummaryKeyType
pattern SummaryKeyType_PoliciesQuota = SummaryKeyType' "PoliciesQuota"

pattern SummaryKeyType_PolicySizeQuota :: SummaryKeyType
pattern SummaryKeyType_PolicySizeQuota = SummaryKeyType' "PolicySizeQuota"

pattern SummaryKeyType_PolicyVersionsInUse :: SummaryKeyType
pattern SummaryKeyType_PolicyVersionsInUse = SummaryKeyType' "PolicyVersionsInUse"

pattern SummaryKeyType_PolicyVersionsInUseQuota :: SummaryKeyType
pattern SummaryKeyType_PolicyVersionsInUseQuota = SummaryKeyType' "PolicyVersionsInUseQuota"

pattern SummaryKeyType_ServerCertificates :: SummaryKeyType
pattern SummaryKeyType_ServerCertificates = SummaryKeyType' "ServerCertificates"

pattern SummaryKeyType_ServerCertificatesQuota :: SummaryKeyType
pattern SummaryKeyType_ServerCertificatesQuota = SummaryKeyType' "ServerCertificatesQuota"

pattern SummaryKeyType_SigningCertificatesPerUserQuota :: SummaryKeyType
pattern SummaryKeyType_SigningCertificatesPerUserQuota = SummaryKeyType' "SigningCertificatesPerUserQuota"

pattern SummaryKeyType_UserPolicySizeQuota :: SummaryKeyType
pattern SummaryKeyType_UserPolicySizeQuota = SummaryKeyType' "UserPolicySizeQuota"

pattern SummaryKeyType_Users :: SummaryKeyType
pattern SummaryKeyType_Users = SummaryKeyType' "Users"

pattern SummaryKeyType_UsersQuota :: SummaryKeyType
pattern SummaryKeyType_UsersQuota = SummaryKeyType' "UsersQuota"

pattern SummaryKeyType_VersionsPerPolicyQuota :: SummaryKeyType
pattern SummaryKeyType_VersionsPerPolicyQuota = SummaryKeyType' "VersionsPerPolicyQuota"

{-# COMPLETE
  SummaryKeyType_AccessKeysPerUserQuota,
  SummaryKeyType_AccountAccessKeysPresent,
  SummaryKeyType_AccountMFAEnabled,
  SummaryKeyType_AccountSigningCertificatesPresent,
  SummaryKeyType_AttachedPoliciesPerGroupQuota,
  SummaryKeyType_AttachedPoliciesPerRoleQuota,
  SummaryKeyType_AttachedPoliciesPerUserQuota,
  SummaryKeyType_GlobalEndpointTokenVersion,
  SummaryKeyType_GroupPolicySizeQuota,
  SummaryKeyType_Groups,
  SummaryKeyType_GroupsPerUserQuota,
  SummaryKeyType_GroupsQuota,
  SummaryKeyType_MFADevices,
  SummaryKeyType_MFADevicesInUse,
  SummaryKeyType_Policies,
  SummaryKeyType_PoliciesQuota,
  SummaryKeyType_PolicySizeQuota,
  SummaryKeyType_PolicyVersionsInUse,
  SummaryKeyType_PolicyVersionsInUseQuota,
  SummaryKeyType_ServerCertificates,
  SummaryKeyType_ServerCertificatesQuota,
  SummaryKeyType_SigningCertificatesPerUserQuota,
  SummaryKeyType_UserPolicySizeQuota,
  SummaryKeyType_Users,
  SummaryKeyType_UsersQuota,
  SummaryKeyType_VersionsPerPolicyQuota,
  SummaryKeyType'
  #-}
