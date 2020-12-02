{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SummaryKeyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SummaryKeyType where

import Network.AWS.Prelude

data SummaryKeyType
  = AccessKeysPerUserQuota
  | AccountAccessKeysPresent
  | AccountMFAEnabled
  | AccountSigningCertificatesPresent
  | AttachedPoliciesPerGroupQuota
  | AttachedPoliciesPerRoleQuota
  | AttachedPoliciesPerUserQuota
  | GlobalEndpointTokenVersion
  | GroupPolicySizeQuota
  | Groups
  | GroupsPerUserQuota
  | GroupsQuota
  | MFADevices
  | MFADevicesInUse
  | Policies
  | PoliciesQuota
  | PolicySizeQuota
  | PolicyVersionsInUse
  | PolicyVersionsInUseQuota
  | ServerCertificates
  | ServerCertificatesQuota
  | SigningCertificatesPerUserQuota
  | UserPolicySizeQuota
  | Users
  | UsersQuota
  | VersionsPerPolicyQuota
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText SummaryKeyType where
  parser =
    takeLowerText >>= \case
      "accesskeysperuserquota" -> pure AccessKeysPerUserQuota
      "accountaccesskeyspresent" -> pure AccountAccessKeysPresent
      "accountmfaenabled" -> pure AccountMFAEnabled
      "accountsigningcertificatespresent" -> pure AccountSigningCertificatesPresent
      "attachedpoliciespergroupquota" -> pure AttachedPoliciesPerGroupQuota
      "attachedpoliciesperrolequota" -> pure AttachedPoliciesPerRoleQuota
      "attachedpoliciesperuserquota" -> pure AttachedPoliciesPerUserQuota
      "globalendpointtokenversion" -> pure GlobalEndpointTokenVersion
      "grouppolicysizequota" -> pure GroupPolicySizeQuota
      "groups" -> pure Groups
      "groupsperuserquota" -> pure GroupsPerUserQuota
      "groupsquota" -> pure GroupsQuota
      "mfadevices" -> pure MFADevices
      "mfadevicesinuse" -> pure MFADevicesInUse
      "policies" -> pure Policies
      "policiesquota" -> pure PoliciesQuota
      "policysizequota" -> pure PolicySizeQuota
      "policyversionsinuse" -> pure PolicyVersionsInUse
      "policyversionsinusequota" -> pure PolicyVersionsInUseQuota
      "servercertificates" -> pure ServerCertificates
      "servercertificatesquota" -> pure ServerCertificatesQuota
      "signingcertificatesperuserquota" -> pure SigningCertificatesPerUserQuota
      "userpolicysizequota" -> pure UserPolicySizeQuota
      "users" -> pure Users
      "usersquota" -> pure UsersQuota
      "versionsperpolicyquota" -> pure VersionsPerPolicyQuota
      e ->
        fromTextError $
          "Failure parsing SummaryKeyType from value: '" <> e
            <> "'. Accepted values: accesskeysperuserquota, accountaccesskeyspresent, accountmfaenabled, accountsigningcertificatespresent, attachedpoliciespergroupquota, attachedpoliciesperrolequota, attachedpoliciesperuserquota, globalendpointtokenversion, grouppolicysizequota, groups, groupsperuserquota, groupsquota, mfadevices, mfadevicesinuse, policies, policiesquota, policysizequota, policyversionsinuse, policyversionsinusequota, servercertificates, servercertificatesquota, signingcertificatesperuserquota, userpolicysizequota, users, usersquota, versionsperpolicyquota"

instance ToText SummaryKeyType where
  toText = \case
    AccessKeysPerUserQuota -> "AccessKeysPerUserQuota"
    AccountAccessKeysPresent -> "AccountAccessKeysPresent"
    AccountMFAEnabled -> "AccountMFAEnabled"
    AccountSigningCertificatesPresent -> "AccountSigningCertificatesPresent"
    AttachedPoliciesPerGroupQuota -> "AttachedPoliciesPerGroupQuota"
    AttachedPoliciesPerRoleQuota -> "AttachedPoliciesPerRoleQuota"
    AttachedPoliciesPerUserQuota -> "AttachedPoliciesPerUserQuota"
    GlobalEndpointTokenVersion -> "GlobalEndpointTokenVersion"
    GroupPolicySizeQuota -> "GroupPolicySizeQuota"
    Groups -> "Groups"
    GroupsPerUserQuota -> "GroupsPerUserQuota"
    GroupsQuota -> "GroupsQuota"
    MFADevices -> "MFADevices"
    MFADevicesInUse -> "MFADevicesInUse"
    Policies -> "Policies"
    PoliciesQuota -> "PoliciesQuota"
    PolicySizeQuota -> "PolicySizeQuota"
    PolicyVersionsInUse -> "PolicyVersionsInUse"
    PolicyVersionsInUseQuota -> "PolicyVersionsInUseQuota"
    ServerCertificates -> "ServerCertificates"
    ServerCertificatesQuota -> "ServerCertificatesQuota"
    SigningCertificatesPerUserQuota -> "SigningCertificatesPerUserQuota"
    UserPolicySizeQuota -> "UserPolicySizeQuota"
    Users -> "Users"
    UsersQuota -> "UsersQuota"
    VersionsPerPolicyQuota -> "VersionsPerPolicyQuota"

instance Hashable SummaryKeyType

instance NFData SummaryKeyType

instance ToByteString SummaryKeyType

instance ToQuery SummaryKeyType

instance ToHeader SummaryKeyType

instance FromXML SummaryKeyType where
  parseXML = parseXMLText "SummaryKeyType"
