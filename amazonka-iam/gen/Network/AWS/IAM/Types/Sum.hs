{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.Sum where

import           Network.AWS.Prelude

data AssignmentStatusType
    = Any
    | Assigned
    | Unassigned
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AssignmentStatusType where
    parser = takeLowerText >>= \case
        "any" -> pure Any
        "assigned" -> pure Assigned
        "unassigned" -> pure Unassigned
        e -> fromTextError $ "Failure parsing AssignmentStatusType from value: '" <> e
           <> "'. Accepted values: Any, Assigned, Unassigned"

instance ToText AssignmentStatusType where
    toText = \case
        Any -> "Any"
        Assigned -> "Assigned"
        Unassigned -> "Unassigned"

instance Hashable     AssignmentStatusType
instance ToByteString AssignmentStatusType
instance ToQuery      AssignmentStatusType
instance ToHeader     AssignmentStatusType

data EncodingType
    = Pem
    | SSH
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EncodingType where
    parser = takeLowerText >>= \case
        "pem" -> pure Pem
        "ssh" -> pure SSH
        e -> fromTextError $ "Failure parsing EncodingType from value: '" <> e
           <> "'. Accepted values: PEM, SSH"

instance ToText EncodingType where
    toText = \case
        Pem -> "PEM"
        SSH -> "SSH"

instance Hashable     EncodingType
instance ToByteString EncodingType
instance ToQuery      EncodingType
instance ToHeader     EncodingType

data EntityType
    = AWSManagedPolicy
    | Group
    | LocalManagedPolicy
    | Role
    | User
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EntityType where
    parser = takeLowerText >>= \case
        "awsmanagedpolicy" -> pure AWSManagedPolicy
        "group" -> pure Group
        "localmanagedpolicy" -> pure LocalManagedPolicy
        "role" -> pure Role
        "user" -> pure User
        e -> fromTextError $ "Failure parsing EntityType from value: '" <> e
           <> "'. Accepted values: AWSManagedPolicy, Group, LocalManagedPolicy, Role, User"

instance ToText EntityType where
    toText = \case
        AWSManagedPolicy -> "AWSManagedPolicy"
        Group -> "Group"
        LocalManagedPolicy -> "LocalManagedPolicy"
        Role -> "Role"
        User -> "User"

instance Hashable     EntityType
instance ToByteString EntityType
instance ToQuery      EntityType
instance ToHeader     EntityType

data PolicyScopeType
    = AWS
    | All
    | Local
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PolicyScopeType where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "all" -> pure All
        "local" -> pure Local
        e -> fromTextError $ "Failure parsing PolicyScopeType from value: '" <> e
           <> "'. Accepted values: AWS, All, Local"

instance ToText PolicyScopeType where
    toText = \case
        AWS -> "AWS"
        All -> "All"
        Local -> "Local"

instance Hashable     PolicyScopeType
instance ToByteString PolicyScopeType
instance ToQuery      PolicyScopeType
instance ToHeader     PolicyScopeType

data ReportFormatType =
    TextCSV
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReportFormatType where
    parser = takeLowerText >>= \case
        "text/csv" -> pure TextCSV
        e -> fromTextError $ "Failure parsing ReportFormatType from value: '" <> e
           <> "'. Accepted values: text/csv"

instance ToText ReportFormatType where
    toText = \case
        TextCSV -> "text/csv"

instance Hashable     ReportFormatType
instance ToByteString ReportFormatType
instance ToQuery      ReportFormatType
instance ToHeader     ReportFormatType

instance FromXML ReportFormatType where
    parseXML = parseXMLText "ReportFormatType"

data ReportStateType
    = Complete
    | Inprogress
    | Started
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReportStateType where
    parser = takeLowerText >>= \case
        "complete" -> pure Complete
        "inprogress" -> pure Inprogress
        "started" -> pure Started
        e -> fromTextError $ "Failure parsing ReportStateType from value: '" <> e
           <> "'. Accepted values: COMPLETE, INPROGRESS, STARTED"

instance ToText ReportStateType where
    toText = \case
        Complete -> "COMPLETE"
        Inprogress -> "INPROGRESS"
        Started -> "STARTED"

instance Hashable     ReportStateType
instance ToByteString ReportStateType
instance ToQuery      ReportStateType
instance ToHeader     ReportStateType

instance FromXML ReportStateType where
    parseXML = parseXMLText "ReportStateType"

data StatusType
    = Active
    | Inactive
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StatusType where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: Active, Inactive"

instance ToText StatusType where
    toText = \case
        Active -> "Active"
        Inactive -> "Inactive"

instance Hashable     StatusType
instance ToByteString StatusType
instance ToQuery      StatusType
instance ToHeader     StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

data SummaryKeyType
    = AccessKeysPerUserQuota
    | AccountAccessKeysPresent
    | AccountMFAEnabled
    | AccountSigningCertificatesPresent
    | AttachedPoliciesPerGroupQuota
    | AttachedPoliciesPerRoleQuota
    | AttachedPoliciesPerUserQuota
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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SummaryKeyType where
    parser = takeLowerText >>= \case
        "accesskeysperuserquota" -> pure AccessKeysPerUserQuota
        "accountaccesskeyspresent" -> pure AccountAccessKeysPresent
        "accountmfaenabled" -> pure AccountMFAEnabled
        "accountsigningcertificatespresent" -> pure AccountSigningCertificatesPresent
        "attachedpoliciespergroupquota" -> pure AttachedPoliciesPerGroupQuota
        "attachedpoliciesperrolequota" -> pure AttachedPoliciesPerRoleQuota
        "attachedpoliciesperuserquota" -> pure AttachedPoliciesPerUserQuota
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
        e -> fromTextError $ "Failure parsing SummaryKeyType from value: '" <> e
           <> "'. Accepted values: AccessKeysPerUserQuota, AccountAccessKeysPresent, AccountMFAEnabled, AccountSigningCertificatesPresent, AttachedPoliciesPerGroupQuota, AttachedPoliciesPerRoleQuota, AttachedPoliciesPerUserQuota, GroupPolicySizeQuota, Groups, GroupsPerUserQuota, GroupsQuota, MFADevices, MFADevicesInUse, Policies, PoliciesQuota, PolicySizeQuota, PolicyVersionsInUse, PolicyVersionsInUseQuota, ServerCertificates, ServerCertificatesQuota, SigningCertificatesPerUserQuota, UserPolicySizeQuota, Users, UsersQuota, VersionsPerPolicyQuota"

instance ToText SummaryKeyType where
    toText = \case
        AccessKeysPerUserQuota -> "AccessKeysPerUserQuota"
        AccountAccessKeysPresent -> "AccountAccessKeysPresent"
        AccountMFAEnabled -> "AccountMFAEnabled"
        AccountSigningCertificatesPresent -> "AccountSigningCertificatesPresent"
        AttachedPoliciesPerGroupQuota -> "AttachedPoliciesPerGroupQuota"
        AttachedPoliciesPerRoleQuota -> "AttachedPoliciesPerRoleQuota"
        AttachedPoliciesPerUserQuota -> "AttachedPoliciesPerUserQuota"
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

instance Hashable     SummaryKeyType
instance ToByteString SummaryKeyType
instance ToQuery      SummaryKeyType
instance ToHeader     SummaryKeyType

instance FromXML SummaryKeyType where
    parseXML = parseXMLText "SummaryKeyType"
