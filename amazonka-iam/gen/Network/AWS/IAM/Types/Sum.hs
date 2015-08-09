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

import Network.AWS.Prelude

data AssignmentStatusType
    = Assigned 
    | Unassigned 
    | Any 
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AssignmentStatusType where
    parser = takeLowerText >>= \case
        "any" -> pure Any
        "assigned" -> pure Assigned
        "unassigned" -> pure Unassigned
        e -> fromTextError $ "Failure parsing AssignmentStatusType from value: '" <> e
           <> "'. Accepted values: any, assigned, unassigned"

instance ToText AssignmentStatusType where
    toText = \case
        Any -> "any"
        Assigned -> "assigned"
        Unassigned -> "unassigned"

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
           <> "'. Accepted values: pem, ssh"

instance ToText EncodingType where
    toText = \case
        Pem -> "pem"
        SSH -> "ssh"

instance Hashable     EncodingType
instance ToByteString EncodingType
instance ToQuery      EncodingType
instance ToHeader     EncodingType

data EntityType
    = Group 
    | LocalManagedPolicy 
    | AWSManagedPolicy 
    | User 
    | Role 
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EntityType where
    parser = takeLowerText >>= \case
        "awsmanagedpolicy" -> pure AWSManagedPolicy
        "group" -> pure Group
        "localmanagedpolicy" -> pure LocalManagedPolicy
        "role" -> pure Role
        "user" -> pure User
        e -> fromTextError $ "Failure parsing EntityType from value: '" <> e
           <> "'. Accepted values: awsmanagedpolicy, group, localmanagedpolicy, role, user"

instance ToText EntityType where
    toText = \case
        AWSManagedPolicy -> "awsmanagedpolicy"
        Group -> "group"
        LocalManagedPolicy -> "localmanagedpolicy"
        Role -> "role"
        User -> "user"

instance Hashable     EntityType
instance ToByteString EntityType
instance ToQuery      EntityType
instance ToHeader     EntityType

data PolicyScopeType
    = AWS 
    | Local 
    | All 
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PolicyScopeType where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "all" -> pure All
        "local" -> pure Local
        e -> fromTextError $ "Failure parsing PolicyScopeType from value: '" <> e
           <> "'. Accepted values: aws, all, local"

instance ToText PolicyScopeType where
    toText = \case
        AWS -> "aws"
        All -> "all"
        Local -> "local"

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
    = Inprogress 
    | Started 
    | Complete 
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReportStateType where
    parser = takeLowerText >>= \case
        "complete" -> pure Complete
        "inprogress" -> pure Inprogress
        "started" -> pure Started
        e -> fromTextError $ "Failure parsing ReportStateType from value: '" <> e
           <> "'. Accepted values: complete, inprogress, started"

instance ToText ReportStateType where
    toText = \case
        Complete -> "complete"
        Inprogress -> "inprogress"
        Started -> "started"

instance Hashable     ReportStateType
instance ToByteString ReportStateType
instance ToQuery      ReportStateType
instance ToHeader     ReportStateType

instance FromXML ReportStateType where
    parseXML = parseXMLText "ReportStateType"

data StatusType
    = Inactive 
    | Active 
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StatusType where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText StatusType where
    toText = \case
        Active -> "active"
        Inactive -> "inactive"

instance Hashable     StatusType
instance ToByteString StatusType
instance ToQuery      StatusType
instance ToHeader     StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

data SummaryKeyType
    = AttachedPoliciesPerUserQuota 
    | UsersQuota 
    | Groups 
    | GroupsQuota 
    | Users 
    | MFADevicesInUse 
    | PolicyVersionsInUse 
    | SigningCertificatesPerUserQuota 
    | PoliciesQuota 
    | AccessKeysPerUserQuota 
    | PolicySizeQuota 
    | ServerCertificates 
    | AttachedPoliciesPerRoleQuota 
    | GroupsPerUserQuota 
    | GroupPolicySizeQuota 
    | AccountSigningCertificatesPresent 
    | UserPolicySizeQuota 
    | AttachedPoliciesPerGroupQuota 
    | AccountAccessKeysPresent 
    | ServerCertificatesQuota 
    | VersionsPerPolicyQuota 
    | PolicyVersionsInUseQuota 
    | Policies 
    | AccountMFAEnabled 
    | MFADevices 
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
           <> "'. Accepted values: accesskeysperuserquota, accountaccesskeyspresent, accountmfaenabled, accountsigningcertificatespresent, attachedpoliciespergroupquota, attachedpoliciesperrolequota, attachedpoliciesperuserquota, grouppolicysizequota, groups, groupsperuserquota, groupsquota, mfadevices, mfadevicesinuse, policies, policiesquota, policysizequota, policyversionsinuse, policyversionsinusequota, servercertificates, servercertificatesquota, signingcertificatesperuserquota, userpolicysizequota, users, usersquota, versionsperpolicyquota"

instance ToText SummaryKeyType where
    toText = \case
        AccessKeysPerUserQuota -> "accesskeysperuserquota"
        AccountAccessKeysPresent -> "accountaccesskeyspresent"
        AccountMFAEnabled -> "accountmfaenabled"
        AccountSigningCertificatesPresent -> "accountsigningcertificatespresent"
        AttachedPoliciesPerGroupQuota -> "attachedpoliciespergroupquota"
        AttachedPoliciesPerRoleQuota -> "attachedpoliciesperrolequota"
        AttachedPoliciesPerUserQuota -> "attachedpoliciesperuserquota"
        GroupPolicySizeQuota -> "grouppolicysizequota"
        Groups -> "groups"
        GroupsPerUserQuota -> "groupsperuserquota"
        GroupsQuota -> "groupsquota"
        MFADevices -> "mfadevices"
        MFADevicesInUse -> "mfadevicesinuse"
        Policies -> "policies"
        PoliciesQuota -> "policiesquota"
        PolicySizeQuota -> "policysizequota"
        PolicyVersionsInUse -> "policyversionsinuse"
        PolicyVersionsInUseQuota -> "policyversionsinusequota"
        ServerCertificates -> "servercertificates"
        ServerCertificatesQuota -> "servercertificatesquota"
        SigningCertificatesPerUserQuota -> "signingcertificatesperuserquota"
        UserPolicySizeQuota -> "userpolicysizequota"
        Users -> "users"
        UsersQuota -> "usersquota"
        VersionsPerPolicyQuota -> "versionsperpolicyquota"

instance Hashable     SummaryKeyType
instance ToByteString SummaryKeyType
instance ToQuery      SummaryKeyType
instance ToHeader     SummaryKeyType

instance FromXML SummaryKeyType where
    parseXML = parseXMLText "SummaryKeyType"
