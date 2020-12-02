{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.Sum where

import Network.AWS.Prelude

data AssignmentStatusType
  = Any
  | Assigned
  | Unassigned
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssignmentStatusType where
    parser = takeLowerText >>= \case
        "any" -> pure Any
        "assigned" -> pure Assigned
        "unassigned" -> pure Unassigned
        e -> fromTextError $ "Failure parsing AssignmentStatusType from value: '" <> e
           <> "'. Accepted values: any, assigned, unassigned"

instance ToText AssignmentStatusType where
    toText = \case
        Any -> "Any"
        Assigned -> "Assigned"
        Unassigned -> "Unassigned"

instance Hashable     AssignmentStatusType
instance NFData       AssignmentStatusType
instance ToByteString AssignmentStatusType
instance ToQuery      AssignmentStatusType
instance ToHeader     AssignmentStatusType

data ContextKeyTypeEnum
  = Binary
  | BinaryList
  | Boolean
  | BooleanList
  | Date
  | DateList
  | IP
  | IPList
  | Numeric
  | NumericList
  | String
  | StringList
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContextKeyTypeEnum where
    parser = takeLowerText >>= \case
        "binary" -> pure Binary
        "binarylist" -> pure BinaryList
        "boolean" -> pure Boolean
        "booleanlist" -> pure BooleanList
        "date" -> pure Date
        "datelist" -> pure DateList
        "ip" -> pure IP
        "iplist" -> pure IPList
        "numeric" -> pure Numeric
        "numericlist" -> pure NumericList
        "string" -> pure String
        "stringlist" -> pure StringList
        e -> fromTextError $ "Failure parsing ContextKeyTypeEnum from value: '" <> e
           <> "'. Accepted values: binary, binarylist, boolean, booleanlist, date, datelist, ip, iplist, numeric, numericlist, string, stringlist"

instance ToText ContextKeyTypeEnum where
    toText = \case
        Binary -> "binary"
        BinaryList -> "binaryList"
        Boolean -> "boolean"
        BooleanList -> "booleanList"
        Date -> "date"
        DateList -> "dateList"
        IP -> "ip"
        IPList -> "ipList"
        Numeric -> "numeric"
        NumericList -> "numericList"
        String -> "string"
        StringList -> "stringList"

instance Hashable     ContextKeyTypeEnum
instance NFData       ContextKeyTypeEnum
instance ToByteString ContextKeyTypeEnum
instance ToQuery      ContextKeyTypeEnum
instance ToHeader     ContextKeyTypeEnum

data DeletionTaskStatusType
  = Failed
  | InProgress
  | NotStarted
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeletionTaskStatusType where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "not_started" -> pure NotStarted
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing DeletionTaskStatusType from value: '" <> e
           <> "'. Accepted values: failed, in_progress, not_started, succeeded"

instance ToText DeletionTaskStatusType where
    toText = \case
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        NotStarted -> "NOT_STARTED"
        Succeeded -> "SUCCEEDED"

instance Hashable     DeletionTaskStatusType
instance NFData       DeletionTaskStatusType
instance ToByteString DeletionTaskStatusType
instance ToQuery      DeletionTaskStatusType
instance ToHeader     DeletionTaskStatusType

instance FromXML DeletionTaskStatusType where
    parseXML = parseXMLText "DeletionTaskStatusType"

data EncodingType
  = Pem
  | SSH
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EncodingType where
    parser = takeLowerText >>= \case
        "pem" -> pure Pem
        "ssh" -> pure SSH
        e -> fromTextError $ "Failure parsing EncodingType from value: '" <> e
           <> "'. Accepted values: pem, ssh"

instance ToText EncodingType where
    toText = \case
        Pem -> "PEM"
        SSH -> "SSH"

instance Hashable     EncodingType
instance NFData       EncodingType
instance ToByteString EncodingType
instance ToQuery      EncodingType
instance ToHeader     EncodingType

data EntityType
  = ETAWSManagedPolicy
  | ETGroup
  | ETLocalManagedPolicy
  | ETRole
  | ETUser
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EntityType where
    parser = takeLowerText >>= \case
        "awsmanagedpolicy" -> pure ETAWSManagedPolicy
        "group" -> pure ETGroup
        "localmanagedpolicy" -> pure ETLocalManagedPolicy
        "role" -> pure ETRole
        "user" -> pure ETUser
        e -> fromTextError $ "Failure parsing EntityType from value: '" <> e
           <> "'. Accepted values: awsmanagedpolicy, group, localmanagedpolicy, role, user"

instance ToText EntityType where
    toText = \case
        ETAWSManagedPolicy -> "AWSManagedPolicy"
        ETGroup -> "Group"
        ETLocalManagedPolicy -> "LocalManagedPolicy"
        ETRole -> "Role"
        ETUser -> "User"

instance Hashable     EntityType
instance NFData       EntityType
instance ToByteString EntityType
instance ToQuery      EntityType
instance ToHeader     EntityType

data PolicyEvaluationDecisionType
  = Allowed
  | ExplicitDeny
  | ImplicitDeny
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PolicyEvaluationDecisionType where
    parser = takeLowerText >>= \case
        "allowed" -> pure Allowed
        "explicitdeny" -> pure ExplicitDeny
        "implicitdeny" -> pure ImplicitDeny
        e -> fromTextError $ "Failure parsing PolicyEvaluationDecisionType from value: '" <> e
           <> "'. Accepted values: allowed, explicitdeny, implicitdeny"

instance ToText PolicyEvaluationDecisionType where
    toText = \case
        Allowed -> "allowed"
        ExplicitDeny -> "explicitDeny"
        ImplicitDeny -> "implicitDeny"

instance Hashable     PolicyEvaluationDecisionType
instance NFData       PolicyEvaluationDecisionType
instance ToByteString PolicyEvaluationDecisionType
instance ToQuery      PolicyEvaluationDecisionType
instance ToHeader     PolicyEvaluationDecisionType

instance FromXML PolicyEvaluationDecisionType where
    parseXML = parseXMLText "PolicyEvaluationDecisionType"

data PolicyScopeType
  = AWS
  | All
  | Local
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PolicyScopeType where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "all" -> pure All
        "local" -> pure Local
        e -> fromTextError $ "Failure parsing PolicyScopeType from value: '" <> e
           <> "'. Accepted values: aws, all, local"

instance ToText PolicyScopeType where
    toText = \case
        AWS -> "AWS"
        All -> "All"
        Local -> "Local"

instance Hashable     PolicyScopeType
instance NFData       PolicyScopeType
instance ToByteString PolicyScopeType
instance ToQuery      PolicyScopeType
instance ToHeader     PolicyScopeType

data PolicySourceType
  = AWSManaged
  | Group
  | None
  | Resource
  | Role
  | User
  | UserManaged
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PolicySourceType where
    parser = takeLowerText >>= \case
        "aws-managed" -> pure AWSManaged
        "group" -> pure Group
        "none" -> pure None
        "resource" -> pure Resource
        "role" -> pure Role
        "user" -> pure User
        "user-managed" -> pure UserManaged
        e -> fromTextError $ "Failure parsing PolicySourceType from value: '" <> e
           <> "'. Accepted values: aws-managed, group, none, resource, role, user, user-managed"

instance ToText PolicySourceType where
    toText = \case
        AWSManaged -> "aws-managed"
        Group -> "group"
        None -> "none"
        Resource -> "resource"
        Role -> "role"
        User -> "user"
        UserManaged -> "user-managed"

instance Hashable     PolicySourceType
instance NFData       PolicySourceType
instance ToByteString PolicySourceType
instance ToQuery      PolicySourceType
instance ToHeader     PolicySourceType

instance FromXML PolicySourceType where
    parseXML = parseXMLText "PolicySourceType"

data ReportFormatType =
  TextCSV
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportFormatType where
    parser = takeLowerText >>= \case
        "text/csv" -> pure TextCSV
        e -> fromTextError $ "Failure parsing ReportFormatType from value: '" <> e
           <> "'. Accepted values: text/csv"

instance ToText ReportFormatType where
    toText = \case
        TextCSV -> "text/csv"

instance Hashable     ReportFormatType
instance NFData       ReportFormatType
instance ToByteString ReportFormatType
instance ToQuery      ReportFormatType
instance ToHeader     ReportFormatType

instance FromXML ReportFormatType where
    parseXML = parseXMLText "ReportFormatType"

data ReportStateType
  = RSTComplete
  | RSTInprogress
  | RSTStarted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportStateType where
    parser = takeLowerText >>= \case
        "complete" -> pure RSTComplete
        "inprogress" -> pure RSTInprogress
        "started" -> pure RSTStarted
        e -> fromTextError $ "Failure parsing ReportStateType from value: '" <> e
           <> "'. Accepted values: complete, inprogress, started"

instance ToText ReportStateType where
    toText = \case
        RSTComplete -> "COMPLETE"
        RSTInprogress -> "INPROGRESS"
        RSTStarted -> "STARTED"

instance Hashable     ReportStateType
instance NFData       ReportStateType
instance ToByteString ReportStateType
instance ToQuery      ReportStateType
instance ToHeader     ReportStateType

instance FromXML ReportStateType where
    parseXML = parseXMLText "ReportStateType"

data StatusType
  = Active
  | Inactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StatusType where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText StatusType where
    toText = \case
        Active -> "Active"
        Inactive -> "Inactive"

instance Hashable     StatusType
instance NFData       StatusType
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
instance NFData       SummaryKeyType
instance ToByteString SummaryKeyType
instance ToQuery      SummaryKeyType
instance ToHeader     SummaryKeyType

instance FromXML SummaryKeyType where
    parseXML = parseXMLText "SummaryKeyType"
