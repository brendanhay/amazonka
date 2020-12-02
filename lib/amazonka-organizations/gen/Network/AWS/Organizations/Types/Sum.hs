{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.Sum where

import Network.AWS.Prelude

data AccountJoinedMethod
  = Created
  | Invited
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccountJoinedMethod where
    parser = takeLowerText >>= \case
        "created" -> pure Created
        "invited" -> pure Invited
        e -> fromTextError $ "Failure parsing AccountJoinedMethod from value: '" <> e
           <> "'. Accepted values: created, invited"

instance ToText AccountJoinedMethod where
    toText = \case
        Created -> "CREATED"
        Invited -> "INVITED"

instance Hashable     AccountJoinedMethod
instance NFData       AccountJoinedMethod
instance ToByteString AccountJoinedMethod
instance ToQuery      AccountJoinedMethod
instance ToHeader     AccountJoinedMethod

instance FromJSON AccountJoinedMethod where
    parseJSON = parseJSONText "AccountJoinedMethod"

data AccountStatus
  = Active
  | Suspended
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccountStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "suspended" -> pure Suspended
        e -> fromTextError $ "Failure parsing AccountStatus from value: '" <> e
           <> "'. Accepted values: active, suspended"

instance ToText AccountStatus where
    toText = \case
        Active -> "ACTIVE"
        Suspended -> "SUSPENDED"

instance Hashable     AccountStatus
instance NFData       AccountStatus
instance ToByteString AccountStatus
instance ToQuery      AccountStatus
instance ToHeader     AccountStatus

instance FromJSON AccountStatus where
    parseJSON = parseJSONText "AccountStatus"

data ActionType
  = AddOrganizationsServiceLinkedRole
  | ApproveAllFeatures
  | EnableAllFeatures
  | Invite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionType where
    parser = takeLowerText >>= \case
        "add_organizations_service_linked_role" -> pure AddOrganizationsServiceLinkedRole
        "approve_all_features" -> pure ApproveAllFeatures
        "enable_all_features" -> pure EnableAllFeatures
        "invite" -> pure Invite
        e -> fromTextError $ "Failure parsing ActionType from value: '" <> e
           <> "'. Accepted values: add_organizations_service_linked_role, approve_all_features, enable_all_features, invite"

instance ToText ActionType where
    toText = \case
        AddOrganizationsServiceLinkedRole -> "ADD_ORGANIZATIONS_SERVICE_LINKED_ROLE"
        ApproveAllFeatures -> "APPROVE_ALL_FEATURES"
        EnableAllFeatures -> "ENABLE_ALL_FEATURES"
        Invite -> "INVITE"

instance Hashable     ActionType
instance NFData       ActionType
instance ToByteString ActionType
instance ToQuery      ActionType
instance ToHeader     ActionType

instance ToJSON ActionType where
    toJSON = toJSONText

instance FromJSON ActionType where
    parseJSON = parseJSONText "ActionType"

data ChildType
  = CTAccount
  | CTOrganizationalUnit
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChildType where
    parser = takeLowerText >>= \case
        "account" -> pure CTAccount
        "organizational_unit" -> pure CTOrganizationalUnit
        e -> fromTextError $ "Failure parsing ChildType from value: '" <> e
           <> "'. Accepted values: account, organizational_unit"

instance ToText ChildType where
    toText = \case
        CTAccount -> "ACCOUNT"
        CTOrganizationalUnit -> "ORGANIZATIONAL_UNIT"

instance Hashable     ChildType
instance NFData       ChildType
instance ToByteString ChildType
instance ToQuery      ChildType
instance ToHeader     ChildType

instance ToJSON ChildType where
    toJSON = toJSONText

instance FromJSON ChildType where
    parseJSON = parseJSONText "ChildType"

data CreateAccountFailureReason
  = AccountLimitExceeded
  | ConcurrentAccountModification
  | EmailAlreadyExists
  | InternalFailure
  | InvalidAddress
  | InvalidEmail
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CreateAccountFailureReason where
    parser = takeLowerText >>= \case
        "account_limit_exceeded" -> pure AccountLimitExceeded
        "concurrent_account_modification" -> pure ConcurrentAccountModification
        "email_already_exists" -> pure EmailAlreadyExists
        "internal_failure" -> pure InternalFailure
        "invalid_address" -> pure InvalidAddress
        "invalid_email" -> pure InvalidEmail
        e -> fromTextError $ "Failure parsing CreateAccountFailureReason from value: '" <> e
           <> "'. Accepted values: account_limit_exceeded, concurrent_account_modification, email_already_exists, internal_failure, invalid_address, invalid_email"

instance ToText CreateAccountFailureReason where
    toText = \case
        AccountLimitExceeded -> "ACCOUNT_LIMIT_EXCEEDED"
        ConcurrentAccountModification -> "CONCURRENT_ACCOUNT_MODIFICATION"
        EmailAlreadyExists -> "EMAIL_ALREADY_EXISTS"
        InternalFailure -> "INTERNAL_FAILURE"
        InvalidAddress -> "INVALID_ADDRESS"
        InvalidEmail -> "INVALID_EMAIL"

instance Hashable     CreateAccountFailureReason
instance NFData       CreateAccountFailureReason
instance ToByteString CreateAccountFailureReason
instance ToQuery      CreateAccountFailureReason
instance ToHeader     CreateAccountFailureReason

instance FromJSON CreateAccountFailureReason where
    parseJSON = parseJSONText "CreateAccountFailureReason"

data CreateAccountState
  = Failed
  | InProgress
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CreateAccountState where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing CreateAccountState from value: '" <> e
           <> "'. Accepted values: failed, in_progress, succeeded"

instance ToText CreateAccountState where
    toText = \case
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        Succeeded -> "SUCCEEDED"

instance Hashable     CreateAccountState
instance NFData       CreateAccountState
instance ToByteString CreateAccountState
instance ToQuery      CreateAccountState
instance ToHeader     CreateAccountState

instance ToJSON CreateAccountState where
    toJSON = toJSONText

instance FromJSON CreateAccountState where
    parseJSON = parseJSONText "CreateAccountState"

data HandshakePartyType
  = HPTAccount
  | HPTEmail
  | HPTOrganization
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HandshakePartyType where
    parser = takeLowerText >>= \case
        "account" -> pure HPTAccount
        "email" -> pure HPTEmail
        "organization" -> pure HPTOrganization
        e -> fromTextError $ "Failure parsing HandshakePartyType from value: '" <> e
           <> "'. Accepted values: account, email, organization"

instance ToText HandshakePartyType where
    toText = \case
        HPTAccount -> "ACCOUNT"
        HPTEmail -> "EMAIL"
        HPTOrganization -> "ORGANIZATION"

instance Hashable     HandshakePartyType
instance NFData       HandshakePartyType
instance ToByteString HandshakePartyType
instance ToQuery      HandshakePartyType
instance ToHeader     HandshakePartyType

instance ToJSON HandshakePartyType where
    toJSON = toJSONText

instance FromJSON HandshakePartyType where
    parseJSON = parseJSONText "HandshakePartyType"

data HandshakeResourceType
  = Account
  | Email
  | MasterEmail
  | MasterName
  | Notes
  | Organization
  | OrganizationFeatureSet
  | ParentHandshake
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HandshakeResourceType where
    parser = takeLowerText >>= \case
        "account" -> pure Account
        "email" -> pure Email
        "master_email" -> pure MasterEmail
        "master_name" -> pure MasterName
        "notes" -> pure Notes
        "organization" -> pure Organization
        "organization_feature_set" -> pure OrganizationFeatureSet
        "parent_handshake" -> pure ParentHandshake
        e -> fromTextError $ "Failure parsing HandshakeResourceType from value: '" <> e
           <> "'. Accepted values: account, email, master_email, master_name, notes, organization, organization_feature_set, parent_handshake"

instance ToText HandshakeResourceType where
    toText = \case
        Account -> "ACCOUNT"
        Email -> "EMAIL"
        MasterEmail -> "MASTER_EMAIL"
        MasterName -> "MASTER_NAME"
        Notes -> "NOTES"
        Organization -> "ORGANIZATION"
        OrganizationFeatureSet -> "ORGANIZATION_FEATURE_SET"
        ParentHandshake -> "PARENT_HANDSHAKE"

instance Hashable     HandshakeResourceType
instance NFData       HandshakeResourceType
instance ToByteString HandshakeResourceType
instance ToQuery      HandshakeResourceType
instance ToHeader     HandshakeResourceType

instance FromJSON HandshakeResourceType where
    parseJSON = parseJSONText "HandshakeResourceType"

data HandshakeState
  = Accepted
  | Canceled
  | Declined
  | Expired
  | Open
  | Requested
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HandshakeState where
    parser = takeLowerText >>= \case
        "accepted" -> pure Accepted
        "canceled" -> pure Canceled
        "declined" -> pure Declined
        "expired" -> pure Expired
        "open" -> pure Open
        "requested" -> pure Requested
        e -> fromTextError $ "Failure parsing HandshakeState from value: '" <> e
           <> "'. Accepted values: accepted, canceled, declined, expired, open, requested"

instance ToText HandshakeState where
    toText = \case
        Accepted -> "ACCEPTED"
        Canceled -> "CANCELED"
        Declined -> "DECLINED"
        Expired -> "EXPIRED"
        Open -> "OPEN"
        Requested -> "REQUESTED"

instance Hashable     HandshakeState
instance NFData       HandshakeState
instance ToByteString HandshakeState
instance ToQuery      HandshakeState
instance ToHeader     HandshakeState

instance FromJSON HandshakeState where
    parseJSON = parseJSONText "HandshakeState"

data IAMUserAccessToBilling
  = Allow
  | Deny
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IAMUserAccessToBilling where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "deny" -> pure Deny
        e -> fromTextError $ "Failure parsing IAMUserAccessToBilling from value: '" <> e
           <> "'. Accepted values: allow, deny"

instance ToText IAMUserAccessToBilling where
    toText = \case
        Allow -> "ALLOW"
        Deny -> "DENY"

instance Hashable     IAMUserAccessToBilling
instance NFData       IAMUserAccessToBilling
instance ToByteString IAMUserAccessToBilling
instance ToQuery      IAMUserAccessToBilling
instance ToHeader     IAMUserAccessToBilling

instance ToJSON IAMUserAccessToBilling where
    toJSON = toJSONText

data OrganizationFeatureSet
  = All
  | ConsolidatedBilling
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrganizationFeatureSet where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "consolidated_billing" -> pure ConsolidatedBilling
        e -> fromTextError $ "Failure parsing OrganizationFeatureSet from value: '" <> e
           <> "'. Accepted values: all, consolidated_billing"

instance ToText OrganizationFeatureSet where
    toText = \case
        All -> "ALL"
        ConsolidatedBilling -> "CONSOLIDATED_BILLING"

instance Hashable     OrganizationFeatureSet
instance NFData       OrganizationFeatureSet
instance ToByteString OrganizationFeatureSet
instance ToQuery      OrganizationFeatureSet
instance ToHeader     OrganizationFeatureSet

instance ToJSON OrganizationFeatureSet where
    toJSON = toJSONText

instance FromJSON OrganizationFeatureSet where
    parseJSON = parseJSONText "OrganizationFeatureSet"

data ParentType
  = OrganizationalUnit
  | Root
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ParentType where
    parser = takeLowerText >>= \case
        "organizational_unit" -> pure OrganizationalUnit
        "root" -> pure Root
        e -> fromTextError $ "Failure parsing ParentType from value: '" <> e
           <> "'. Accepted values: organizational_unit, root"

instance ToText ParentType where
    toText = \case
        OrganizationalUnit -> "ORGANIZATIONAL_UNIT"
        Root -> "ROOT"

instance Hashable     ParentType
instance NFData       ParentType
instance ToByteString ParentType
instance ToQuery      ParentType
instance ToHeader     ParentType

instance FromJSON ParentType where
    parseJSON = parseJSONText "ParentType"

data PolicyType =
  ServiceControlPolicy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PolicyType where
    parser = takeLowerText >>= \case
        "service_control_policy" -> pure ServiceControlPolicy
        e -> fromTextError $ "Failure parsing PolicyType from value: '" <> e
           <> "'. Accepted values: service_control_policy"

instance ToText PolicyType where
    toText = \case
        ServiceControlPolicy -> "SERVICE_CONTROL_POLICY"

instance Hashable     PolicyType
instance NFData       PolicyType
instance ToByteString PolicyType
instance ToQuery      PolicyType
instance ToHeader     PolicyType

instance ToJSON PolicyType where
    toJSON = toJSONText

instance FromJSON PolicyType where
    parseJSON = parseJSONText "PolicyType"

data PolicyTypeStatus
  = Enabled
  | PendingDisable
  | PendingEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PolicyTypeStatus where
    parser = takeLowerText >>= \case
        "enabled" -> pure Enabled
        "pending_disable" -> pure PendingDisable
        "pending_enable" -> pure PendingEnable
        e -> fromTextError $ "Failure parsing PolicyTypeStatus from value: '" <> e
           <> "'. Accepted values: enabled, pending_disable, pending_enable"

instance ToText PolicyTypeStatus where
    toText = \case
        Enabled -> "ENABLED"
        PendingDisable -> "PENDING_DISABLE"
        PendingEnable -> "PENDING_ENABLE"

instance Hashable     PolicyTypeStatus
instance NFData       PolicyTypeStatus
instance ToByteString PolicyTypeStatus
instance ToQuery      PolicyTypeStatus
instance ToHeader     PolicyTypeStatus

instance FromJSON PolicyTypeStatus where
    parseJSON = parseJSONText "PolicyTypeStatus"

data TargetType
  = TTAccount
  | TTOrganizationalUnit
  | TTRoot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TargetType where
    parser = takeLowerText >>= \case
        "account" -> pure TTAccount
        "organizational_unit" -> pure TTOrganizationalUnit
        "root" -> pure TTRoot
        e -> fromTextError $ "Failure parsing TargetType from value: '" <> e
           <> "'. Accepted values: account, organizational_unit, root"

instance ToText TargetType where
    toText = \case
        TTAccount -> "ACCOUNT"
        TTOrganizationalUnit -> "ORGANIZATIONAL_UNIT"
        TTRoot -> "ROOT"

instance Hashable     TargetType
instance NFData       TargetType
instance ToByteString TargetType
instance ToQuery      TargetType
instance ToHeader     TargetType

instance FromJSON TargetType where
    parseJSON = parseJSONText "TargetType"
