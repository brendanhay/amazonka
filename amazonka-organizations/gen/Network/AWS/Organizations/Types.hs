{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types
    (
    -- * Service Configuration
      organizations

    -- * Errors
    , _PolicyNotFoundException
    , _PolicyTypeAlreadyEnabledException
    , _HandshakeConstraintViolationException
    , _AccessDeniedException
    , _MalformedPolicyDocumentException
    , _RootNotFoundException
    , _MasterCannotLeaveOrganizationException
    , _AccountNotFoundException
    , _DuplicatePolicyException
    , _ConstraintViolationException
    , _FinalizingOrganizationException
    , _HandshakeNotFoundException
    , _PolicyTypeNotAvailableForOrganizationException
    , _ChildNotFoundException
    , _OrganizationalUnitNotFoundException
    , _DestinationParentNotFoundException
    , _OrganizationNotEmptyException
    , _PolicyTypeNotEnabledException
    , _DuplicateHandshakeException
    , _OrganizationalUnitNotEmptyException
    , _TooManyRequestsException
    , _ConcurrentModificationException
    , _ServiceException
    , _SourceParentNotFoundException
    , _TargetNotFoundException
    , _CreateAccountStatusNotFoundException
    , _AlreadyInOrganizationException
    , _DuplicateOrganizationalUnitException
    , _InvalidInputException
    , _PolicyNotAttachedException
    , _ParentNotFoundException
    , _AWSOrganizationsNotInUseException
    , _PolicyInUseException
    , _InvalidHandshakeTransitionException
    , _HandshakeAlreadyInStateException
    , _DuplicateAccountException
    , _DuplicatePolicyAttachmentException

    -- * AccountJoinedMethod
    , AccountJoinedMethod (..)

    -- * AccountStatus
    , AccountStatus (..)

    -- * ActionType
    , ActionType (..)

    -- * ChildType
    , ChildType (..)

    -- * CreateAccountFailureReason
    , CreateAccountFailureReason (..)

    -- * CreateAccountState
    , CreateAccountState (..)

    -- * HandshakePartyType
    , HandshakePartyType (..)

    -- * HandshakeResourceType
    , HandshakeResourceType (..)

    -- * HandshakeState
    , HandshakeState (..)

    -- * IAMUserAccessToBilling
    , IAMUserAccessToBilling (..)

    -- * OrganizationFeatureSet
    , OrganizationFeatureSet (..)

    -- * ParentType
    , ParentType (..)

    -- * PolicyType
    , PolicyType (..)

    -- * PolicyTypeStatus
    , PolicyTypeStatus (..)

    -- * TargetType
    , TargetType (..)

    -- * Account
    , Account
    , account
    , aStatus
    , aJoinedMethod
    , aARN
    , aJoinedTimestamp
    , aName
    , aId

    -- * Child
    , Child
    , child
    , cId
    , cType

    -- * CreateAccountStatus
    , CreateAccountStatus
    , createAccountStatus
    , casFailureReason
    , casState
    , casCompletedTimestamp
    , casAccountName
    , casAccountId
    , casId
    , casRequestedTimestamp

    -- * Handshake
    , Handshake
    , handshake
    , hState
    , hARN
    , hAction
    , hResources
    , hId
    , hExpirationTimestamp
    , hParties
    , hRequestedTimestamp

    -- * HandshakeFilter
    , HandshakeFilter
    , handshakeFilter
    , hfParentHandshakeId
    , hfActionType

    -- * HandshakeParty
    , HandshakeParty
    , handshakeParty
    , hpId
    , hpType

    -- * HandshakeResource
    , HandshakeResource
    , handshakeResource
    , hrValue
    , hrResources
    , hrType

    -- * Organization
    , Organization
    , organization
    , oARN
    , oMasterAccountId
    , oMasterAccountARN
    , oMasterAccountEmail
    , oAvailablePolicyTypes
    , oId
    , oFeatureSet

    -- * OrganizationalUnit
    , OrganizationalUnit
    , organizationalUnit
    , ouARN
    , ouName
    , ouId

    -- * Parent
    , Parent
    , parent
    , pId
    , pType

    -- * Policy
    , Policy
    , policy
    , pContent
    , pPolicySummary

    -- * PolicySummary
    , PolicySummary
    , policySummary
    , psARN
    , psName
    , psId
    , psAWSManaged
    , psType
    , psDescription

    -- * PolicyTargetSummary
    , PolicyTargetSummary
    , policyTargetSummary
    , polTargetId
    , polARN
    , polName
    , polType

    -- * PolicyTypeSummary
    , PolicyTypeSummary
    , policyTypeSummary
    , ptsStatus
    , ptsType

    -- * Root
    , Root
    , root
    , rARN
    , rName
    , rId
    , rPolicyTypes
    ) where

import           Network.AWS.Lens
import           Network.AWS.Organizations.Types.Product
import           Network.AWS.Organizations.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2016-11-28@ of the Amazon Organizations SDK configuration.
organizations :: Service
organizations =
    Service
    { _svcAbbrev = "Organizations"
    , _svcSigner = v4
    , _svcPrefix = "organizations"
    , _svcVersion = "2016-11-28"
    , _svcEndpoint = defaultEndpoint organizations
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Organizations"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | We can't find a policy with the PolicyId that you specified.
--
--
_PolicyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyNotFoundException =
    _MatchServiceError organizations "PolicyNotFoundException"

-- | The specified policy type is already enabled in the specified root.
--
--
_PolicyTypeAlreadyEnabledException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyTypeAlreadyEnabledException =
    _MatchServiceError organizations "PolicyTypeAlreadyEnabledException"

-- | The requested operation would violate the constraint identified in the reason code.
--
--
_HandshakeConstraintViolationException :: AsError a => Getting (First ServiceError) a ServiceError
_HandshakeConstraintViolationException =
    _MatchServiceError organizations "HandshakeConstraintViolationException"

-- | You don't have permissions to perform the requested operation. The user or role that is making the request must have at least one IAM permissions policy attached that grants the required permissions. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/access.html Access Management> in the /IAM User Guide/ .
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException =
    _MatchServiceError organizations "AccessDeniedException"

-- | The provided policy document does not meet the requirements of the specified policy type. For example, the syntax might be incorrect. For details about service control policy syntax, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax> in the /AWS Organizations User Guide/ .
--
--
_MalformedPolicyDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
    _MatchServiceError organizations "MalformedPolicyDocumentException"

-- | We can't find a root with the RootId that you specified.
--
--
_RootNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_RootNotFoundException =
    _MatchServiceError organizations "RootNotFoundException"

-- | You can't remove a master account from an organization. If you want the master account to become a member account in another organization, you must first delete the current organization of the master account.
--
--
_MasterCannotLeaveOrganizationException :: AsError a => Getting (First ServiceError) a ServiceError
_MasterCannotLeaveOrganizationException =
    _MatchServiceError organizations "MasterCannotLeaveOrganizationException"

-- | We can't find an AWS account with the AccountId that you specified, or the account whose credentials you used to make this request is not a member of an organization.
--
--
_AccountNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_AccountNotFoundException =
    _MatchServiceError organizations "AccountNotFoundException"

-- | A policy with the same name already exists.
--
--
_DuplicatePolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicatePolicyException =
    _MatchServiceError organizations "DuplicatePolicyException"

-- | Performing this operation violates a minimum or maximum value limit. For example, attempting to removing the last SCP from an OU or root, inviting or creating too many accounts to the organization, or attaching too many policies to an account, OU, or root.
--
--
_ConstraintViolationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConstraintViolationException =
    _MatchServiceError organizations "ConstraintViolationException"

-- | AWS Organizations could not finalize the creation of your organization. Try again later. If this persists, contact AWS customer support.
--
--
_FinalizingOrganizationException :: AsError a => Getting (First ServiceError) a ServiceError
_FinalizingOrganizationException =
    _MatchServiceError organizations "FinalizingOrganizationException"

-- | We can't find a handshake with the HandshakeId that you specified.
--
--
_HandshakeNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_HandshakeNotFoundException =
    _MatchServiceError organizations "HandshakeNotFoundException"

-- | You can't use the specified policy type with the feature set currently enabled for this organization. For example, you can enable service control policies (SCPs) only after you enable all features in the organization. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html#enable_policies_on_root Enabling and Disabling a Policy Type on a Root> in the /AWS Organizations User Guide/ .
--
--
_PolicyTypeNotAvailableForOrganizationException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyTypeNotAvailableForOrganizationException =
    _MatchServiceError
        organizations
        "PolicyTypeNotAvailableForOrganizationException"

-- | We can't find an organizational unit (OU) or AWS account with the ChildId that you specified.
--
--
_ChildNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ChildNotFoundException =
    _MatchServiceError organizations "ChildNotFoundException"

-- | We can't find an organizational unit (OU) with the OrganizationalUnitId that you specified.
--
--
_OrganizationalUnitNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationalUnitNotFoundException =
    _MatchServiceError organizations "OrganizationalUnitNotFoundException"

-- | We can't find the destination container (a root or OU) with the ParentId that you specified.
--
--
_DestinationParentNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_DestinationParentNotFoundException =
    _MatchServiceError organizations "DestinationParentNotFoundException"

-- | The organization isn't empty. To delete an organization, you must first remove all accounts except the master account, delete all organizational units (OUs), and delete all policies.
--
--
_OrganizationNotEmptyException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationNotEmptyException =
    _MatchServiceError organizations "OrganizationNotEmptyException"

-- | The specified policy type is not currently enabled in this root. You cannot attach policies of the specified type to entities in a root until you enable that type in the root. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
--
--
_PolicyTypeNotEnabledException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyTypeNotEnabledException =
    _MatchServiceError organizations "PolicyTypeNotEnabledException"

-- | A handshake with the same action and target already exists. For example, if you invited an account to join your organization, the invited account might already have a pending invitation from this organization. If you intend to resend an invitation to an account, ensure that existing handshakes that might be considered duplicates are canceled or declined.
--
--
_DuplicateHandshakeException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateHandshakeException =
    _MatchServiceError organizations "DuplicateHandshakeException"

-- | The specified organizational unit (OU) is not empty. Move all accounts to another root or to other OUs, remove all child OUs, and then try the operation again.
--
--
_OrganizationalUnitNotEmptyException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationalUnitNotEmptyException =
    _MatchServiceError organizations "OrganizationalUnitNotEmptyException"

-- | You've sent too many requests in too short a period of time. The limit helps protect against denial-of-service attacks. Try again later.
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _MatchServiceError organizations "TooManyRequestsException"

-- | The target of the operation is currently being modified by a different request. Try again later.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
    _MatchServiceError organizations "ConcurrentModificationException"

-- | AWS Organizations can't complete your request because of an internal service error. Try again later.
--
--
_ServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceException = _MatchServiceError organizations "ServiceException"

-- | We can't find a source root or OU with the ParentId that you specified.
--
--
_SourceParentNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_SourceParentNotFoundException =
    _MatchServiceError organizations "SourceParentNotFoundException"

-- | We can't find a root, OU, or account with the TargetId that you specified.
--
--
_TargetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TargetNotFoundException =
    _MatchServiceError organizations "TargetNotFoundException"

-- | We can't find an create account request with the CreateAccountRequestId that you specified.
--
--
_CreateAccountStatusNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_CreateAccountStatusNotFoundException =
    _MatchServiceError organizations "CreateAccountStatusNotFoundException"

-- | This account is already a member of an organization. An account can belong to only one organization at a time.
--
--
_AlreadyInOrganizationException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyInOrganizationException =
    _MatchServiceError organizations "AlreadyInOrganizationException"

-- | An organizational unit (OU) with the same name already exists.
--
--
_DuplicateOrganizationalUnitException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateOrganizationalUnitException =
    _MatchServiceError organizations "DuplicateOrganizationalUnitException"

-- | You provided invalid values for one or more of the request parameters.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException =
    _MatchServiceError organizations "InvalidInputException"

-- | The policy isn't attached to the specified target in the specified root.
--
--
_PolicyNotAttachedException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyNotAttachedException =
    _MatchServiceError organizations "PolicyNotAttachedException"

-- | We can't find a root or organizational unit (OU) with the ParentId that you specified.
--
--
_ParentNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ParentNotFoundException =
    _MatchServiceError organizations "ParentNotFoundException"

-- | Your account is not a member of an organization. To make this request, you must use the credentials of an account that belongs to an organization.
--
--
_AWSOrganizationsNotInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_AWSOrganizationsNotInUseException =
    _MatchServiceError organizations "AWSOrganizationsNotInUseException"

-- | The policy is attached to one or more entities. You must detach it from all roots, organizational units (OUs), and accounts before performing this operation.
--
--
_PolicyInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyInUseException = _MatchServiceError organizations "PolicyInUseException"

-- | You can't perform the operation on the handshake in its current state. For example, you can't cancel a handshake that was already accepted, or accept a handshake that was already declined.
--
--
_InvalidHandshakeTransitionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidHandshakeTransitionException =
    _MatchServiceError organizations "InvalidHandshakeTransitionException"

-- | The specified handshake is already in the requested state. For example, you can't accept a handshake that was already accepted.
--
--
_HandshakeAlreadyInStateException :: AsError a => Getting (First ServiceError) a ServiceError
_HandshakeAlreadyInStateException =
    _MatchServiceError organizations "HandshakeAlreadyInStateException"

-- | That account is already present in the specified destination.
--
--
_DuplicateAccountException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateAccountException =
    _MatchServiceError organizations "DuplicateAccountException"

-- | The selected policy is already attached to the specified target.
--
--
_DuplicatePolicyAttachmentException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicatePolicyAttachmentException =
    _MatchServiceError organizations "DuplicatePolicyAttachmentException"
