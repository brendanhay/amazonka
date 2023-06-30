{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Organizations.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AWSOrganizationsNotInUseException,
    _AccessDeniedException,
    _AccessDeniedForDependencyException,
    _AccountAlreadyClosedException,
    _AccountAlreadyRegisteredException,
    _AccountNotFoundException,
    _AccountNotRegisteredException,
    _AccountOwnerNotVerifiedException,
    _AlreadyInOrganizationException,
    _ChildNotFoundException,
    _ConcurrentModificationException,
    _ConflictException,
    _ConstraintViolationException,
    _CreateAccountStatusNotFoundException,
    _DestinationParentNotFoundException,
    _DuplicateAccountException,
    _DuplicateHandshakeException,
    _DuplicateOrganizationalUnitException,
    _DuplicatePolicyAttachmentException,
    _DuplicatePolicyException,
    _EffectivePolicyNotFoundException,
    _FinalizingOrganizationException,
    _HandshakeAlreadyInStateException,
    _HandshakeConstraintViolationException,
    _HandshakeNotFoundException,
    _InvalidHandshakeTransitionException,
    _InvalidInputException,
    _MalformedPolicyDocumentException,
    _MasterCannotLeaveOrganizationException,
    _OrganizationNotEmptyException,
    _OrganizationalUnitNotEmptyException,
    _OrganizationalUnitNotFoundException,
    _ParentNotFoundException,
    _PolicyChangesInProgressException,
    _PolicyInUseException,
    _PolicyNotAttachedException,
    _PolicyNotFoundException,
    _PolicyTypeAlreadyEnabledException,
    _PolicyTypeNotAvailableForOrganizationException,
    _PolicyTypeNotEnabledException,
    _ResourcePolicyNotFoundException,
    _RootNotFoundException,
    _ServiceException,
    _SourceParentNotFoundException,
    _TargetNotFoundException,
    _TooManyRequestsException,
    _UnsupportedAPIEndpointException,

    -- * AccountJoinedMethod
    AccountJoinedMethod (..),

    -- * AccountStatus
    AccountStatus (..),

    -- * ActionType
    ActionType (..),

    -- * ChildType
    ChildType (..),

    -- * CreateAccountFailureReason
    CreateAccountFailureReason (..),

    -- * CreateAccountState
    CreateAccountState (..),

    -- * EffectivePolicyType
    EffectivePolicyType (..),

    -- * HandshakePartyType
    HandshakePartyType (..),

    -- * HandshakeResourceType
    HandshakeResourceType (..),

    -- * HandshakeState
    HandshakeState (..),

    -- * IAMUserAccessToBilling
    IAMUserAccessToBilling (..),

    -- * OrganizationFeatureSet
    OrganizationFeatureSet (..),

    -- * ParentType
    ParentType (..),

    -- * PolicyType
    PolicyType (..),

    -- * PolicyTypeStatus
    PolicyTypeStatus (..),

    -- * TargetType
    TargetType (..),

    -- * Account
    Account (..),
    newAccount,
    account_arn,
    account_email,
    account_id,
    account_joinedMethod,
    account_joinedTimestamp,
    account_name,
    account_status,

    -- * Child
    Child (..),
    newChild,
    child_id,
    child_type,

    -- * CreateAccountStatus
    CreateAccountStatus (..),
    newCreateAccountStatus,
    createAccountStatus_accountId,
    createAccountStatus_accountName,
    createAccountStatus_completedTimestamp,
    createAccountStatus_failureReason,
    createAccountStatus_govCloudAccountId,
    createAccountStatus_id,
    createAccountStatus_requestedTimestamp,
    createAccountStatus_state,

    -- * DelegatedAdministrator
    DelegatedAdministrator (..),
    newDelegatedAdministrator,
    delegatedAdministrator_arn,
    delegatedAdministrator_delegationEnabledDate,
    delegatedAdministrator_email,
    delegatedAdministrator_id,
    delegatedAdministrator_joinedMethod,
    delegatedAdministrator_joinedTimestamp,
    delegatedAdministrator_name,
    delegatedAdministrator_status,

    -- * DelegatedService
    DelegatedService (..),
    newDelegatedService,
    delegatedService_delegationEnabledDate,
    delegatedService_servicePrincipal,

    -- * EffectivePolicy
    EffectivePolicy (..),
    newEffectivePolicy,
    effectivePolicy_lastUpdatedTimestamp,
    effectivePolicy_policyContent,
    effectivePolicy_policyType,
    effectivePolicy_targetId,

    -- * EnabledServicePrincipal
    EnabledServicePrincipal (..),
    newEnabledServicePrincipal,
    enabledServicePrincipal_dateEnabled,
    enabledServicePrincipal_servicePrincipal,

    -- * Handshake
    Handshake (..),
    newHandshake,
    handshake_action,
    handshake_arn,
    handshake_expirationTimestamp,
    handshake_id,
    handshake_parties,
    handshake_requestedTimestamp,
    handshake_resources,
    handshake_state,

    -- * HandshakeFilter
    HandshakeFilter (..),
    newHandshakeFilter,
    handshakeFilter_actionType,
    handshakeFilter_parentHandshakeId,

    -- * HandshakeParty
    HandshakeParty (..),
    newHandshakeParty,
    handshakeParty_id,
    handshakeParty_type,

    -- * HandshakeResource
    HandshakeResource (..),
    newHandshakeResource,
    handshakeResource_resources,
    handshakeResource_type,
    handshakeResource_value,

    -- * Organization
    Organization (..),
    newOrganization,
    organization_arn,
    organization_availablePolicyTypes,
    organization_featureSet,
    organization_id,
    organization_masterAccountArn,
    organization_masterAccountEmail,
    organization_masterAccountId,

    -- * OrganizationalUnit
    OrganizationalUnit (..),
    newOrganizationalUnit,
    organizationalUnit_arn,
    organizationalUnit_id,
    organizationalUnit_name,

    -- * Parent
    Parent (..),
    newParent,
    parent_id,
    parent_type,

    -- * Policy
    Policy (..),
    newPolicy,
    policy_content,
    policy_policySummary,

    -- * PolicySummary
    PolicySummary (..),
    newPolicySummary,
    policySummary_arn,
    policySummary_awsManaged,
    policySummary_description,
    policySummary_id,
    policySummary_name,
    policySummary_type,

    -- * PolicyTargetSummary
    PolicyTargetSummary (..),
    newPolicyTargetSummary,
    policyTargetSummary_arn,
    policyTargetSummary_name,
    policyTargetSummary_targetId,
    policyTargetSummary_type,

    -- * PolicyTypeSummary
    PolicyTypeSummary (..),
    newPolicyTypeSummary,
    policyTypeSummary_status,
    policyTypeSummary_type,

    -- * ResourcePolicy
    ResourcePolicy (..),
    newResourcePolicy,
    resourcePolicy_content,
    resourcePolicy_resourcePolicySummary,

    -- * ResourcePolicySummary
    ResourcePolicySummary (..),
    newResourcePolicySummary,
    resourcePolicySummary_arn,
    resourcePolicySummary_id,

    -- * Root
    Root (..),
    newRoot,
    root_arn,
    root_id,
    root_name,
    root_policyTypes,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Organizations.Types.Account
import Amazonka.Organizations.Types.AccountJoinedMethod
import Amazonka.Organizations.Types.AccountStatus
import Amazonka.Organizations.Types.ActionType
import Amazonka.Organizations.Types.Child
import Amazonka.Organizations.Types.ChildType
import Amazonka.Organizations.Types.CreateAccountFailureReason
import Amazonka.Organizations.Types.CreateAccountState
import Amazonka.Organizations.Types.CreateAccountStatus
import Amazonka.Organizations.Types.DelegatedAdministrator
import Amazonka.Organizations.Types.DelegatedService
import Amazonka.Organizations.Types.EffectivePolicy
import Amazonka.Organizations.Types.EffectivePolicyType
import Amazonka.Organizations.Types.EnabledServicePrincipal
import Amazonka.Organizations.Types.Handshake
import Amazonka.Organizations.Types.HandshakeFilter
import Amazonka.Organizations.Types.HandshakeParty
import Amazonka.Organizations.Types.HandshakePartyType
import Amazonka.Organizations.Types.HandshakeResource
import Amazonka.Organizations.Types.HandshakeResourceType
import Amazonka.Organizations.Types.HandshakeState
import Amazonka.Organizations.Types.IAMUserAccessToBilling
import Amazonka.Organizations.Types.Organization
import Amazonka.Organizations.Types.OrganizationFeatureSet
import Amazonka.Organizations.Types.OrganizationalUnit
import Amazonka.Organizations.Types.Parent
import Amazonka.Organizations.Types.ParentType
import Amazonka.Organizations.Types.Policy
import Amazonka.Organizations.Types.PolicySummary
import Amazonka.Organizations.Types.PolicyTargetSummary
import Amazonka.Organizations.Types.PolicyType
import Amazonka.Organizations.Types.PolicyTypeStatus
import Amazonka.Organizations.Types.PolicyTypeSummary
import Amazonka.Organizations.Types.ResourcePolicy
import Amazonka.Organizations.Types.ResourcePolicySummary
import Amazonka.Organizations.Types.Root
import Amazonka.Organizations.Types.Tag
import Amazonka.Organizations.Types.TargetType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Organizations SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Organizations",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "organizations",
      Core.signingName = "organizations",
      Core.version = "2016-11-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Organizations",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Your account isn\'t a member of an organization. To make this request,
-- you must use the credentials of an account that belongs to an
-- organization.
_AWSOrganizationsNotInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AWSOrganizationsNotInUseException =
  Core._MatchServiceError
    defaultService
    "AWSOrganizationsNotInUseException"

-- | You don\'t have permissions to perform the requested operation. The user
-- or role that is making the request must have at least one IAM
-- permissions policy attached that grants the required permissions. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access.html Access Management>
-- in the /IAM User Guide./
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The operation that you attempted requires you to have the
-- @iam:CreateServiceLinkedRole@ for @organizations.amazonaws.com@
-- permission so that Organizations can create the required service-linked
-- role. You don\'t have that permission.
_AccessDeniedForDependencyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedForDependencyException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedForDependencyException"

-- | You attempted to close an account that is already closed.
_AccountAlreadyClosedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountAlreadyClosedException =
  Core._MatchServiceError
    defaultService
    "AccountAlreadyClosedException"

-- | The specified account is already a delegated administrator for this
-- Amazon Web Services service.
_AccountAlreadyRegisteredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "AccountAlreadyRegisteredException"

-- | We can\'t find an Amazon Web Services account with the @AccountId@ that
-- you specified, or the account whose credentials you used to make this
-- request isn\'t a member of an organization.
_AccountNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountNotFoundException =
  Core._MatchServiceError
    defaultService
    "AccountNotFoundException"

-- | The specified account is not a delegated administrator for this Amazon
-- Web Services service.
_AccountNotRegisteredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountNotRegisteredException =
  Core._MatchServiceError
    defaultService
    "AccountNotRegisteredException"

-- | You can\'t invite an existing account to your organization until you
-- verify that you own the email address associated with the management
-- account. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_create.html#about-email-verification Email Address Verification>
-- in the /Organizations User Guide./
_AccountOwnerNotVerifiedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountOwnerNotVerifiedException =
  Core._MatchServiceError
    defaultService
    "AccountOwnerNotVerifiedException"

-- | This account is already a member of an organization. An account can
-- belong to only one organization at a time.
_AlreadyInOrganizationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AlreadyInOrganizationException =
  Core._MatchServiceError
    defaultService
    "AlreadyInOrganizationException"

-- | We can\'t find an organizational unit (OU) or Amazon Web Services
-- account with the @ChildId@ that you specified.
_ChildNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ChildNotFoundException =
  Core._MatchServiceError
    defaultService
    "ChildNotFoundException"

-- | The target of the operation is currently being modified by a different
-- request. Try again later.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The request failed because it conflicts with the current state of the
-- specified resource.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Performing this operation violates a minimum or maximum value limit. For
-- example, attempting to remove the last service control policy (SCP) from
-- an OU or root, inviting or creating too many accounts to the
-- organization, or attaching too many policies to an account, OU, or root.
-- This exception includes a reason that contains additional information
-- about the violated limit:
--
-- Some of the reasons in the following list might not be applicable to
-- this specific API or operation.
--
-- -   ACCOUNT_CANNOT_LEAVE_ORGANIZATION: You attempted to remove the
--     management account from the organization. You can\'t remove the
--     management account. Instead, after you remove all member accounts,
--     delete the organization itself.
--
-- -   ACCOUNT_CANNOT_LEAVE_WITHOUT_PHONE_VERIFICATION: You attempted to
--     remove an account from the organization that doesn\'t yet have
--     enough information to exist as a standalone account. This account
--     requires you to first complete phone verification. Follow the steps
--     at
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#orgs_manage_accounts_remove-from-master Removing a member account from your organization>
--     in the /Organizations User Guide./
--
-- -   ACCOUNT_CREATION_RATE_LIMIT_EXCEEDED: You attempted to exceed the
--     number of accounts that you can create in one day.
--
-- -   ACCOUNT_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the limit on
--     the number of accounts in an organization. If you need more
--     accounts, contact
--     <https://docs.aws.amazon.com/support/home#/ Amazon Web Services Support>
--     to request an increase in your limit.
--
--     Or the number of invitations that you tried to send would cause you
--     to exceed the limit of accounts in your organization. Send fewer
--     invitations or contact Amazon Web Services Support to request an
--     increase in the number of accounts.
--
--     Deleted and closed accounts still count toward your limit.
--
--     If you get this exception when running a command immediately after
--     creating the organization, wait one hour and try again. After an
--     hour, if the command continues to fail with this error, contact
--     <https://docs.aws.amazon.com/support/home#/ Amazon Web Services Support>.
--
-- -   CANNOT_REGISTER_MASTER_AS_DELEGATED_ADMINISTRATOR: You attempted to
--     register the management account of the organization as a delegated
--     administrator for an Amazon Web Services service integrated with
--     Organizations. You can designate only a member account as a
--     delegated administrator.
--
-- -   CANNOT_CLOSE_MANAGEMENT_ACCOUNT: You attempted to close the
--     management account. To close the management account for the
--     organization, you must first either remove or close all member
--     accounts in the organization. Follow standard account closure
--     process using root credentials.​
--
-- -   CANNOT_REMOVE_DELEGATED_ADMINISTRATOR_FROM_ORG: You attempted to
--     remove an account that is registered as a delegated administrator
--     for a service integrated with your organization. To complete this
--     operation, you must first deregister this account as a delegated
--     administrator.
--
-- -   CLOSE_ACCOUNT_QUOTA_EXCEEDED: You have exceeded close account quota
--     for the past 30 days.
--
-- -   CLOSE_ACCOUNT_REQUESTS_LIMIT_EXCEEDED: You attempted to exceed the
--     number of accounts that you can close at a time. ​
--
-- -   CREATE_ORGANIZATION_IN_BILLING_MODE_UNSUPPORTED_REGION: To create an
--     organization in the specified region, you must enable all features
--     mode.
--
-- -   DELEGATED_ADMINISTRATOR_EXISTS_FOR_THIS_SERVICE: You attempted to
--     register an Amazon Web Services account as a delegated administrator
--     for an Amazon Web Services service that already has a delegated
--     administrator. To complete this operation, you must first deregister
--     any existing delegated administrators for this service.
--
-- -   EMAIL_VERIFICATION_CODE_EXPIRED: The email verification code is only
--     valid for a limited period of time. You must resubmit the request
--     and generate a new verfication code.
--
-- -   HANDSHAKE_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of
--     handshakes that you can send in one day.
--
-- -   INVALID_PAYMENT_INSTRUMENT: You cannot remove an account because no
--     supported payment method is associated with the account. Amazon Web
--     Services does not support cards issued by financial institutions in
--     Russia or Belarus. For more information, see
--     <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-general.html Managing your Amazon Web Services payments>.
--
-- -   MASTER_ACCOUNT_ADDRESS_DOES_NOT_MATCH_MARKETPLACE: To create an
--     account in this organization, you first must migrate the
--     organization\'s management account to the marketplace that
--     corresponds to the management account\'s address. For example,
--     accounts with India addresses must be associated with the AISPL
--     marketplace. All accounts in an organization must be associated with
--     the same marketplace.
--
-- -   MASTER_ACCOUNT_MISSING_BUSINESS_LICENSE: Applies only to the Amazon
--     Web Services \/> Regions in China. To create an organization, the
--     master must have a valid business license. For more information,
--     contact customer support.
--
-- -   MASTER_ACCOUNT_MISSING_CONTACT_INFO: To complete this operation, you
--     must first provide a valid contact address and phone number for the
--     management account. Then try the operation again.
--
-- -   MASTER_ACCOUNT_NOT_GOVCLOUD_ENABLED: To complete this operation, the
--     management account must have an associated account in the Amazon Web
--     Services GovCloud (US-West) Region. For more information, see
--     <https://docs.aws.amazon.com/govcloud-us/latest/UserGuide/govcloud-organizations.html Organizations>
--     in the /Amazon Web Services GovCloud User Guide./
--
-- -   MASTER_ACCOUNT_PAYMENT_INSTRUMENT_REQUIRED: To create an
--     organization with this management account, you first must associate
--     a valid payment instrument, such as a credit card, with the account.
--     Follow the steps at
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided>
--     in the /Organizations User Guide./
--
-- -   MAX_DELEGATED_ADMINISTRATORS_FOR_SERVICE_LIMIT_EXCEEDED: You
--     attempted to register more delegated administrators than allowed for
--     the service principal.
--
-- -   MAX_POLICY_TYPE_ATTACHMENT_LIMIT_EXCEEDED: You attempted to exceed
--     the number of policies of a certain type that can be attached to an
--     entity at one time.
--
-- -   MAX_TAG_LIMIT_EXCEEDED: You have exceeded the number of tags allowed
--     on this resource.
--
-- -   MEMBER_ACCOUNT_PAYMENT_INSTRUMENT_REQUIRED: To complete this
--     operation with this member account, you first must associate a valid
--     payment instrument, such as a credit card, with the account. Follow
--     the steps at
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided>
--     in the /Organizations User Guide./
--
-- -   MIN_POLICY_TYPE_ATTACHMENT_LIMIT_EXCEEDED: You attempted to detach a
--     policy from an entity that would cause the entity to have fewer than
--     the minimum number of policies of a certain type required.
--
-- -   ORGANIZATION_NOT_IN_ALL_FEATURES_MODE: You attempted to perform an
--     operation that requires the organization to be configured to support
--     all features. An organization that supports only consolidated
--     billing features can\'t perform this operation.
--
-- -   OU_DEPTH_LIMIT_EXCEEDED: You attempted to create an OU tree that is
--     too many levels deep.
--
-- -   OU_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the number of OUs
--     that you can have in an organization.
--
-- -   POLICY_CONTENT_LIMIT_EXCEEDED: You attempted to create a policy that
--     is larger than the maximum size.
--
-- -   POLICY_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the number of
--     policies that you can have in an organization.
--
-- -   SERVICE_ACCESS_NOT_ENABLED: You attempted to register a delegated
--     administrator before you enabled service access. Call the
--     @EnableAWSServiceAccess@ API first.
--
-- -   TAG_POLICY_VIOLATION: You attempted to create or update a resource
--     with tags that are not compliant with the tag policy requirements
--     for this account.
--
-- -   WAIT_PERIOD_ACTIVE: After you create an Amazon Web Services account,
--     there is a waiting period before you can remove it from the
--     organization. If you get an error that indicates that a wait period
--     is required, try again in a few days.
_ConstraintViolationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConstraintViolationException =
  Core._MatchServiceError
    defaultService
    "ConstraintViolationException"

-- | We can\'t find an create account request with the
-- @CreateAccountRequestId@ that you specified.
_CreateAccountStatusNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CreateAccountStatusNotFoundException =
  Core._MatchServiceError
    defaultService
    "CreateAccountStatusNotFoundException"

-- | We can\'t find the destination container (a root or OU) with the
-- @ParentId@ that you specified.
_DestinationParentNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DestinationParentNotFoundException =
  Core._MatchServiceError
    defaultService
    "DestinationParentNotFoundException"

-- | That account is already present in the specified destination.
_DuplicateAccountException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateAccountException =
  Core._MatchServiceError
    defaultService
    "DuplicateAccountException"

-- | A handshake with the same action and target already exists. For example,
-- if you invited an account to join your organization, the invited account
-- might already have a pending invitation from this organization. If you
-- intend to resend an invitation to an account, ensure that existing
-- handshakes that might be considered duplicates are canceled or declined.
_DuplicateHandshakeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateHandshakeException =
  Core._MatchServiceError
    defaultService
    "DuplicateHandshakeException"

-- | An OU with the same name already exists.
_DuplicateOrganizationalUnitException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateOrganizationalUnitException =
  Core._MatchServiceError
    defaultService
    "DuplicateOrganizationalUnitException"

-- | The selected policy is already attached to the specified target.
_DuplicatePolicyAttachmentException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicatePolicyAttachmentException =
  Core._MatchServiceError
    defaultService
    "DuplicatePolicyAttachmentException"

-- | A policy with the same name already exists.
_DuplicatePolicyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicatePolicyException =
  Core._MatchServiceError
    defaultService
    "DuplicatePolicyException"

-- | If you ran this action on the management account, this policy type is
-- not enabled. If you ran the action on a member account, the account
-- doesn\'t have an effective policy of this type. Contact the
-- administrator of your organization about attaching a policy of this type
-- to the account.
_EffectivePolicyNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EffectivePolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "EffectivePolicyNotFoundException"

-- | Organizations couldn\'t perform the operation because your organization
-- hasn\'t finished initializing. This can take up to an hour. Try again
-- later. If after one hour you continue to receive this error, contact
-- <https://console.aws.amazon.com/support/home#/ Amazon Web Services Support>.
_FinalizingOrganizationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FinalizingOrganizationException =
  Core._MatchServiceError
    defaultService
    "FinalizingOrganizationException"

-- | The specified handshake is already in the requested state. For example,
-- you can\'t accept a handshake that was already accepted.
_HandshakeAlreadyInStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HandshakeAlreadyInStateException =
  Core._MatchServiceError
    defaultService
    "HandshakeAlreadyInStateException"

-- | The requested operation would violate the constraint identified in the
-- reason code.
--
-- Some of the reasons in the following list might not be applicable to
-- this specific API or operation:
--
-- -   ACCOUNT_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the limit on
--     the number of accounts in an organization. Note that deleted and
--     closed accounts still count toward your limit.
--
--     If you get this exception immediately after creating the
--     organization, wait one hour and try again. If after an hour it
--     continues to fail with this error, contact
--     <https://docs.aws.amazon.com/support/home#/ Amazon Web Services Support>.
--
-- -   ALREADY_IN_AN_ORGANIZATION: The handshake request is invalid because
--     the invited account is already a member of an organization.
--
-- -   HANDSHAKE_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of
--     handshakes that you can send in one day.
--
-- -   INVITE_DISABLED_DURING_ENABLE_ALL_FEATURES: You can\'t issue new
--     invitations to join an organization while it\'s in the process of
--     enabling all features. You can resume inviting accounts after you
--     finalize the process when all accounts have agreed to the change.
--
-- -   ORGANIZATION_ALREADY_HAS_ALL_FEATURES: The handshake request is
--     invalid because the organization has already enabled all features.
--
-- -   ORGANIZATION_IS_ALREADY_PENDING_ALL_FEATURES_MIGRATION: The
--     handshake request is invalid because the organization has already
--     started the process to enable all features.
--
-- -   ORGANIZATION_FROM_DIFFERENT_SELLER_OF_RECORD: The request failed
--     because the account is from a different marketplace than the
--     accounts in the organization. For example, accounts with India
--     addresses must be associated with the AISPL marketplace. All
--     accounts in an organization must be from the same marketplace.
--
-- -   ORGANIZATION_MEMBERSHIP_CHANGE_RATE_LIMIT_EXCEEDED: You attempted to
--     change the membership of an account too quickly after its previous
--     change.
--
-- -   PAYMENT_INSTRUMENT_REQUIRED: You can\'t complete the operation with
--     an account that doesn\'t have a payment instrument, such as a credit
--     card, associated with it.
_HandshakeConstraintViolationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HandshakeConstraintViolationException =
  Core._MatchServiceError
    defaultService
    "HandshakeConstraintViolationException"

-- | We can\'t find a handshake with the @HandshakeId@ that you specified.
_HandshakeNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HandshakeNotFoundException =
  Core._MatchServiceError
    defaultService
    "HandshakeNotFoundException"

-- | You can\'t perform the operation on the handshake in its current state.
-- For example, you can\'t cancel a handshake that was already accepted or
-- accept a handshake that was already declined.
_InvalidHandshakeTransitionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidHandshakeTransitionException =
  Core._MatchServiceError
    defaultService
    "InvalidHandshakeTransitionException"

-- | The requested operation failed because you provided invalid values for
-- one or more of the request parameters. This exception includes a reason
-- that contains additional information about the violated limit:
--
-- Some of the reasons in the following list might not be applicable to
-- this specific API or operation.
--
-- -   DUPLICATE_TAG_KEY: Tag keys must be unique among the tags attached
--     to the same entity.
--
-- -   IMMUTABLE_POLICY: You specified a policy that is managed by Amazon
--     Web Services and can\'t be modified.
--
-- -   INPUT_REQUIRED: You must include a value for all required
--     parameters.
--
-- -   INVALID_EMAIL_ADDRESS_TARGET: You specified an invalid email address
--     for the invited account owner.
--
-- -   INVALID_ENUM: You specified an invalid value.
--
-- -   INVALID_ENUM_POLICY_TYPE: You specified an invalid policy type
--     string.
--
-- -   INVALID_FULL_NAME_TARGET: You specified a full name that contains
--     invalid characters.
--
-- -   INVALID_LIST_MEMBER: You provided a list to a parameter that
--     contains at least one invalid value.
--
-- -   INVALID_PAGINATION_TOKEN: Get the value for the @NextToken@
--     parameter from the response to a previous call of the operation.
--
-- -   INVALID_PARTY_TYPE_TARGET: You specified the wrong type of entity
--     (account, organization, or email) as a party.
--
-- -   INVALID_PATTERN: You provided a value that doesn\'t match the
--     required pattern.
--
-- -   INVALID_PATTERN_TARGET_ID: You specified a policy target ID that
--     doesn\'t match the required pattern.
--
-- -   INVALID_ROLE_NAME: You provided a role name that isn\'t valid. A
--     role name can\'t begin with the reserved prefix @AWSServiceRoleFor@.
--
-- -   INVALID_SYNTAX_ORGANIZATION_ARN: You specified an invalid Amazon
--     Resource Name (ARN) for the organization.
--
-- -   INVALID_SYNTAX_POLICY_ID: You specified an invalid policy ID.
--
-- -   INVALID_SYSTEM_TAGS_PARAMETER: You specified a tag key that is a
--     system tag. You can’t add, edit, or delete system tag keys because
--     they\'re reserved for Amazon Web Services use. System tags don’t
--     count against your tags per resource limit.
--
-- -   MAX_FILTER_LIMIT_EXCEEDED: You can specify only one filter parameter
--     for the operation.
--
-- -   MAX_LENGTH_EXCEEDED: You provided a string parameter that is longer
--     than allowed.
--
-- -   MAX_VALUE_EXCEEDED: You provided a numeric parameter that has a
--     larger value than allowed.
--
-- -   MIN_LENGTH_EXCEEDED: You provided a string parameter that is shorter
--     than allowed.
--
-- -   MIN_VALUE_EXCEEDED: You provided a numeric parameter that has a
--     smaller value than allowed.
--
-- -   MOVING_ACCOUNT_BETWEEN_DIFFERENT_ROOTS: You can move an account only
--     between entities in the same root.
--
-- -   TARGET_NOT_SUPPORTED: You can\'t perform the specified operation on
--     that target entity.
--
-- -   UNRECOGNIZED_SERVICE_PRINCIPAL: You specified a service principal
--     that isn\'t recognized.
_InvalidInputException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | The provided policy document doesn\'t meet the requirements of the
-- specified policy type. For example, the syntax might be incorrect. For
-- details about service control policy syntax, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax>
-- in the /Organizations User Guide./
_MalformedPolicyDocumentException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MalformedPolicyDocumentException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyDocumentException"

-- | You can\'t remove a management account from an organization. If you want
-- the management account to become a member account in another
-- organization, you must first delete the current organization of the
-- management account.
_MasterCannotLeaveOrganizationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MasterCannotLeaveOrganizationException =
  Core._MatchServiceError
    defaultService
    "MasterCannotLeaveOrganizationException"

-- | The organization isn\'t empty. To delete an organization, you must first
-- remove all accounts except the management account, delete all OUs, and
-- delete all policies.
_OrganizationNotEmptyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OrganizationNotEmptyException =
  Core._MatchServiceError
    defaultService
    "OrganizationNotEmptyException"

-- | The specified OU is not empty. Move all accounts to another root or to
-- other OUs, remove all child OUs, and try the operation again.
_OrganizationalUnitNotEmptyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OrganizationalUnitNotEmptyException =
  Core._MatchServiceError
    defaultService
    "OrganizationalUnitNotEmptyException"

-- | We can\'t find an OU with the @OrganizationalUnitId@ that you specified.
_OrganizationalUnitNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OrganizationalUnitNotFoundException =
  Core._MatchServiceError
    defaultService
    "OrganizationalUnitNotFoundException"

-- | We can\'t find a root or OU with the @ParentId@ that you specified.
_ParentNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ParentNotFoundException =
  Core._MatchServiceError
    defaultService
    "ParentNotFoundException"

-- | Changes to the effective policy are in progress, and its contents can\'t
-- be returned. Try the operation again later.
_PolicyChangesInProgressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicyChangesInProgressException =
  Core._MatchServiceError
    defaultService
    "PolicyChangesInProgressException"

-- | The policy is attached to one or more entities. You must detach it from
-- all roots, OUs, and accounts before performing this operation.
_PolicyInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicyInUseException =
  Core._MatchServiceError
    defaultService
    "PolicyInUseException"

-- | The policy isn\'t attached to the specified target in the specified
-- root.
_PolicyNotAttachedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicyNotAttachedException =
  Core._MatchServiceError
    defaultService
    "PolicyNotAttachedException"

-- | We can\'t find a policy with the @PolicyId@ that you specified.
_PolicyNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "PolicyNotFoundException"

-- | The specified policy type is already enabled in the specified root.
_PolicyTypeAlreadyEnabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicyTypeAlreadyEnabledException =
  Core._MatchServiceError
    defaultService
    "PolicyTypeAlreadyEnabledException"

-- | You can\'t use the specified policy type with the feature set currently
-- enabled for this organization. For example, you can enable SCPs only
-- after you enable all features in the organization. For more information,
-- see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html#enable_policies_on_root Managing Organizations Policies>in
-- the /Organizations User Guide./
_PolicyTypeNotAvailableForOrganizationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicyTypeNotAvailableForOrganizationException =
  Core._MatchServiceError
    defaultService
    "PolicyTypeNotAvailableForOrganizationException"

-- | The specified policy type isn\'t currently enabled in this root. You
-- can\'t attach policies of the specified type to entities in a root until
-- you enable that type in the root. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization>
-- in the /Organizations User Guide./
_PolicyTypeNotEnabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicyTypeNotEnabledException =
  Core._MatchServiceError
    defaultService
    "PolicyTypeNotEnabledException"

-- | We can\'t find a resource policy request with the parameter that you
-- specified.
_ResourcePolicyNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourcePolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourcePolicyNotFoundException"

-- | We can\'t find a root with the @RootId@ that you specified.
_RootNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RootNotFoundException =
  Core._MatchServiceError
    defaultService
    "RootNotFoundException"

-- | Organizations can\'t complete your request because of an internal
-- service error. Try again later.
_ServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"

-- | We can\'t find a source root or OU with the @ParentId@ that you
-- specified.
_SourceParentNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SourceParentNotFoundException =
  Core._MatchServiceError
    defaultService
    "SourceParentNotFoundException"

-- | We can\'t find a root, OU, account, or policy with the @TargetId@ that
-- you specified.
_TargetNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TargetNotFoundException =
  Core._MatchServiceError
    defaultService
    "TargetNotFoundException"

-- | You have sent too many requests in too short a period of time. The quota
-- helps protect against denial-of-service attacks. Try again later.
--
-- For information about quotas that affect Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_limits.html Quotas for Organizations>in
-- the /Organizations User Guide./
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | This action isn\'t available in the current Amazon Web Services Region.
_UnsupportedAPIEndpointException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedAPIEndpointException =
  Core._MatchServiceError
    defaultService
    "UnsupportedAPIEndpointException"
