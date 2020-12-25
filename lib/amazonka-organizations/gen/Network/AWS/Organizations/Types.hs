-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _PolicyNotFoundException,
    _PolicyTypeAlreadyEnabledException,
    _HandshakeConstraintViolationException,
    _AccessDeniedException,
    _MalformedPolicyDocumentException,
    _RootNotFoundException,
    _MasterCannotLeaveOrganizationException,
    _AccountNotFoundException,
    _AccountAlreadyRegisteredException,
    _DuplicatePolicyException,
    _ConstraintViolationException,
    _AccountNotRegisteredException,
    _FinalizingOrganizationException,
    _HandshakeNotFoundException,
    _PolicyTypeNotAvailableForOrganizationException,
    _ChildNotFoundException,
    _UnsupportedAPIEndpointException,
    _EffectivePolicyNotFoundException,
    _OrganizationalUnitNotFoundException,
    _DestinationParentNotFoundException,
    _OrganizationNotEmptyException,
    _AccountOwnerNotVerifiedException,
    _PolicyTypeNotEnabledException,
    _DuplicateHandshakeException,
    _OrganizationalUnitNotEmptyException,
    _TooManyRequestsException,
    _ConcurrentModificationException,
    _ServiceException,
    _SourceParentNotFoundException,
    _TargetNotFoundException,
    _CreateAccountStatusNotFoundException,
    _AlreadyInOrganizationException,
    _DuplicateOrganizationalUnitException,
    _InvalidInputException,
    _PolicyChangesInProgressException,
    _PolicyNotAttachedException,
    _ParentNotFoundException,
    _AccessDeniedForDependencyException,
    _AWSOrganizationsNotInUseException,
    _PolicyInUseException,
    _InvalidHandshakeTransitionException,
    _HandshakeAlreadyInStateException,
    _DuplicateAccountException,
    _DuplicatePolicyAttachmentException,

    -- * Parent
    Parent (..),
    mkParent,
    pId,
    pType,

    -- * HandshakePartyId
    HandshakePartyId (..),

    -- * PolicyTypeSummary
    PolicyTypeSummary (..),
    mkPolicyTypeSummary,
    ptsStatus,
    ptsType,

    -- * GenericArn
    GenericArn (..),

    -- * HandshakeNotes
    HandshakeNotes (..),

    -- * Email
    Email (..),

    -- * PolicyTypeStatus
    PolicyTypeStatus (..),

    -- * Handshake
    Handshake (..),
    mkHandshake,
    hAction,
    hArn,
    hExpirationTimestamp,
    hId,
    hParties,
    hRequestedTimestamp,
    hResources,
    hState,

    -- * ServicePrincipal
    ServicePrincipal (..),

    -- * PolicyName
    PolicyName (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * EnabledServicePrincipal
    EnabledServicePrincipal (..),
    mkEnabledServicePrincipal,
    espDateEnabled,
    espServicePrincipal,

    -- * Child
    Child (..),
    mkChild,
    cId,
    cType,

    -- * CreateAccountState
    CreateAccountState (..),

    -- * EffectivePolicy
    EffectivePolicy (..),
    mkEffectivePolicy,
    epLastUpdatedTimestamp,
    epPolicyContent,
    epPolicyType,
    epTargetId,

    -- * AccountJoinedMethod
    AccountJoinedMethod (..),

    -- * AccountStatus
    AccountStatus (..),

    -- * PolicyId
    PolicyId (..),

    -- * HandshakeResourceType
    HandshakeResourceType (..),

    -- * OrganizationalUnitArn
    OrganizationalUnitArn (..),

    -- * OrganizationalUnit
    OrganizationalUnit (..),
    mkOrganizationalUnit,
    ouArn,
    ouId,
    ouName,

    -- * IAMUserAccessToBilling
    IAMUserAccessToBilling (..),

    -- * PolicyType
    PolicyType (..),

    -- * RootId
    RootId (..),

    -- * RoleName
    RoleName (..),

    -- * AccountName
    AccountName (..),

    -- * Root
    Root (..),
    mkRoot,
    rArn,
    rId,
    rName,
    rPolicyTypes,

    -- * DelegatedService
    DelegatedService (..),
    mkDelegatedService,
    dsDelegationEnabledDate,
    dsServicePrincipal,

    -- * PolicyDescription
    PolicyDescription (..),

    -- * HandshakePartyType
    HandshakePartyType (..),

    -- * CreateAccountRequestId
    CreateAccountRequestId (..),

    -- * TargetType
    TargetType (..),

    -- * CreateAccountFailureReason
    CreateAccountFailureReason (..),

    -- * Account
    Account (..),
    mkAccount,
    aArn,
    aEmail,
    aId,
    aJoinedMethod,
    aJoinedTimestamp,
    aName,
    aStatus,

    -- * AccountId
    AccountId (..),

    -- * PolicyTargetId
    PolicyTargetId (..),

    -- * NextToken
    NextToken (..),

    -- * ParentType
    ParentType (..),

    -- * AccountArn
    AccountArn (..),

    -- * PolicySummary
    PolicySummary (..),
    mkPolicySummary,
    psArn,
    psAwsManaged,
    psDescription,
    psId,
    psName,
    psType,

    -- * HandshakeFilter
    HandshakeFilter (..),
    mkHandshakeFilter,
    hfActionType,
    hfParentHandshakeId,

    -- * OrganizationalUnitName
    OrganizationalUnitName (..),

    -- * ChildId
    ChildId (..),

    -- * ChildType
    ChildType (..),

    -- * HandshakeId
    HandshakeId (..),

    -- * PolicyTargetSummary
    PolicyTargetSummary (..),
    mkPolicyTargetSummary,
    ptsfArn,
    ptsfName,
    ptsfTargetId,
    ptsfType,

    -- * HandshakeResource
    HandshakeResource (..),
    mkHandshakeResource,
    hrResources,
    hrType,
    hrValue,

    -- * EffectivePolicyType
    EffectivePolicyType (..),

    -- * OrganizationalUnitId
    OrganizationalUnitId (..),

    -- * OrganizationFeatureSet
    OrganizationFeatureSet (..),

    -- * TagKey
    TagKey (..),

    -- * Policy
    Policy (..),
    mkPolicy,
    pContent,
    pPolicySummary,

    -- * DelegatedAdministrator
    DelegatedAdministrator (..),
    mkDelegatedAdministrator,
    daArn,
    daDelegationEnabledDate,
    daEmail,
    daId,
    daJoinedMethod,
    daJoinedTimestamp,
    daName,
    daStatus,

    -- * HandshakeState
    HandshakeState (..),

    -- * TaggableResourceId
    TaggableResourceId (..),

    -- * Organization
    Organization (..),
    mkOrganization,
    oArn,
    oAvailablePolicyTypes,
    oFeatureSet,
    oId,
    oMasterAccountArn,
    oMasterAccountEmail,
    oMasterAccountId,

    -- * ActionType
    ActionType (..),

    -- * HandshakeParty
    HandshakeParty (..),
    mkHandshakeParty,
    hpId,
    hpType,

    -- * ParentId
    ParentId (..),

    -- * PolicyContent
    PolicyContent (..),

    -- * CreateAccountStatus
    CreateAccountStatus (..),
    mkCreateAccountStatus,
    casAccountId,
    casAccountName,
    casCompletedTimestamp,
    casFailureReason,
    casGovCloudAccountId,
    casId,
    casRequestedTimestamp,
    casState,

    -- * Id
    Id (..),

    -- * Arn
    Arn (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * TargetId
    TargetId (..),

    -- * Name
    Name (..),

    -- * Content
    Content (..),

    -- * Description
    Description (..),

    -- * ResourceId
    ResourceId (..),

    -- * ParentHandshakeId
    ParentHandshakeId (..),

    -- * SourceParentId
    SourceParentId (..),

    -- * DestinationParentId
    DestinationParentId (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.Account
import Network.AWS.Organizations.Types.AccountArn
import Network.AWS.Organizations.Types.AccountId
import Network.AWS.Organizations.Types.AccountJoinedMethod
import Network.AWS.Organizations.Types.AccountName
import Network.AWS.Organizations.Types.AccountStatus
import Network.AWS.Organizations.Types.ActionType
import Network.AWS.Organizations.Types.Arn
import Network.AWS.Organizations.Types.Child
import Network.AWS.Organizations.Types.ChildId
import Network.AWS.Organizations.Types.ChildType
import Network.AWS.Organizations.Types.Content
import Network.AWS.Organizations.Types.CreateAccountFailureReason
import Network.AWS.Organizations.Types.CreateAccountRequestId
import Network.AWS.Organizations.Types.CreateAccountState
import Network.AWS.Organizations.Types.CreateAccountStatus
import Network.AWS.Organizations.Types.DelegatedAdministrator
import Network.AWS.Organizations.Types.DelegatedService
import Network.AWS.Organizations.Types.Description
import Network.AWS.Organizations.Types.DestinationParentId
import Network.AWS.Organizations.Types.EffectivePolicy
import Network.AWS.Organizations.Types.EffectivePolicyType
import Network.AWS.Organizations.Types.Email
import Network.AWS.Organizations.Types.EnabledServicePrincipal
import Network.AWS.Organizations.Types.GenericArn
import Network.AWS.Organizations.Types.Handshake
import Network.AWS.Organizations.Types.HandshakeFilter
import Network.AWS.Organizations.Types.HandshakeId
import Network.AWS.Organizations.Types.HandshakeNotes
import Network.AWS.Organizations.Types.HandshakeParty
import Network.AWS.Organizations.Types.HandshakePartyId
import Network.AWS.Organizations.Types.HandshakePartyType
import Network.AWS.Organizations.Types.HandshakeResource
import Network.AWS.Organizations.Types.HandshakeResourceType
import Network.AWS.Organizations.Types.HandshakeState
import Network.AWS.Organizations.Types.IAMUserAccessToBilling
import Network.AWS.Organizations.Types.Id
import Network.AWS.Organizations.Types.Key
import Network.AWS.Organizations.Types.Name
import Network.AWS.Organizations.Types.NextToken
import Network.AWS.Organizations.Types.Organization
import Network.AWS.Organizations.Types.OrganizationFeatureSet
import Network.AWS.Organizations.Types.OrganizationalUnit
import Network.AWS.Organizations.Types.OrganizationalUnitArn
import Network.AWS.Organizations.Types.OrganizationalUnitId
import Network.AWS.Organizations.Types.OrganizationalUnitName
import Network.AWS.Organizations.Types.Parent
import Network.AWS.Organizations.Types.ParentHandshakeId
import Network.AWS.Organizations.Types.ParentId
import Network.AWS.Organizations.Types.ParentType
import Network.AWS.Organizations.Types.Policy
import Network.AWS.Organizations.Types.PolicyContent
import Network.AWS.Organizations.Types.PolicyDescription
import Network.AWS.Organizations.Types.PolicyId
import Network.AWS.Organizations.Types.PolicyName
import Network.AWS.Organizations.Types.PolicySummary
import Network.AWS.Organizations.Types.PolicyTargetId
import Network.AWS.Organizations.Types.PolicyTargetSummary
import Network.AWS.Organizations.Types.PolicyType
import Network.AWS.Organizations.Types.PolicyTypeStatus
import Network.AWS.Organizations.Types.PolicyTypeSummary
import Network.AWS.Organizations.Types.ResourceId
import Network.AWS.Organizations.Types.RoleName
import Network.AWS.Organizations.Types.Root
import Network.AWS.Organizations.Types.RootId
import Network.AWS.Organizations.Types.ServicePrincipal
import Network.AWS.Organizations.Types.SourceParentId
import Network.AWS.Organizations.Types.Tag
import Network.AWS.Organizations.Types.TagKey
import Network.AWS.Organizations.Types.TaggableResourceId
import Network.AWS.Organizations.Types.TargetId
import Network.AWS.Organizations.Types.TargetType
import Network.AWS.Organizations.Types.Value
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Organizations SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Organizations",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "organizations",
      Core._svcVersion = "2016-11-28",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Organizations",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | We can't find a policy with the @PolicyId@ that you specified.
_PolicyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyNotFoundException =
  Core._MatchServiceError mkServiceConfig "PolicyNotFoundException"
{-# DEPRECATED _PolicyNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified policy type is already enabled in the specified root.
_PolicyTypeAlreadyEnabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyTypeAlreadyEnabledException =
  Core._MatchServiceError
    mkServiceConfig
    "PolicyTypeAlreadyEnabledException"
{-# DEPRECATED _PolicyTypeAlreadyEnabledException "Use generic-lens or generic-optics instead." #-}

-- | The requested operation would violate the constraint identified in the reason code.
--
--
--     * ACCOUNT_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the limit on the number of accounts in an organization. Note that deleted and closed accounts still count toward your limit.
-- /Important:/ If you get this exception immediately after creating the organization, wait one hour and try again. If after an hour it continues to fail with this error, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--
--     * ALREADY_IN_AN_ORGANIZATION: The handshake request is invalid because the invited account is already a member of an organization.
--
--
--     * HANDSHAKE_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of handshakes that you can send in one day.
--
--
--     * INVITE_DISABLED_DURING_ENABLE_ALL_FEATURES: You can't issue new invitations to join an organization while it's in the process of enabling all features. You can resume inviting accounts after you finalize the process when all accounts have agreed to the change.
--
--
--     * ORGANIZATION_ALREADY_HAS_ALL_FEATURES: The handshake request is invalid because the organization has already enabled all features.
--
--
--     * ORGANIZATION_FROM_DIFFERENT_SELLER_OF_RECORD: The request failed because the account is from a different marketplace than the accounts in the organization. For example, accounts with India addresses must be associated with the AISPL marketplace. All accounts in an organization must be from the same marketplace.
--
--
--     * ORGANIZATION_MEMBERSHIP_CHANGE_RATE_LIMIT_EXCEEDED: You attempted to change the membership of an account too quickly after its previous change.
--
--
--     * PAYMENT_INSTRUMENT_REQUIRED: You can't complete the operation with an account that doesn't have a payment instrument, such as a credit card, associated with it.
_HandshakeConstraintViolationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HandshakeConstraintViolationException =
  Core._MatchServiceError
    mkServiceConfig
    "HandshakeConstraintViolationException"
{-# DEPRECATED _HandshakeConstraintViolationException "Use generic-lens or generic-optics instead." #-}

-- | You don't have permissions to perform the requested operation. The user or role that is making the request must have at least one IAM permissions policy attached that grants the required permissions. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access.html Access Management> in the /IAM User Guide./
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead." #-}

-- | The provided policy document doesn't meet the requirements of the specified policy type. For example, the syntax might be incorrect. For details about service control policy syntax, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax> in the /AWS Organizations User Guide./
_MalformedPolicyDocumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyDocumentException =
  Core._MatchServiceError
    mkServiceConfig
    "MalformedPolicyDocumentException"
{-# DEPRECATED _MalformedPolicyDocumentException "Use generic-lens or generic-optics instead." #-}

-- | We can't find a root with the @RootId@ that you specified.
_RootNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RootNotFoundException =
  Core._MatchServiceError mkServiceConfig "RootNotFoundException"
{-# DEPRECATED _RootNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | You can't remove a management account from an organization. If you want the management account to become a member account in another organization, you must first delete the current organization of the management account.
_MasterCannotLeaveOrganizationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MasterCannotLeaveOrganizationException =
  Core._MatchServiceError
    mkServiceConfig
    "MasterCannotLeaveOrganizationException"
{-# DEPRECATED _MasterCannotLeaveOrganizationException "Use generic-lens or generic-optics instead." #-}

-- | We can't find an AWS account with the @AccountId@ that you specified, or the account whose credentials you used to make this request isn't a member of an organization.
_AccountNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "AccountNotFoundException"
{-# DEPRECATED _AccountNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified account is already a delegated administrator for this AWS service.
_AccountAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountAlreadyRegisteredException =
  Core._MatchServiceError
    mkServiceConfig
    "AccountAlreadyRegisteredException"
{-# DEPRECATED _AccountAlreadyRegisteredException "Use generic-lens or generic-optics instead." #-}

-- | A policy with the same name already exists.
_DuplicatePolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicatePolicyException =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicatePolicyException"
{-# DEPRECATED _DuplicatePolicyException "Use generic-lens or generic-optics instead." #-}

-- | Performing this operation violates a minimum or maximum value limit. For example, attempting to remove the last service control policy (SCP) from an OU or root, inviting or creating too many accounts to the organization, or attaching too many policies to an account, OU, or root. This exception includes a reason that contains additional information about the violated limit:
--
--
--     * ACCOUNT_CANNOT_LEAVE_ORGANIZAION: You attempted to remove the management account from the organization. You can't remove the management account. Instead, after you remove all member accounts, delete the organization itself.
--
--
--     * ACCOUNT_CANNOT_LEAVE_WITHOUT_EULA: You attempted to remove an account from the organization that doesn't yet have enough information to exist as a standalone account. This account requires you to first agree to the AWS Customer Agreement. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#orgs_manage_accounts_remove-from-master Removing a member account from your organization> in the /AWS Organizations User Guide./
--
--
--     * ACCOUNT_CANNOT_LEAVE_WITHOUT_PHONE_VERIFICATION: You attempted to remove an account from the organization that doesn't yet have enough information to exist as a standalone account. This account requires you to first complete phone verification. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#orgs_manage_accounts_remove-from-master Removing a member account from your organization> in the /AWS Organizations User Guide./
--
--
--     * ACCOUNT_CREATION_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of accounts that you can create in one day.
--
--
--     * ACCOUNT_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the limit on the number of accounts in an organization. If you need more accounts, contact <https://console.aws.amazon.com/support/home#/ AWS Support> to request an increase in your limit.
-- Or the number of invitations that you tried to send would cause you to exceed the limit of accounts in your organization. Send fewer invitations or contact AWS Support to request an increase in the number of accounts.
-- /Important:/ If you get this exception when running a command immediately after creating the organization, wait one hour and try again. After an hour, if the command continues to fail with this error, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--
--     * CANNOT_REGISTER_MASTER_AS_DELEGATED_ADMINISTRATOR: You attempted to register the management account of the organization as a delegated administrator for an AWS service integrated with Organizations. You can designate only a member account as a delegated administrator.
--
--
--     * CANNOT_REMOVE_DELEGATED_ADMINISTRATOR_FROM_ORG: You attempted to remove an account that is registered as a delegated administrator for a service integrated with your organization. To complete this operation, you must first deregister this account as a delegated administrator.
--
--
--     * CREATE_ORGANIZATION_IN_BILLING_MODE_UNSUPPORTED_REGION: To create an organization in the specified region, you must enable all features mode.
--
--
--     * DELEGATED_ADMINISTRATOR_EXISTS_FOR_THIS_SERVICE: You attempted to register an AWS account as a delegated administrator for an AWS service that already has a delegated administrator. To complete this operation, you must first deregister any existing delegated administrators for this service.
--
--
--     * EMAIL_VERIFICATION_CODE_EXPIRED: The email verification code is only valid for a limited period of time. You must resubmit the request and generate a new verfication code.
--
--
--     * HANDSHAKE_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of handshakes that you can send in one day.
--
--
--     * MASTER_ACCOUNT_ADDRESS_DOES_NOT_MATCH_MARKETPLACE: To create an account in this organization, you first must migrate the organization's management account to the marketplace that corresponds to the management account's address. For example, accounts with India addresses must be associated with the AISPL marketplace. All accounts in an organization must be associated with the same marketplace.
--
--
--     * MASTER_ACCOUNT_MISSING_BUSINESS_LICENSE: Applies only to the AWS Regions in China. To create an organization, the master must have an valid business license. For more information, contact customer support.
--
--
--     * MASTER_ACCOUNT_MISSING_CONTACT_INFO: To complete this operation, you must first provide a valid contact address and phone number for the management account. Then try the operation again.
--
--
--     * MASTER_ACCOUNT_NOT_GOVCLOUD_ENABLED: To complete this operation, the management account must have an associated account in the AWS GovCloud (US-West) Region. For more information, see <http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/govcloud-organizations.html AWS Organizations> in the /AWS GovCloud User Guide./
--
--
--     * MASTER_ACCOUNT_PAYMENT_INSTRUMENT_REQUIRED: To create an organization with this management account, you first must associate a valid payment instrument, such as a credit card, with the account. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided> in the /AWS Organizations User Guide./
--
--
--     * MAX_DELEGATED_ADMINISTRATORS_FOR_SERVICE_LIMIT_EXCEEDED: You attempted to register more delegated administrators than allowed for the service principal.
--
--
--     * MAX_POLICY_TYPE_ATTACHMENT_LIMIT_EXCEEDED: You attempted to exceed the number of policies of a certain type that can be attached to an entity at one time.
--
--
--     * MAX_TAG_LIMIT_EXCEEDED: You have exceeded the number of tags allowed on this resource.
--
--
--     * MEMBER_ACCOUNT_PAYMENT_INSTRUMENT_REQUIRED: To complete this operation with this member account, you first must associate a valid payment instrument, such as a credit card, with the account. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided> in the /AWS Organizations User Guide./
--
--
--     * MIN_POLICY_TYPE_ATTACHMENT_LIMIT_EXCEEDED: You attempted to detach a policy from an entity that would cause the entity to have fewer than the minimum number of policies of a certain type required.
--
--
--     * ORGANIZATION_NOT_IN_ALL_FEATURES_MODE: You attempted to perform an operation that requires the organization to be configured to support all features. An organization that supports only consolidated billing features can't perform this operation.
--
--
--     * OU_DEPTH_LIMIT_EXCEEDED: You attempted to create an OU tree that is too many levels deep.
--
--
--     * OU_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the number of OUs that you can have in an organization.
--
--
--     * POLICY_CONTENT_LIMIT_EXCEEDED: You attempted to create a policy that is larger than the maximum size.
--
--
--     * POLICY_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the number of policies that you can have in an organization.
--
--
--     * TAG_POLICY_VIOLATION: You attempted to create or update a resource with tags that are not compliant with the tag policy requirements for this account.
_ConstraintViolationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConstraintViolationException =
  Core._MatchServiceError
    mkServiceConfig
    "ConstraintViolationException"
{-# DEPRECATED _ConstraintViolationException "Use generic-lens or generic-optics instead." #-}

-- | The specified account is not a delegated administrator for this AWS service.
_AccountNotRegisteredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountNotRegisteredException =
  Core._MatchServiceError
    mkServiceConfig
    "AccountNotRegisteredException"
{-# DEPRECATED _AccountNotRegisteredException "Use generic-lens or generic-optics instead." #-}

-- | AWS Organizations couldn't perform the operation because your organization hasn't finished initializing. This can take up to an hour. Try again later. If after one hour you continue to receive this error, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
_FinalizingOrganizationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FinalizingOrganizationException =
  Core._MatchServiceError
    mkServiceConfig
    "FinalizingOrganizationException"
{-# DEPRECATED _FinalizingOrganizationException "Use generic-lens or generic-optics instead." #-}

-- | We can't find a handshake with the @HandshakeId@ that you specified.
_HandshakeNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HandshakeNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "HandshakeNotFoundException"
{-# DEPRECATED _HandshakeNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | You can't use the specified policy type with the feature set currently enabled for this organization. For example, you can enable SCPs only after you enable all features in the organization. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html#enable_policies_on_root Managing AWS Organizations Policies> in the /AWS Organizations User Guide./
_PolicyTypeNotAvailableForOrganizationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyTypeNotAvailableForOrganizationException =
  Core._MatchServiceError
    mkServiceConfig
    "PolicyTypeNotAvailableForOrganizationException"
{-# DEPRECATED _PolicyTypeNotAvailableForOrganizationException "Use generic-lens or generic-optics instead." #-}

-- | We can't find an organizational unit (OU) or AWS account with the @ChildId@ that you specified.
_ChildNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ChildNotFoundException =
  Core._MatchServiceError mkServiceConfig "ChildNotFoundException"
{-# DEPRECATED _ChildNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | This action isn't available in the current AWS Region.
_UnsupportedAPIEndpointException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedAPIEndpointException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedAPIEndpointException"
{-# DEPRECATED _UnsupportedAPIEndpointException "Use generic-lens or generic-optics instead." #-}

-- | If you ran this action on the management account, this policy type is not enabled. If you ran the action on a member account, the account doesn't have an effective policy of this type. Contact the administrator of your organization about attaching a policy of this type to the account.
_EffectivePolicyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EffectivePolicyNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "EffectivePolicyNotFoundException"
{-# DEPRECATED _EffectivePolicyNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | We can't find an OU with the @OrganizationalUnitId@ that you specified.
_OrganizationalUnitNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationalUnitNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "OrganizationalUnitNotFoundException"
{-# DEPRECATED _OrganizationalUnitNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | We can't find the destination container (a root or OU) with the @ParentId@ that you specified.
_DestinationParentNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DestinationParentNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "DestinationParentNotFoundException"
{-# DEPRECATED _DestinationParentNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The organization isn't empty. To delete an organization, you must first remove all accounts except the management account, delete all OUs, and delete all policies.
_OrganizationNotEmptyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationNotEmptyException =
  Core._MatchServiceError
    mkServiceConfig
    "OrganizationNotEmptyException"
{-# DEPRECATED _OrganizationNotEmptyException "Use generic-lens or generic-optics instead." #-}

-- | You can't invite an existing account to your organization until you verify that you own the email address associated with the management account. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_create.html#about-email-verification Email Address Verification> in the /AWS Organizations User Guide./
_AccountOwnerNotVerifiedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountOwnerNotVerifiedException =
  Core._MatchServiceError
    mkServiceConfig
    "AccountOwnerNotVerifiedException"
{-# DEPRECATED _AccountOwnerNotVerifiedException "Use generic-lens or generic-optics instead." #-}

-- | The specified policy type isn't currently enabled in this root. You can't attach policies of the specified type to entities in a root until you enable that type in the root. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide./
_PolicyTypeNotEnabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyTypeNotEnabledException =
  Core._MatchServiceError
    mkServiceConfig
    "PolicyTypeNotEnabledException"
{-# DEPRECATED _PolicyTypeNotEnabledException "Use generic-lens or generic-optics instead." #-}

-- | A handshake with the same action and target already exists. For example, if you invited an account to join your organization, the invited account might already have a pending invitation from this organization. If you intend to resend an invitation to an account, ensure that existing handshakes that might be considered duplicates are canceled or declined.
_DuplicateHandshakeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateHandshakeException =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicateHandshakeException"
{-# DEPRECATED _DuplicateHandshakeException "Use generic-lens or generic-optics instead." #-}

-- | The specified OU is not empty. Move all accounts to another root or to other OUs, remove all child OUs, and try the operation again.
_OrganizationalUnitNotEmptyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationalUnitNotEmptyException =
  Core._MatchServiceError
    mkServiceConfig
    "OrganizationalUnitNotEmptyException"
{-# DEPRECATED _OrganizationalUnitNotEmptyException "Use generic-lens or generic-optics instead." #-}

-- | You have sent too many requests in too short a period of time. The quota helps protect against denial-of-service attacks. Try again later.
--
-- For information about quotas that affect AWS Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_limits.html Quotas for AWS Organizations> in the /AWS Organizations User Guide./
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyRequestsException"
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead." #-}

-- | The target of the operation is currently being modified by a different request. Try again later.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentModificationException"
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead." #-}

-- | AWS Organizations can't complete your request because of an internal service error. Try again later.
_ServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceException =
  Core._MatchServiceError mkServiceConfig "ServiceException"
{-# DEPRECATED _ServiceException "Use generic-lens or generic-optics instead." #-}

-- | We can't find a source root or OU with the @ParentId@ that you specified.
_SourceParentNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SourceParentNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "SourceParentNotFoundException"
{-# DEPRECATED _SourceParentNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | We can't find a root, OU, account, or policy with the @TargetId@ that you specified.
_TargetNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TargetNotFoundException =
  Core._MatchServiceError mkServiceConfig "TargetNotFoundException"
{-# DEPRECATED _TargetNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | We can't find an create account request with the @CreateAccountRequestId@ that you specified.
_CreateAccountStatusNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CreateAccountStatusNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "CreateAccountStatusNotFoundException"
{-# DEPRECATED _CreateAccountStatusNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | This account is already a member of an organization. An account can belong to only one organization at a time.
_AlreadyInOrganizationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyInOrganizationException =
  Core._MatchServiceError
    mkServiceConfig
    "AlreadyInOrganizationException"
{-# DEPRECATED _AlreadyInOrganizationException "Use generic-lens or generic-optics instead." #-}

-- | An OU with the same name already exists.
_DuplicateOrganizationalUnitException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateOrganizationalUnitException =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicateOrganizationalUnitException"
{-# DEPRECATED _DuplicateOrganizationalUnitException "Use generic-lens or generic-optics instead." #-}

-- | The requested operation failed because you provided invalid values for one or more of the request parameters. This exception includes a reason that contains additional information about the violated limit:
--
--
--     * DUPLICATE_TAG_KEY: Tag keys must be unique among the tags attached to the same entity.
--
--
--     * IMMUTABLE_POLICY: You specified a policy that is managed by AWS and can't be modified.
--
--
--     * INPUT_REQUIRED: You must include a value for all required parameters.
--
--
--     * INVALID_ENUM: You specified an invalid value.
--
--
--     * INVALID_ENUM_POLICY_TYPE: You specified an invalid policy type string.
--
--
--     * INVALID_FULL_NAME_TARGET: You specified a full name that contains invalid characters.
--
--
--     * INVALID_LIST_MEMBER: You provided a list to a parameter that contains at least one invalid value.
--
--
--     * INVALID_PAGINATION_TOKEN: Get the value for the @NextToken@ parameter from the response to a previous call of the operation.
--
--
--     * INVALID_PARTY_TYPE_TARGET: You specified the wrong type of entity (account, organization, or email) as a party.
--
--
--     * INVALID_PATTERN: You provided a value that doesn't match the required pattern.
--
--
--     * INVALID_PATTERN_TARGET_ID: You specified a policy target ID that doesn't match the required pattern.
--
--
--     * INVALID_ROLE_NAME: You provided a role name that isn't valid. A role name can't begin with the reserved prefix @AWSServiceRoleFor@ .
--
--
--     * INVALID_SYNTAX_ORGANIZATION_ARN: You specified an invalid Amazon Resource Name (ARN) for the organization.
--
--
--     * INVALID_SYNTAX_POLICY_ID: You specified an invalid policy ID.
--
--
--     * INVALID_SYSTEM_TAGS_PARAMETER: You specified a tag key that is a system tag. You can’t add, edit, or delete system tag keys because they're reserved for AWS use. System tags don’t count against your tags per resource limit.
--
--
--     * MAX_FILTER_LIMIT_EXCEEDED: You can specify only one filter parameter for the operation.
--
--
--     * MAX_LENGTH_EXCEEDED: You provided a string parameter that is longer than allowed.
--
--
--     * MAX_VALUE_EXCEEDED: You provided a numeric parameter that has a larger value than allowed.
--
--
--     * MIN_LENGTH_EXCEEDED: You provided a string parameter that is shorter than allowed.
--
--
--     * MIN_VALUE_EXCEEDED: You provided a numeric parameter that has a smaller value than allowed.
--
--
--     * MOVING_ACCOUNT_BETWEEN_DIFFERENT_ROOTS: You can move an account only between entities in the same root.
--
--
--     * TARGET_NOT_SUPPORTED: You can't perform the specified operation on that target entity.
--
--
--     * UNRECOGNIZED_SERVICE_PRINCIPAL: You specified a service principal that isn't recognized.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError mkServiceConfig "InvalidInputException"
{-# DEPRECATED _InvalidInputException "Use generic-lens or generic-optics instead." #-}

-- | Changes to the effective policy are in progress, and its contents can't be returned. Try the operation again later.
_PolicyChangesInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyChangesInProgressException =
  Core._MatchServiceError
    mkServiceConfig
    "PolicyChangesInProgressException"
{-# DEPRECATED _PolicyChangesInProgressException "Use generic-lens or generic-optics instead." #-}

-- | The policy isn't attached to the specified target in the specified root.
_PolicyNotAttachedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyNotAttachedException =
  Core._MatchServiceError
    mkServiceConfig
    "PolicyNotAttachedException"
{-# DEPRECATED _PolicyNotAttachedException "Use generic-lens or generic-optics instead." #-}

-- | We can't find a root or OU with the @ParentId@ that you specified.
_ParentNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParentNotFoundException =
  Core._MatchServiceError mkServiceConfig "ParentNotFoundException"
{-# DEPRECATED _ParentNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The operation that you attempted requires you to have the @iam:CreateServiceLinkedRole@ for @organizations.amazonaws.com@ permission so that AWS Organizations can create the required service-linked role. You don't have that permission.
_AccessDeniedForDependencyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedForDependencyException =
  Core._MatchServiceError
    mkServiceConfig
    "AccessDeniedForDependencyException"
{-# DEPRECATED _AccessDeniedForDependencyException "Use generic-lens or generic-optics instead." #-}

-- | Your account isn't a member of an organization. To make this request, you must use the credentials of an account that belongs to an organization.
_AWSOrganizationsNotInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AWSOrganizationsNotInUseException =
  Core._MatchServiceError
    mkServiceConfig
    "AWSOrganizationsNotInUseException"
{-# DEPRECATED _AWSOrganizationsNotInUseException "Use generic-lens or generic-optics instead." #-}

-- | The policy is attached to one or more entities. You must detach it from all roots, OUs, and accounts before performing this operation.
_PolicyInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyInUseException =
  Core._MatchServiceError mkServiceConfig "PolicyInUseException"
{-# DEPRECATED _PolicyInUseException "Use generic-lens or generic-optics instead." #-}

-- | You can't perform the operation on the handshake in its current state. For example, you can't cancel a handshake that was already accepted or accept a handshake that was already declined.
_InvalidHandshakeTransitionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidHandshakeTransitionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidHandshakeTransitionException"
{-# DEPRECATED _InvalidHandshakeTransitionException "Use generic-lens or generic-optics instead." #-}

-- | The specified handshake is already in the requested state. For example, you can't accept a handshake that was already accepted.
_HandshakeAlreadyInStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HandshakeAlreadyInStateException =
  Core._MatchServiceError
    mkServiceConfig
    "HandshakeAlreadyInStateException"
{-# DEPRECATED _HandshakeAlreadyInStateException "Use generic-lens or generic-optics instead." #-}

-- | That account is already present in the specified destination.
_DuplicateAccountException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateAccountException =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicateAccountException"
{-# DEPRECATED _DuplicateAccountException "Use generic-lens or generic-optics instead." #-}

-- | The selected policy is already attached to the specified target.
_DuplicatePolicyAttachmentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicatePolicyAttachmentException =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicatePolicyAttachmentException"
{-# DEPRECATED _DuplicatePolicyAttachmentException "Use generic-lens or generic-optics instead." #-}
