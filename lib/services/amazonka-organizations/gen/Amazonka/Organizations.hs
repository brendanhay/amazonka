{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Organizations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-11-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Organizations is a web service that enables you to consolidate your
-- multiple Amazon Web Services accounts into an /organization/ and
-- centrally manage your accounts and their resources.
--
-- This guide provides descriptions of the Organizations operations. For
-- more information about using this service, see the
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html Organizations User Guide>.
--
-- __Support and feedback for Organizations__
--
-- We welcome your feedback. Send your comments to
-- <mailto:feedback-awsorganizations@amazon.com feedback-awsorganizations\@amazon.com>
-- or post your feedback and questions in the
-- <http://forums.aws.amazon.com/forum.jspa?forumID=219 Organizations support forum>.
-- For more information about the Amazon Web Services support forums, see
-- <http://forums.aws.amazon.com/help.jspa Forums Help>.
--
-- __Endpoint to call When using the CLI or the Amazon Web Services SDK__
--
-- For the current release of Organizations, specify the @us-east-1@ region
-- for all Amazon Web Services API and CLI calls made from the commercial
-- Amazon Web Services Regions outside of China. If calling from one of the
-- Amazon Web Services Regions in China, then specify @cn-northwest-1@. You
-- can do this in the CLI by using these parameters and commands:
--
-- -   Use the following parameter with each command to specify both the
--     endpoint and its region:
--
--     @--endpoint-url https:\/\/organizations.us-east-1.amazonaws.com@
--     /(from commercial Amazon Web Services Regions outside of China)/
--
--     or
--
--     @--endpoint-url https:\/\/organizations.cn-northwest-1.amazonaws.com.cn@
--     /(from Amazon Web Services Regions in China)/
--
-- -   Use the default endpoint, but configure your default region with
--     this command:
--
--     @aws configure set default.region us-east-1@ /(from commercial
--     Amazon Web Services Regions outside of China)/
--
--     or
--
--     @aws configure set default.region cn-northwest-1@ /(from Amazon Web
--     Services Regions in China)/
--
-- -   Use the following parameter with each command to specify the
--     endpoint:
--
--     @--region us-east-1@ /(from commercial Amazon Web Services Regions
--     outside of China)/
--
--     or
--
--     @--region cn-northwest-1@ /(from Amazon Web Services Regions in
--     China)/
--
-- __Recording API Requests__
--
-- Organizations supports CloudTrail, a service that records Amazon Web
-- Services API calls for your Amazon Web Services account and delivers log
-- files to an Amazon S3 bucket. By using information collected by
-- CloudTrail, you can determine which requests the Organizations service
-- received, who made the request and when, and so on. For more about
-- Organizations and its support for CloudTrail, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_incident-response.html#orgs_cloudtrail-integration Logging Organizations Events with CloudTrail>
-- in the /Organizations User Guide/. To learn more about CloudTrail,
-- including how to turn it on and find your log files, see the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html CloudTrail User Guide>.
module Amazonka.Organizations
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AWSOrganizationsNotInUseException
    _AWSOrganizationsNotInUseException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AccessDeniedForDependencyException
    _AccessDeniedForDependencyException,

    -- ** AccountAlreadyClosedException
    _AccountAlreadyClosedException,

    -- ** AccountAlreadyRegisteredException
    _AccountAlreadyRegisteredException,

    -- ** AccountNotFoundException
    _AccountNotFoundException,

    -- ** AccountNotRegisteredException
    _AccountNotRegisteredException,

    -- ** AccountOwnerNotVerifiedException
    _AccountOwnerNotVerifiedException,

    -- ** AlreadyInOrganizationException
    _AlreadyInOrganizationException,

    -- ** ChildNotFoundException
    _ChildNotFoundException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ConflictException
    _ConflictException,

    -- ** ConstraintViolationException
    _ConstraintViolationException,

    -- ** CreateAccountStatusNotFoundException
    _CreateAccountStatusNotFoundException,

    -- ** DestinationParentNotFoundException
    _DestinationParentNotFoundException,

    -- ** DuplicateAccountException
    _DuplicateAccountException,

    -- ** DuplicateHandshakeException
    _DuplicateHandshakeException,

    -- ** DuplicateOrganizationalUnitException
    _DuplicateOrganizationalUnitException,

    -- ** DuplicatePolicyAttachmentException
    _DuplicatePolicyAttachmentException,

    -- ** DuplicatePolicyException
    _DuplicatePolicyException,

    -- ** EffectivePolicyNotFoundException
    _EffectivePolicyNotFoundException,

    -- ** FinalizingOrganizationException
    _FinalizingOrganizationException,

    -- ** HandshakeAlreadyInStateException
    _HandshakeAlreadyInStateException,

    -- ** HandshakeConstraintViolationException
    _HandshakeConstraintViolationException,

    -- ** HandshakeNotFoundException
    _HandshakeNotFoundException,

    -- ** InvalidHandshakeTransitionException
    _InvalidHandshakeTransitionException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** MasterCannotLeaveOrganizationException
    _MasterCannotLeaveOrganizationException,

    -- ** OrganizationNotEmptyException
    _OrganizationNotEmptyException,

    -- ** OrganizationalUnitNotEmptyException
    _OrganizationalUnitNotEmptyException,

    -- ** OrganizationalUnitNotFoundException
    _OrganizationalUnitNotFoundException,

    -- ** ParentNotFoundException
    _ParentNotFoundException,

    -- ** PolicyChangesInProgressException
    _PolicyChangesInProgressException,

    -- ** PolicyInUseException
    _PolicyInUseException,

    -- ** PolicyNotAttachedException
    _PolicyNotAttachedException,

    -- ** PolicyNotFoundException
    _PolicyNotFoundException,

    -- ** PolicyTypeAlreadyEnabledException
    _PolicyTypeAlreadyEnabledException,

    -- ** PolicyTypeNotAvailableForOrganizationException
    _PolicyTypeNotAvailableForOrganizationException,

    -- ** PolicyTypeNotEnabledException
    _PolicyTypeNotEnabledException,

    -- ** ResourcePolicyNotFoundException
    _ResourcePolicyNotFoundException,

    -- ** RootNotFoundException
    _RootNotFoundException,

    -- ** ServiceException
    _ServiceException,

    -- ** SourceParentNotFoundException
    _SourceParentNotFoundException,

    -- ** TargetNotFoundException
    _TargetNotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** UnsupportedAPIEndpointException
    _UnsupportedAPIEndpointException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptHandshake
    AcceptHandshake (AcceptHandshake'),
    newAcceptHandshake,
    AcceptHandshakeResponse (AcceptHandshakeResponse'),
    newAcceptHandshakeResponse,

    -- ** AttachPolicy
    AttachPolicy (AttachPolicy'),
    newAttachPolicy,
    AttachPolicyResponse (AttachPolicyResponse'),
    newAttachPolicyResponse,

    -- ** CancelHandshake
    CancelHandshake (CancelHandshake'),
    newCancelHandshake,
    CancelHandshakeResponse (CancelHandshakeResponse'),
    newCancelHandshakeResponse,

    -- ** CloseAccount
    CloseAccount (CloseAccount'),
    newCloseAccount,
    CloseAccountResponse (CloseAccountResponse'),
    newCloseAccountResponse,

    -- ** CreateAccount
    CreateAccount (CreateAccount'),
    newCreateAccount,
    CreateAccountResponse (CreateAccountResponse'),
    newCreateAccountResponse,

    -- ** CreateGovCloudAccount
    CreateGovCloudAccount (CreateGovCloudAccount'),
    newCreateGovCloudAccount,
    CreateGovCloudAccountResponse (CreateGovCloudAccountResponse'),
    newCreateGovCloudAccountResponse,

    -- ** CreateOrganization
    CreateOrganization (CreateOrganization'),
    newCreateOrganization,
    CreateOrganizationResponse (CreateOrganizationResponse'),
    newCreateOrganizationResponse,

    -- ** CreateOrganizationalUnit
    CreateOrganizationalUnit (CreateOrganizationalUnit'),
    newCreateOrganizationalUnit,
    CreateOrganizationalUnitResponse (CreateOrganizationalUnitResponse'),
    newCreateOrganizationalUnitResponse,

    -- ** CreatePolicy
    CreatePolicy (CreatePolicy'),
    newCreatePolicy,
    CreatePolicyResponse (CreatePolicyResponse'),
    newCreatePolicyResponse,

    -- ** DeclineHandshake
    DeclineHandshake (DeclineHandshake'),
    newDeclineHandshake,
    DeclineHandshakeResponse (DeclineHandshakeResponse'),
    newDeclineHandshakeResponse,

    -- ** DeleteOrganization
    DeleteOrganization (DeleteOrganization'),
    newDeleteOrganization,
    DeleteOrganizationResponse (DeleteOrganizationResponse'),
    newDeleteOrganizationResponse,

    -- ** DeleteOrganizationalUnit
    DeleteOrganizationalUnit (DeleteOrganizationalUnit'),
    newDeleteOrganizationalUnit,
    DeleteOrganizationalUnitResponse (DeleteOrganizationalUnitResponse'),
    newDeleteOrganizationalUnitResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeregisterDelegatedAdministrator
    DeregisterDelegatedAdministrator (DeregisterDelegatedAdministrator'),
    newDeregisterDelegatedAdministrator,
    DeregisterDelegatedAdministratorResponse (DeregisterDelegatedAdministratorResponse'),
    newDeregisterDelegatedAdministratorResponse,

    -- ** DescribeAccount
    DescribeAccount (DescribeAccount'),
    newDescribeAccount,
    DescribeAccountResponse (DescribeAccountResponse'),
    newDescribeAccountResponse,

    -- ** DescribeCreateAccountStatus
    DescribeCreateAccountStatus (DescribeCreateAccountStatus'),
    newDescribeCreateAccountStatus,
    DescribeCreateAccountStatusResponse (DescribeCreateAccountStatusResponse'),
    newDescribeCreateAccountStatusResponse,

    -- ** DescribeEffectivePolicy
    DescribeEffectivePolicy (DescribeEffectivePolicy'),
    newDescribeEffectivePolicy,
    DescribeEffectivePolicyResponse (DescribeEffectivePolicyResponse'),
    newDescribeEffectivePolicyResponse,

    -- ** DescribeHandshake
    DescribeHandshake (DescribeHandshake'),
    newDescribeHandshake,
    DescribeHandshakeResponse (DescribeHandshakeResponse'),
    newDescribeHandshakeResponse,

    -- ** DescribeOrganization
    DescribeOrganization (DescribeOrganization'),
    newDescribeOrganization,
    DescribeOrganizationResponse (DescribeOrganizationResponse'),
    newDescribeOrganizationResponse,

    -- ** DescribeOrganizationalUnit
    DescribeOrganizationalUnit (DescribeOrganizationalUnit'),
    newDescribeOrganizationalUnit,
    DescribeOrganizationalUnitResponse (DescribeOrganizationalUnitResponse'),
    newDescribeOrganizationalUnitResponse,

    -- ** DescribePolicy
    DescribePolicy (DescribePolicy'),
    newDescribePolicy,
    DescribePolicyResponse (DescribePolicyResponse'),
    newDescribePolicyResponse,

    -- ** DescribeResourcePolicy
    DescribeResourcePolicy (DescribeResourcePolicy'),
    newDescribeResourcePolicy,
    DescribeResourcePolicyResponse (DescribeResourcePolicyResponse'),
    newDescribeResourcePolicyResponse,

    -- ** DetachPolicy
    DetachPolicy (DetachPolicy'),
    newDetachPolicy,
    DetachPolicyResponse (DetachPolicyResponse'),
    newDetachPolicyResponse,

    -- ** DisableAWSServiceAccess
    DisableAWSServiceAccess (DisableAWSServiceAccess'),
    newDisableAWSServiceAccess,
    DisableAWSServiceAccessResponse (DisableAWSServiceAccessResponse'),
    newDisableAWSServiceAccessResponse,

    -- ** DisablePolicyType
    DisablePolicyType (DisablePolicyType'),
    newDisablePolicyType,
    DisablePolicyTypeResponse (DisablePolicyTypeResponse'),
    newDisablePolicyTypeResponse,

    -- ** EnableAWSServiceAccess
    EnableAWSServiceAccess (EnableAWSServiceAccess'),
    newEnableAWSServiceAccess,
    EnableAWSServiceAccessResponse (EnableAWSServiceAccessResponse'),
    newEnableAWSServiceAccessResponse,

    -- ** EnableAllFeatures
    EnableAllFeatures (EnableAllFeatures'),
    newEnableAllFeatures,
    EnableAllFeaturesResponse (EnableAllFeaturesResponse'),
    newEnableAllFeaturesResponse,

    -- ** EnablePolicyType
    EnablePolicyType (EnablePolicyType'),
    newEnablePolicyType,
    EnablePolicyTypeResponse (EnablePolicyTypeResponse'),
    newEnablePolicyTypeResponse,

    -- ** InviteAccountToOrganization
    InviteAccountToOrganization (InviteAccountToOrganization'),
    newInviteAccountToOrganization,
    InviteAccountToOrganizationResponse (InviteAccountToOrganizationResponse'),
    newInviteAccountToOrganizationResponse,

    -- ** LeaveOrganization
    LeaveOrganization (LeaveOrganization'),
    newLeaveOrganization,
    LeaveOrganizationResponse (LeaveOrganizationResponse'),
    newLeaveOrganizationResponse,

    -- ** ListAWSServiceAccessForOrganization (Paginated)
    ListAWSServiceAccessForOrganization (ListAWSServiceAccessForOrganization'),
    newListAWSServiceAccessForOrganization,
    ListAWSServiceAccessForOrganizationResponse (ListAWSServiceAccessForOrganizationResponse'),
    newListAWSServiceAccessForOrganizationResponse,

    -- ** ListAccounts (Paginated)
    ListAccounts (ListAccounts'),
    newListAccounts,
    ListAccountsResponse (ListAccountsResponse'),
    newListAccountsResponse,

    -- ** ListAccountsForParent (Paginated)
    ListAccountsForParent (ListAccountsForParent'),
    newListAccountsForParent,
    ListAccountsForParentResponse (ListAccountsForParentResponse'),
    newListAccountsForParentResponse,

    -- ** ListChildren (Paginated)
    ListChildren (ListChildren'),
    newListChildren,
    ListChildrenResponse (ListChildrenResponse'),
    newListChildrenResponse,

    -- ** ListCreateAccountStatus (Paginated)
    ListCreateAccountStatus (ListCreateAccountStatus'),
    newListCreateAccountStatus,
    ListCreateAccountStatusResponse (ListCreateAccountStatusResponse'),
    newListCreateAccountStatusResponse,

    -- ** ListDelegatedAdministrators (Paginated)
    ListDelegatedAdministrators (ListDelegatedAdministrators'),
    newListDelegatedAdministrators,
    ListDelegatedAdministratorsResponse (ListDelegatedAdministratorsResponse'),
    newListDelegatedAdministratorsResponse,

    -- ** ListDelegatedServicesForAccount (Paginated)
    ListDelegatedServicesForAccount (ListDelegatedServicesForAccount'),
    newListDelegatedServicesForAccount,
    ListDelegatedServicesForAccountResponse (ListDelegatedServicesForAccountResponse'),
    newListDelegatedServicesForAccountResponse,

    -- ** ListHandshakesForAccount (Paginated)
    ListHandshakesForAccount (ListHandshakesForAccount'),
    newListHandshakesForAccount,
    ListHandshakesForAccountResponse (ListHandshakesForAccountResponse'),
    newListHandshakesForAccountResponse,

    -- ** ListHandshakesForOrganization (Paginated)
    ListHandshakesForOrganization (ListHandshakesForOrganization'),
    newListHandshakesForOrganization,
    ListHandshakesForOrganizationResponse (ListHandshakesForOrganizationResponse'),
    newListHandshakesForOrganizationResponse,

    -- ** ListOrganizationalUnitsForParent (Paginated)
    ListOrganizationalUnitsForParent (ListOrganizationalUnitsForParent'),
    newListOrganizationalUnitsForParent,
    ListOrganizationalUnitsForParentResponse (ListOrganizationalUnitsForParentResponse'),
    newListOrganizationalUnitsForParentResponse,

    -- ** ListParents (Paginated)
    ListParents (ListParents'),
    newListParents,
    ListParentsResponse (ListParentsResponse'),
    newListParentsResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** ListPoliciesForTarget (Paginated)
    ListPoliciesForTarget (ListPoliciesForTarget'),
    newListPoliciesForTarget,
    ListPoliciesForTargetResponse (ListPoliciesForTargetResponse'),
    newListPoliciesForTargetResponse,

    -- ** ListRoots (Paginated)
    ListRoots (ListRoots'),
    newListRoots,
    ListRootsResponse (ListRootsResponse'),
    newListRootsResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTargetsForPolicy (Paginated)
    ListTargetsForPolicy (ListTargetsForPolicy'),
    newListTargetsForPolicy,
    ListTargetsForPolicyResponse (ListTargetsForPolicyResponse'),
    newListTargetsForPolicyResponse,

    -- ** MoveAccount
    MoveAccount (MoveAccount'),
    newMoveAccount,
    MoveAccountResponse (MoveAccountResponse'),
    newMoveAccountResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** RegisterDelegatedAdministrator
    RegisterDelegatedAdministrator (RegisterDelegatedAdministrator'),
    newRegisterDelegatedAdministrator,
    RegisterDelegatedAdministratorResponse (RegisterDelegatedAdministratorResponse'),
    newRegisterDelegatedAdministratorResponse,

    -- ** RemoveAccountFromOrganization
    RemoveAccountFromOrganization (RemoveAccountFromOrganization'),
    newRemoveAccountFromOrganization,
    RemoveAccountFromOrganizationResponse (RemoveAccountFromOrganizationResponse'),
    newRemoveAccountFromOrganizationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateOrganizationalUnit
    UpdateOrganizationalUnit (UpdateOrganizationalUnit'),
    newUpdateOrganizationalUnit,
    UpdateOrganizationalUnitResponse (UpdateOrganizationalUnitResponse'),
    newUpdateOrganizationalUnitResponse,

    -- ** UpdatePolicy
    UpdatePolicy (UpdatePolicy'),
    newUpdatePolicy,
    UpdatePolicyResponse (UpdatePolicyResponse'),
    newUpdatePolicyResponse,

    -- * Types

    -- ** AccountJoinedMethod
    AccountJoinedMethod (..),

    -- ** AccountStatus
    AccountStatus (..),

    -- ** ActionType
    ActionType (..),

    -- ** ChildType
    ChildType (..),

    -- ** CreateAccountFailureReason
    CreateAccountFailureReason (..),

    -- ** CreateAccountState
    CreateAccountState (..),

    -- ** EffectivePolicyType
    EffectivePolicyType (..),

    -- ** HandshakePartyType
    HandshakePartyType (..),

    -- ** HandshakeResourceType
    HandshakeResourceType (..),

    -- ** HandshakeState
    HandshakeState (..),

    -- ** IAMUserAccessToBilling
    IAMUserAccessToBilling (..),

    -- ** OrganizationFeatureSet
    OrganizationFeatureSet (..),

    -- ** ParentType
    ParentType (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** PolicyTypeStatus
    PolicyTypeStatus (..),

    -- ** TargetType
    TargetType (..),

    -- ** Account
    Account (Account'),
    newAccount,

    -- ** Child
    Child (Child'),
    newChild,

    -- ** CreateAccountStatus
    CreateAccountStatus (CreateAccountStatus'),
    newCreateAccountStatus,

    -- ** DelegatedAdministrator
    DelegatedAdministrator (DelegatedAdministrator'),
    newDelegatedAdministrator,

    -- ** DelegatedService
    DelegatedService (DelegatedService'),
    newDelegatedService,

    -- ** EffectivePolicy
    EffectivePolicy (EffectivePolicy'),
    newEffectivePolicy,

    -- ** EnabledServicePrincipal
    EnabledServicePrincipal (EnabledServicePrincipal'),
    newEnabledServicePrincipal,

    -- ** Handshake
    Handshake (Handshake'),
    newHandshake,

    -- ** HandshakeFilter
    HandshakeFilter (HandshakeFilter'),
    newHandshakeFilter,

    -- ** HandshakeParty
    HandshakeParty (HandshakeParty'),
    newHandshakeParty,

    -- ** HandshakeResource
    HandshakeResource (HandshakeResource'),
    newHandshakeResource,

    -- ** Organization
    Organization (Organization'),
    newOrganization,

    -- ** OrganizationalUnit
    OrganizationalUnit (OrganizationalUnit'),
    newOrganizationalUnit,

    -- ** Parent
    Parent (Parent'),
    newParent,

    -- ** Policy
    Policy (Policy'),
    newPolicy,

    -- ** PolicySummary
    PolicySummary (PolicySummary'),
    newPolicySummary,

    -- ** PolicyTargetSummary
    PolicyTargetSummary (PolicyTargetSummary'),
    newPolicyTargetSummary,

    -- ** PolicyTypeSummary
    PolicyTypeSummary (PolicyTypeSummary'),
    newPolicyTypeSummary,

    -- ** ResourcePolicy
    ResourcePolicy (ResourcePolicy'),
    newResourcePolicy,

    -- ** ResourcePolicySummary
    ResourcePolicySummary (ResourcePolicySummary'),
    newResourcePolicySummary,

    -- ** Root
    Root (Root'),
    newRoot,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.Organizations.AcceptHandshake
import Amazonka.Organizations.AttachPolicy
import Amazonka.Organizations.CancelHandshake
import Amazonka.Organizations.CloseAccount
import Amazonka.Organizations.CreateAccount
import Amazonka.Organizations.CreateGovCloudAccount
import Amazonka.Organizations.CreateOrganization
import Amazonka.Organizations.CreateOrganizationalUnit
import Amazonka.Organizations.CreatePolicy
import Amazonka.Organizations.DeclineHandshake
import Amazonka.Organizations.DeleteOrganization
import Amazonka.Organizations.DeleteOrganizationalUnit
import Amazonka.Organizations.DeletePolicy
import Amazonka.Organizations.DeleteResourcePolicy
import Amazonka.Organizations.DeregisterDelegatedAdministrator
import Amazonka.Organizations.DescribeAccount
import Amazonka.Organizations.DescribeCreateAccountStatus
import Amazonka.Organizations.DescribeEffectivePolicy
import Amazonka.Organizations.DescribeHandshake
import Amazonka.Organizations.DescribeOrganization
import Amazonka.Organizations.DescribeOrganizationalUnit
import Amazonka.Organizations.DescribePolicy
import Amazonka.Organizations.DescribeResourcePolicy
import Amazonka.Organizations.DetachPolicy
import Amazonka.Organizations.DisableAWSServiceAccess
import Amazonka.Organizations.DisablePolicyType
import Amazonka.Organizations.EnableAWSServiceAccess
import Amazonka.Organizations.EnableAllFeatures
import Amazonka.Organizations.EnablePolicyType
import Amazonka.Organizations.InviteAccountToOrganization
import Amazonka.Organizations.LeaveOrganization
import Amazonka.Organizations.Lens
import Amazonka.Organizations.ListAWSServiceAccessForOrganization
import Amazonka.Organizations.ListAccounts
import Amazonka.Organizations.ListAccountsForParent
import Amazonka.Organizations.ListChildren
import Amazonka.Organizations.ListCreateAccountStatus
import Amazonka.Organizations.ListDelegatedAdministrators
import Amazonka.Organizations.ListDelegatedServicesForAccount
import Amazonka.Organizations.ListHandshakesForAccount
import Amazonka.Organizations.ListHandshakesForOrganization
import Amazonka.Organizations.ListOrganizationalUnitsForParent
import Amazonka.Organizations.ListParents
import Amazonka.Organizations.ListPolicies
import Amazonka.Organizations.ListPoliciesForTarget
import Amazonka.Organizations.ListRoots
import Amazonka.Organizations.ListTagsForResource
import Amazonka.Organizations.ListTargetsForPolicy
import Amazonka.Organizations.MoveAccount
import Amazonka.Organizations.PutResourcePolicy
import Amazonka.Organizations.RegisterDelegatedAdministrator
import Amazonka.Organizations.RemoveAccountFromOrganization
import Amazonka.Organizations.TagResource
import Amazonka.Organizations.Types
import Amazonka.Organizations.UntagResource
import Amazonka.Organizations.UpdateOrganizationalUnit
import Amazonka.Organizations.UpdatePolicy
import Amazonka.Organizations.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Organizations'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
