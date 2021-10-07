{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Organizations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-11-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Organizations is a web service that enables you to consolidate your
-- multiple AWS accounts into an /organization/ and centrally manage your
-- accounts and their resources.
--
-- This guide provides descriptions of the Organizations operations. For
-- more information about using this service, see the
-- <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html AWS Organizations User Guide>.
--
-- __Support and feedback for AWS Organizations__
--
-- We welcome your feedback. Send your comments to
-- <mailto:feedback-awsorganizations@amazon.com feedback-awsorganizations\@amazon.com>
-- or post your feedback and questions in the
-- <http://forums.aws.amazon.com/forum.jspa?forumID=219 AWS Organizations support forum>.
-- For more information about the AWS support forums, see
-- <http://forums.aws.amazon.com/help.jspa Forums Help>.
--
-- __Endpoint to call When using the AWS CLI or the AWS SDK__
--
-- For the current release of Organizations, specify the @us-east-1@ region
-- for all AWS API and AWS CLI calls made from the commercial AWS Regions
-- outside of China. If calling from one of the AWS Regions in China, then
-- specify @cn-northwest-1@. You can do this in the AWS CLI by using these
-- parameters and commands:
--
-- -   Use the following parameter with each command to specify both the
--     endpoint and its region:
--
--     @--endpoint-url https:\/\/organizations.us-east-1.amazonaws.com@
--     /(from commercial AWS Regions outside of China)/
--
--     or
--
--     @--endpoint-url https:\/\/organizations.cn-northwest-1.amazonaws.com.cn@
--     /(from AWS Regions in China)/
--
-- -   Use the default endpoint, but configure your default region with
--     this command:
--
--     @aws configure set default.region us-east-1@ /(from commercial AWS
--     Regions outside of China)/
--
--     or
--
--     @aws configure set default.region cn-northwest-1@ /(from AWS Regions
--     in China)/
--
-- -   Use the following parameter with each command to specify the
--     endpoint:
--
--     @--region us-east-1@ /(from commercial AWS Regions outside of
--     China)/
--
--     or
--
--     @--region cn-northwest-1@ /(from AWS Regions in China)/
--
-- __Recording API Requests__
--
-- AWS Organizations supports AWS CloudTrail, a service that records AWS
-- API calls for your AWS account and delivers log files to an Amazon S3
-- bucket. By using information collected by AWS CloudTrail, you can
-- determine which requests the Organizations service received, who made
-- the request and when, and so on. For more about AWS Organizations and
-- its support for AWS CloudTrail, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_incident-response.html#orgs_cloudtrail-integration Logging AWS Organizations Events with AWS CloudTrail>
-- in the /AWS Organizations User Guide/. To learn more about AWS
-- CloudTrail, including how to turn it on and find your log files, see the
-- <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html AWS CloudTrail User Guide>.
module Network.AWS.Organizations
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** OrganizationalUnitNotEmptyException
    _OrganizationalUnitNotEmptyException,

    -- ** DuplicateHandshakeException
    _DuplicateHandshakeException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** PolicyTypeNotEnabledException
    _PolicyTypeNotEnabledException,

    -- ** OrganizationNotEmptyException
    _OrganizationNotEmptyException,

    -- ** OrganizationalUnitNotFoundException
    _OrganizationalUnitNotFoundException,

    -- ** DuplicateAccountException
    _DuplicateAccountException,

    -- ** EffectivePolicyNotFoundException
    _EffectivePolicyNotFoundException,

    -- ** HandshakeAlreadyInStateException
    _HandshakeAlreadyInStateException,

    -- ** HandshakeNotFoundException
    _HandshakeNotFoundException,

    -- ** ConstraintViolationException
    _ConstraintViolationException,

    -- ** PolicyTypeNotAvailableForOrganizationException
    _PolicyTypeNotAvailableForOrganizationException,

    -- ** AWSOrganizationsNotInUseException
    _AWSOrganizationsNotInUseException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** ParentNotFoundException
    _ParentNotFoundException,

    -- ** DuplicatePolicyException
    _DuplicatePolicyException,

    -- ** CreateAccountStatusNotFoundException
    _CreateAccountStatusNotFoundException,

    -- ** AlreadyInOrganizationException
    _AlreadyInOrganizationException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** MasterCannotLeaveOrganizationException
    _MasterCannotLeaveOrganizationException,

    -- ** PolicyNotFoundException
    _PolicyNotFoundException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** PolicyTypeAlreadyEnabledException
    _PolicyTypeAlreadyEnabledException,

    -- ** AccountOwnerNotVerifiedException
    _AccountOwnerNotVerifiedException,

    -- ** HandshakeConstraintViolationException
    _HandshakeConstraintViolationException,

    -- ** DestinationParentNotFoundException
    _DestinationParentNotFoundException,

    -- ** DuplicatePolicyAttachmentException
    _DuplicatePolicyAttachmentException,

    -- ** UnsupportedAPIEndpointException
    _UnsupportedAPIEndpointException,

    -- ** InvalidHandshakeTransitionException
    _InvalidHandshakeTransitionException,

    -- ** ChildNotFoundException
    _ChildNotFoundException,

    -- ** AccountNotRegisteredException
    _AccountNotRegisteredException,

    -- ** FinalizingOrganizationException
    _FinalizingOrganizationException,

    -- ** PolicyInUseException
    _PolicyInUseException,

    -- ** PolicyNotAttachedException
    _PolicyNotAttachedException,

    -- ** AccountAlreadyRegisteredException
    _AccountAlreadyRegisteredException,

    -- ** DuplicateOrganizationalUnitException
    _DuplicateOrganizationalUnitException,

    -- ** PolicyChangesInProgressException
    _PolicyChangesInProgressException,

    -- ** AccessDeniedForDependencyException
    _AccessDeniedForDependencyException,

    -- ** SourceParentNotFoundException
    _SourceParentNotFoundException,

    -- ** AccountNotFoundException
    _AccountNotFoundException,

    -- ** TargetNotFoundException
    _TargetNotFoundException,

    -- ** ServiceException
    _ServiceException,

    -- ** RootNotFoundException
    _RootNotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListAccountsForParent (Paginated)
    ListAccountsForParent (ListAccountsForParent'),
    newListAccountsForParent,
    ListAccountsForParentResponse (ListAccountsForParentResponse'),
    newListAccountsForParentResponse,

    -- ** CreateOrganization
    CreateOrganization (CreateOrganization'),
    newCreateOrganization,
    CreateOrganizationResponse (CreateOrganizationResponse'),
    newCreateOrganizationResponse,

    -- ** DescribeCreateAccountStatus
    DescribeCreateAccountStatus (DescribeCreateAccountStatus'),
    newDescribeCreateAccountStatus,
    DescribeCreateAccountStatusResponse (DescribeCreateAccountStatusResponse'),
    newDescribeCreateAccountStatusResponse,

    -- ** DisablePolicyType
    DisablePolicyType (DisablePolicyType'),
    newDisablePolicyType,
    DisablePolicyTypeResponse (DisablePolicyTypeResponse'),
    newDisablePolicyTypeResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** CreatePolicy
    CreatePolicy (CreatePolicy'),
    newCreatePolicy,
    CreatePolicyResponse (CreatePolicyResponse'),
    newCreatePolicyResponse,

    -- ** ListHandshakesForAccount (Paginated)
    ListHandshakesForAccount (ListHandshakesForAccount'),
    newListHandshakesForAccount,
    ListHandshakesForAccountResponse (ListHandshakesForAccountResponse'),
    newListHandshakesForAccountResponse,

    -- ** UpdatePolicy
    UpdatePolicy (UpdatePolicy'),
    newUpdatePolicy,
    UpdatePolicyResponse (UpdatePolicyResponse'),
    newUpdatePolicyResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** EnablePolicyType
    EnablePolicyType (EnablePolicyType'),
    newEnablePolicyType,
    EnablePolicyTypeResponse (EnablePolicyTypeResponse'),
    newEnablePolicyTypeResponse,

    -- ** ListChildren (Paginated)
    ListChildren (ListChildren'),
    newListChildren,
    ListChildrenResponse (ListChildrenResponse'),
    newListChildrenResponse,

    -- ** ListAWSServiceAccessForOrganization (Paginated)
    ListAWSServiceAccessForOrganization (ListAWSServiceAccessForOrganization'),
    newListAWSServiceAccessForOrganization,
    ListAWSServiceAccessForOrganizationResponse (ListAWSServiceAccessForOrganizationResponse'),
    newListAWSServiceAccessForOrganizationResponse,

    -- ** DescribeOrganization
    DescribeOrganization (DescribeOrganization'),
    newDescribeOrganization,
    DescribeOrganizationResponse (DescribeOrganizationResponse'),
    newDescribeOrganizationResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListCreateAccountStatus (Paginated)
    ListCreateAccountStatus (ListCreateAccountStatus'),
    newListCreateAccountStatus,
    ListCreateAccountStatusResponse (ListCreateAccountStatusResponse'),
    newListCreateAccountStatusResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListAccounts (Paginated)
    ListAccounts (ListAccounts'),
    newListAccounts,
    ListAccountsResponse (ListAccountsResponse'),
    newListAccountsResponse,

    -- ** DescribeOrganizationalUnit
    DescribeOrganizationalUnit (DescribeOrganizationalUnit'),
    newDescribeOrganizationalUnit,
    DescribeOrganizationalUnitResponse (DescribeOrganizationalUnitResponse'),
    newDescribeOrganizationalUnitResponse,

    -- ** ListDelegatedServicesForAccount (Paginated)
    ListDelegatedServicesForAccount (ListDelegatedServicesForAccount'),
    newListDelegatedServicesForAccount,
    ListDelegatedServicesForAccountResponse (ListDelegatedServicesForAccountResponse'),
    newListDelegatedServicesForAccountResponse,

    -- ** EnableAWSServiceAccess
    EnableAWSServiceAccess (EnableAWSServiceAccess'),
    newEnableAWSServiceAccess,
    EnableAWSServiceAccessResponse (EnableAWSServiceAccessResponse'),
    newEnableAWSServiceAccessResponse,

    -- ** CreateGovCloudAccount
    CreateGovCloudAccount (CreateGovCloudAccount'),
    newCreateGovCloudAccount,
    CreateGovCloudAccountResponse (CreateGovCloudAccountResponse'),
    newCreateGovCloudAccountResponse,

    -- ** DeleteOrganization
    DeleteOrganization (DeleteOrganization'),
    newDeleteOrganization,
    DeleteOrganizationResponse (DeleteOrganizationResponse'),
    newDeleteOrganizationResponse,

    -- ** RemoveAccountFromOrganization
    RemoveAccountFromOrganization (RemoveAccountFromOrganization'),
    newRemoveAccountFromOrganization,
    RemoveAccountFromOrganizationResponse (RemoveAccountFromOrganizationResponse'),
    newRemoveAccountFromOrganizationResponse,

    -- ** AcceptHandshake
    AcceptHandshake (AcceptHandshake'),
    newAcceptHandshake,
    AcceptHandshakeResponse (AcceptHandshakeResponse'),
    newAcceptHandshakeResponse,

    -- ** ListRoots (Paginated)
    ListRoots (ListRoots'),
    newListRoots,
    ListRootsResponse (ListRootsResponse'),
    newListRootsResponse,

    -- ** EnableAllFeatures
    EnableAllFeatures (EnableAllFeatures'),
    newEnableAllFeatures,
    EnableAllFeaturesResponse (EnableAllFeaturesResponse'),
    newEnableAllFeaturesResponse,

    -- ** DetachPolicy
    DetachPolicy (DetachPolicy'),
    newDetachPolicy,
    DetachPolicyResponse (DetachPolicyResponse'),
    newDetachPolicyResponse,

    -- ** DescribeAccount
    DescribeAccount (DescribeAccount'),
    newDescribeAccount,
    DescribeAccountResponse (DescribeAccountResponse'),
    newDescribeAccountResponse,

    -- ** CreateOrganizationalUnit
    CreateOrganizationalUnit (CreateOrganizationalUnit'),
    newCreateOrganizationalUnit,
    CreateOrganizationalUnitResponse (CreateOrganizationalUnitResponse'),
    newCreateOrganizationalUnitResponse,

    -- ** RegisterDelegatedAdministrator
    RegisterDelegatedAdministrator (RegisterDelegatedAdministrator'),
    newRegisterDelegatedAdministrator,
    RegisterDelegatedAdministratorResponse (RegisterDelegatedAdministratorResponse'),
    newRegisterDelegatedAdministratorResponse,

    -- ** CancelHandshake
    CancelHandshake (CancelHandshake'),
    newCancelHandshake,
    CancelHandshakeResponse (CancelHandshakeResponse'),
    newCancelHandshakeResponse,

    -- ** DeleteOrganizationalUnit
    DeleteOrganizationalUnit (DeleteOrganizationalUnit'),
    newDeleteOrganizationalUnit,
    DeleteOrganizationalUnitResponse (DeleteOrganizationalUnitResponse'),
    newDeleteOrganizationalUnitResponse,

    -- ** ListDelegatedAdministrators (Paginated)
    ListDelegatedAdministrators (ListDelegatedAdministrators'),
    newListDelegatedAdministrators,
    ListDelegatedAdministratorsResponse (ListDelegatedAdministratorsResponse'),
    newListDelegatedAdministratorsResponse,

    -- ** UpdateOrganizationalUnit
    UpdateOrganizationalUnit (UpdateOrganizationalUnit'),
    newUpdateOrganizationalUnit,
    UpdateOrganizationalUnitResponse (UpdateOrganizationalUnitResponse'),
    newUpdateOrganizationalUnitResponse,

    -- ** ListOrganizationalUnitsForParent (Paginated)
    ListOrganizationalUnitsForParent (ListOrganizationalUnitsForParent'),
    newListOrganizationalUnitsForParent,
    ListOrganizationalUnitsForParentResponse (ListOrganizationalUnitsForParentResponse'),
    newListOrganizationalUnitsForParentResponse,

    -- ** ListPoliciesForTarget (Paginated)
    ListPoliciesForTarget (ListPoliciesForTarget'),
    newListPoliciesForTarget,
    ListPoliciesForTargetResponse (ListPoliciesForTargetResponse'),
    newListPoliciesForTargetResponse,

    -- ** ListHandshakesForOrganization (Paginated)
    ListHandshakesForOrganization (ListHandshakesForOrganization'),
    newListHandshakesForOrganization,
    ListHandshakesForOrganizationResponse (ListHandshakesForOrganizationResponse'),
    newListHandshakesForOrganizationResponse,

    -- ** AttachPolicy
    AttachPolicy (AttachPolicy'),
    newAttachPolicy,
    AttachPolicyResponse (AttachPolicyResponse'),
    newAttachPolicyResponse,

    -- ** DeclineHandshake
    DeclineHandshake (DeclineHandshake'),
    newDeclineHandshake,
    DeclineHandshakeResponse (DeclineHandshakeResponse'),
    newDeclineHandshakeResponse,

    -- ** ListTargetsForPolicy (Paginated)
    ListTargetsForPolicy (ListTargetsForPolicy'),
    newListTargetsForPolicy,
    ListTargetsForPolicyResponse (ListTargetsForPolicyResponse'),
    newListTargetsForPolicyResponse,

    -- ** DescribeEffectivePolicy
    DescribeEffectivePolicy (DescribeEffectivePolicy'),
    newDescribeEffectivePolicy,
    DescribeEffectivePolicyResponse (DescribeEffectivePolicyResponse'),
    newDescribeEffectivePolicyResponse,

    -- ** InviteAccountToOrganization
    InviteAccountToOrganization (InviteAccountToOrganization'),
    newInviteAccountToOrganization,
    InviteAccountToOrganizationResponse (InviteAccountToOrganizationResponse'),
    newInviteAccountToOrganizationResponse,

    -- ** DisableAWSServiceAccess
    DisableAWSServiceAccess (DisableAWSServiceAccess'),
    newDisableAWSServiceAccess,
    DisableAWSServiceAccessResponse (DisableAWSServiceAccessResponse'),
    newDisableAWSServiceAccessResponse,

    -- ** MoveAccount
    MoveAccount (MoveAccount'),
    newMoveAccount,
    MoveAccountResponse (MoveAccountResponse'),
    newMoveAccountResponse,

    -- ** LeaveOrganization
    LeaveOrganization (LeaveOrganization'),
    newLeaveOrganization,
    LeaveOrganizationResponse (LeaveOrganizationResponse'),
    newLeaveOrganizationResponse,

    -- ** DeregisterDelegatedAdministrator
    DeregisterDelegatedAdministrator (DeregisterDelegatedAdministrator'),
    newDeregisterDelegatedAdministrator,
    DeregisterDelegatedAdministratorResponse (DeregisterDelegatedAdministratorResponse'),
    newDeregisterDelegatedAdministratorResponse,

    -- ** CreateAccount
    CreateAccount (CreateAccount'),
    newCreateAccount,
    CreateAccountResponse (CreateAccountResponse'),
    newCreateAccountResponse,

    -- ** ListParents (Paginated)
    ListParents (ListParents'),
    newListParents,
    ListParentsResponse (ListParentsResponse'),
    newListParentsResponse,

    -- ** DescribePolicy
    DescribePolicy (DescribePolicy'),
    newDescribePolicy,
    DescribePolicyResponse (DescribePolicyResponse'),
    newDescribePolicyResponse,

    -- ** DescribeHandshake
    DescribeHandshake (DescribeHandshake'),
    newDescribeHandshake,
    DescribeHandshakeResponse (DescribeHandshakeResponse'),
    newDescribeHandshakeResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** Root
    Root (Root'),
    newRoot,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.Organizations.AcceptHandshake
import Network.AWS.Organizations.AttachPolicy
import Network.AWS.Organizations.CancelHandshake
import Network.AWS.Organizations.CreateAccount
import Network.AWS.Organizations.CreateGovCloudAccount
import Network.AWS.Organizations.CreateOrganization
import Network.AWS.Organizations.CreateOrganizationalUnit
import Network.AWS.Organizations.CreatePolicy
import Network.AWS.Organizations.DeclineHandshake
import Network.AWS.Organizations.DeleteOrganization
import Network.AWS.Organizations.DeleteOrganizationalUnit
import Network.AWS.Organizations.DeletePolicy
import Network.AWS.Organizations.DeregisterDelegatedAdministrator
import Network.AWS.Organizations.DescribeAccount
import Network.AWS.Organizations.DescribeCreateAccountStatus
import Network.AWS.Organizations.DescribeEffectivePolicy
import Network.AWS.Organizations.DescribeHandshake
import Network.AWS.Organizations.DescribeOrganization
import Network.AWS.Organizations.DescribeOrganizationalUnit
import Network.AWS.Organizations.DescribePolicy
import Network.AWS.Organizations.DetachPolicy
import Network.AWS.Organizations.DisableAWSServiceAccess
import Network.AWS.Organizations.DisablePolicyType
import Network.AWS.Organizations.EnableAWSServiceAccess
import Network.AWS.Organizations.EnableAllFeatures
import Network.AWS.Organizations.EnablePolicyType
import Network.AWS.Organizations.InviteAccountToOrganization
import Network.AWS.Organizations.LeaveOrganization
import Network.AWS.Organizations.Lens
import Network.AWS.Organizations.ListAWSServiceAccessForOrganization
import Network.AWS.Organizations.ListAccounts
import Network.AWS.Organizations.ListAccountsForParent
import Network.AWS.Organizations.ListChildren
import Network.AWS.Organizations.ListCreateAccountStatus
import Network.AWS.Organizations.ListDelegatedAdministrators
import Network.AWS.Organizations.ListDelegatedServicesForAccount
import Network.AWS.Organizations.ListHandshakesForAccount
import Network.AWS.Organizations.ListHandshakesForOrganization
import Network.AWS.Organizations.ListOrganizationalUnitsForParent
import Network.AWS.Organizations.ListParents
import Network.AWS.Organizations.ListPolicies
import Network.AWS.Organizations.ListPoliciesForTarget
import Network.AWS.Organizations.ListRoots
import Network.AWS.Organizations.ListTagsForResource
import Network.AWS.Organizations.ListTargetsForPolicy
import Network.AWS.Organizations.MoveAccount
import Network.AWS.Organizations.RegisterDelegatedAdministrator
import Network.AWS.Organizations.RemoveAccountFromOrganization
import Network.AWS.Organizations.TagResource
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.UntagResource
import Network.AWS.Organizations.UpdateOrganizationalUnit
import Network.AWS.Organizations.UpdatePolicy
import Network.AWS.Organizations.Waiters

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
