{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Organizations
module Network.AWS.Organizations
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** OrganizationalUnitNotEmptyException
    _OrganizationalUnitNotEmptyException,

    -- ** PolicyTypeNotEnabledException
    _PolicyTypeNotEnabledException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** DuplicateHandshakeException
    _DuplicateHandshakeException,

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

    -- ** ConstraintViolationException
    _ConstraintViolationException,

    -- ** AWSOrganizationsNotInUseException
    _AWSOrganizationsNotInUseException,

    -- ** PolicyTypeNotAvailableForOrganizationException
    _PolicyTypeNotAvailableForOrganizationException,

    -- ** HandshakeNotFoundException
    _HandshakeNotFoundException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** ParentNotFoundException
    _ParentNotFoundException,

    -- ** DuplicatePolicyException
    _DuplicatePolicyException,

    -- ** AlreadyInOrganizationException
    _AlreadyInOrganizationException,

    -- ** CreateAccountStatusNotFoundException
    _CreateAccountStatusNotFoundException,

    -- ** MasterCannotLeaveOrganizationException
    _MasterCannotLeaveOrganizationException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** PolicyNotFoundException
    _PolicyNotFoundException,

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

    -- ** ChildNotFoundException
    _ChildNotFoundException,

    -- ** InvalidHandshakeTransitionException
    _InvalidHandshakeTransitionException,

    -- ** FinalizingOrganizationException
    _FinalizingOrganizationException,

    -- ** PolicyInUseException
    _PolicyInUseException,

    -- ** AccountNotRegisteredException
    _AccountNotRegisteredException,

    -- ** PolicyChangesInProgressException
    _PolicyChangesInProgressException,

    -- ** AccountAlreadyRegisteredException
    _AccountAlreadyRegisteredException,

    -- ** PolicyNotAttachedException
    _PolicyNotAttachedException,

    -- ** DuplicateOrganizationalUnitException
    _DuplicateOrganizationalUnitException,

    -- ** AccessDeniedForDependencyException
    _AccessDeniedForDependencyException,

    -- ** TargetNotFoundException
    _TargetNotFoundException,

    -- ** SourceParentNotFoundException
    _SourceParentNotFoundException,

    -- ** AccountNotFoundException
    _AccountNotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ServiceException
    _ServiceException,

    -- ** RootNotFoundException
    _RootNotFoundException,

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

    -- ** CreatePolicy
    CreatePolicy (CreatePolicy'),
    newCreatePolicy,
    CreatePolicyResponse (CreatePolicyResponse'),
    newCreatePolicyResponse,

    -- ** DisablePolicyType
    DisablePolicyType (DisablePolicyType'),
    newDisablePolicyType,
    DisablePolicyTypeResponse (DisablePolicyTypeResponse'),
    newDisablePolicyTypeResponse,

    -- ** DescribeCreateAccountStatus
    DescribeCreateAccountStatus (DescribeCreateAccountStatus'),
    newDescribeCreateAccountStatus,
    DescribeCreateAccountStatusResponse (DescribeCreateAccountStatusResponse'),
    newDescribeCreateAccountStatusResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** ListHandshakesForAccount (Paginated)
    ListHandshakesForAccount (ListHandshakesForAccount'),
    newListHandshakesForAccount,
    ListHandshakesForAccountResponse (ListHandshakesForAccountResponse'),
    newListHandshakesForAccountResponse,

    -- ** ListChildren (Paginated)
    ListChildren (ListChildren'),
    newListChildren,
    ListChildrenResponse (ListChildrenResponse'),
    newListChildrenResponse,

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

    -- ** UpdatePolicy
    UpdatePolicy (UpdatePolicy'),
    newUpdatePolicy,
    UpdatePolicyResponse (UpdatePolicyResponse'),
    newUpdatePolicyResponse,

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

    -- ** ListCreateAccountStatus (Paginated)
    ListCreateAccountStatus (ListCreateAccountStatus'),
    newListCreateAccountStatus,
    ListCreateAccountStatusResponse (ListCreateAccountStatusResponse'),
    newListCreateAccountStatusResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListAccounts (Paginated)
    ListAccounts (ListAccounts'),
    newListAccounts,
    ListAccountsResponse (ListAccountsResponse'),
    newListAccountsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** EnableAWSServiceAccess
    EnableAWSServiceAccess (EnableAWSServiceAccess'),
    newEnableAWSServiceAccess,
    EnableAWSServiceAccessResponse (EnableAWSServiceAccessResponse'),
    newEnableAWSServiceAccessResponse,

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

    -- ** RemoveAccountFromOrganization
    RemoveAccountFromOrganization (RemoveAccountFromOrganization'),
    newRemoveAccountFromOrganization,
    RemoveAccountFromOrganizationResponse (RemoveAccountFromOrganizationResponse'),
    newRemoveAccountFromOrganizationResponse,

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

    -- ** AcceptHandshake
    AcceptHandshake (AcceptHandshake'),
    newAcceptHandshake,
    AcceptHandshakeResponse (AcceptHandshakeResponse'),
    newAcceptHandshakeResponse,

    -- ** DetachPolicy
    DetachPolicy (DetachPolicy'),
    newDetachPolicy,
    DetachPolicyResponse (DetachPolicyResponse'),
    newDetachPolicyResponse,

    -- ** CreateOrganizationalUnit
    CreateOrganizationalUnit (CreateOrganizationalUnit'),
    newCreateOrganizationalUnit,
    CreateOrganizationalUnitResponse (CreateOrganizationalUnitResponse'),
    newCreateOrganizationalUnitResponse,

    -- ** DescribeAccount
    DescribeAccount (DescribeAccount'),
    newDescribeAccount,
    DescribeAccountResponse (DescribeAccountResponse'),
    newDescribeAccountResponse,

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

    -- ** DeleteOrganizationalUnit
    DeleteOrganizationalUnit (DeleteOrganizationalUnit'),
    newDeleteOrganizationalUnit,
    DeleteOrganizationalUnitResponse (DeleteOrganizationalUnitResponse'),
    newDeleteOrganizationalUnitResponse,

    -- ** CancelHandshake
    CancelHandshake (CancelHandshake'),
    newCancelHandshake,
    CancelHandshakeResponse (CancelHandshakeResponse'),
    newCancelHandshakeResponse,

    -- ** RegisterDelegatedAdministrator
    RegisterDelegatedAdministrator (RegisterDelegatedAdministrator'),
    newRegisterDelegatedAdministrator,
    RegisterDelegatedAdministratorResponse (RegisterDelegatedAdministratorResponse'),
    newRegisterDelegatedAdministratorResponse,

    -- ** ListHandshakesForOrganization (Paginated)
    ListHandshakesForOrganization (ListHandshakesForOrganization'),
    newListHandshakesForOrganization,
    ListHandshakesForOrganizationResponse (ListHandshakesForOrganizationResponse'),
    newListHandshakesForOrganizationResponse,

    -- ** ListPoliciesForTarget (Paginated)
    ListPoliciesForTarget (ListPoliciesForTarget'),
    newListPoliciesForTarget,
    ListPoliciesForTargetResponse (ListPoliciesForTargetResponse'),
    newListPoliciesForTargetResponse,

    -- ** ListOrganizationalUnitsForParent (Paginated)
    ListOrganizationalUnitsForParent (ListOrganizationalUnitsForParent'),
    newListOrganizationalUnitsForParent,
    ListOrganizationalUnitsForParentResponse (ListOrganizationalUnitsForParentResponse'),
    newListOrganizationalUnitsForParentResponse,

    -- ** ListTargetsForPolicy (Paginated)
    ListTargetsForPolicy (ListTargetsForPolicy'),
    newListTargetsForPolicy,
    ListTargetsForPolicyResponse (ListTargetsForPolicyResponse'),
    newListTargetsForPolicyResponse,

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

    -- ** DescribeEffectivePolicy
    DescribeEffectivePolicy (DescribeEffectivePolicy'),
    newDescribeEffectivePolicy,
    DescribeEffectivePolicyResponse (DescribeEffectivePolicyResponse'),
    newDescribeEffectivePolicyResponse,

    -- ** DeregisterDelegatedAdministrator
    DeregisterDelegatedAdministrator (DeregisterDelegatedAdministrator'),
    newDeregisterDelegatedAdministrator,
    DeregisterDelegatedAdministratorResponse (DeregisterDelegatedAdministratorResponse'),
    newDeregisterDelegatedAdministratorResponse,

    -- ** MoveAccount
    MoveAccount (MoveAccount'),
    newMoveAccount,
    MoveAccountResponse (MoveAccountResponse'),
    newMoveAccountResponse,

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

    -- ** DisableAWSServiceAccess
    DisableAWSServiceAccess (DisableAWSServiceAccess'),
    newDisableAWSServiceAccess,
    DisableAWSServiceAccessResponse (DisableAWSServiceAccessResponse'),
    newDisableAWSServiceAccessResponse,

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

    -- ** CreateAccount
    CreateAccount (CreateAccount'),
    newCreateAccount,
    CreateAccountResponse (CreateAccountResponse'),
    newCreateAccountResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeHandshake
    DescribeHandshake (DescribeHandshake'),
    newDescribeHandshake,
    DescribeHandshakeResponse (DescribeHandshakeResponse'),
    newDescribeHandshakeResponse,

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
