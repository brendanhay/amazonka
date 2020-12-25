{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Organizations__
module Network.AWS.Organizations
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** PolicyNotFoundException
    _PolicyNotFoundException,

    -- ** PolicyTypeAlreadyEnabledException
    _PolicyTypeAlreadyEnabledException,

    -- ** HandshakeConstraintViolationException
    _HandshakeConstraintViolationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** RootNotFoundException
    _RootNotFoundException,

    -- ** MasterCannotLeaveOrganizationException
    _MasterCannotLeaveOrganizationException,

    -- ** AccountNotFoundException
    _AccountNotFoundException,

    -- ** AccountAlreadyRegisteredException
    _AccountAlreadyRegisteredException,

    -- ** DuplicatePolicyException
    _DuplicatePolicyException,

    -- ** ConstraintViolationException
    _ConstraintViolationException,

    -- ** AccountNotRegisteredException
    _AccountNotRegisteredException,

    -- ** FinalizingOrganizationException
    _FinalizingOrganizationException,

    -- ** HandshakeNotFoundException
    _HandshakeNotFoundException,

    -- ** PolicyTypeNotAvailableForOrganizationException
    _PolicyTypeNotAvailableForOrganizationException,

    -- ** ChildNotFoundException
    _ChildNotFoundException,

    -- ** UnsupportedAPIEndpointException
    _UnsupportedAPIEndpointException,

    -- ** EffectivePolicyNotFoundException
    _EffectivePolicyNotFoundException,

    -- ** OrganizationalUnitNotFoundException
    _OrganizationalUnitNotFoundException,

    -- ** DestinationParentNotFoundException
    _DestinationParentNotFoundException,

    -- ** OrganizationNotEmptyException
    _OrganizationNotEmptyException,

    -- ** AccountOwnerNotVerifiedException
    _AccountOwnerNotVerifiedException,

    -- ** PolicyTypeNotEnabledException
    _PolicyTypeNotEnabledException,

    -- ** DuplicateHandshakeException
    _DuplicateHandshakeException,

    -- ** OrganizationalUnitNotEmptyException
    _OrganizationalUnitNotEmptyException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ServiceException
    _ServiceException,

    -- ** SourceParentNotFoundException
    _SourceParentNotFoundException,

    -- ** TargetNotFoundException
    _TargetNotFoundException,

    -- ** CreateAccountStatusNotFoundException
    _CreateAccountStatusNotFoundException,

    -- ** AlreadyInOrganizationException
    _AlreadyInOrganizationException,

    -- ** DuplicateOrganizationalUnitException
    _DuplicateOrganizationalUnitException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** PolicyChangesInProgressException
    _PolicyChangesInProgressException,

    -- ** PolicyNotAttachedException
    _PolicyNotAttachedException,

    -- ** ParentNotFoundException
    _ParentNotFoundException,

    -- ** AccessDeniedForDependencyException
    _AccessDeniedForDependencyException,

    -- ** AWSOrganizationsNotInUseException
    _AWSOrganizationsNotInUseException,

    -- ** PolicyInUseException
    _PolicyInUseException,

    -- ** InvalidHandshakeTransitionException
    _InvalidHandshakeTransitionException,

    -- ** HandshakeAlreadyInStateException
    _HandshakeAlreadyInStateException,

    -- ** DuplicateAccountException
    _DuplicateAccountException,

    -- ** DuplicatePolicyAttachmentException
    _DuplicatePolicyAttachmentException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListHandshakesForAccount (Paginated)
    module Network.AWS.Organizations.ListHandshakesForAccount,

    -- ** DescribeAccount
    module Network.AWS.Organizations.DescribeAccount,

    -- ** ListPolicies (Paginated)
    module Network.AWS.Organizations.ListPolicies,

    -- ** CreatePolicy
    module Network.AWS.Organizations.CreatePolicy,

    -- ** ListRoots (Paginated)
    module Network.AWS.Organizations.ListRoots,

    -- ** AcceptHandshake
    module Network.AWS.Organizations.AcceptHandshake,

    -- ** CreateOrganization
    module Network.AWS.Organizations.CreateOrganization,

    -- ** EnableAllFeatures
    module Network.AWS.Organizations.EnableAllFeatures,

    -- ** DeleteOrganization
    module Network.AWS.Organizations.DeleteOrganization,

    -- ** DescribeHandshake
    module Network.AWS.Organizations.DescribeHandshake,

    -- ** ListTagsForResource (Paginated)
    module Network.AWS.Organizations.ListTagsForResource,

    -- ** DescribePolicy
    module Network.AWS.Organizations.DescribePolicy,

    -- ** ListDelegatedServicesForAccount (Paginated)
    module Network.AWS.Organizations.ListDelegatedServicesForAccount,

    -- ** DisableAWSServiceAccess
    module Network.AWS.Organizations.DisableAWSServiceAccess,

    -- ** DescribeEffectivePolicy
    module Network.AWS.Organizations.DescribeEffectivePolicy,

    -- ** LeaveOrganization
    module Network.AWS.Organizations.LeaveOrganization,

    -- ** MoveAccount
    module Network.AWS.Organizations.MoveAccount,

    -- ** ListAccounts (Paginated)
    module Network.AWS.Organizations.ListAccounts,

    -- ** InviteAccountToOrganization
    module Network.AWS.Organizations.InviteAccountToOrganization,

    -- ** ListAWSServiceAccessForOrganization (Paginated)
    module Network.AWS.Organizations.ListAWSServiceAccessForOrganization,

    -- ** ListOrganizationalUnitsForParent (Paginated)
    module Network.AWS.Organizations.ListOrganizationalUnitsForParent,

    -- ** CancelHandshake
    module Network.AWS.Organizations.CancelHandshake,

    -- ** ListChildren (Paginated)
    module Network.AWS.Organizations.ListChildren,

    -- ** ListDelegatedAdministrators (Paginated)
    module Network.AWS.Organizations.ListDelegatedAdministrators,

    -- ** DeletePolicy
    module Network.AWS.Organizations.DeletePolicy,

    -- ** UpdatePolicy
    module Network.AWS.Organizations.UpdatePolicy,

    -- ** EnablePolicyType
    module Network.AWS.Organizations.EnablePolicyType,

    -- ** DisablePolicyType
    module Network.AWS.Organizations.DisablePolicyType,

    -- ** DescribeCreateAccountStatus
    module Network.AWS.Organizations.DescribeCreateAccountStatus,

    -- ** CreateOrganizationalUnit
    module Network.AWS.Organizations.CreateOrganizationalUnit,

    -- ** ListAccountsForParent (Paginated)
    module Network.AWS.Organizations.ListAccountsForParent,

    -- ** DetachPolicy
    module Network.AWS.Organizations.DetachPolicy,

    -- ** RemoveAccountFromOrganization
    module Network.AWS.Organizations.RemoveAccountFromOrganization,

    -- ** CreateGovCloudAccount
    module Network.AWS.Organizations.CreateGovCloudAccount,

    -- ** EnableAWSServiceAccess
    module Network.AWS.Organizations.EnableAWSServiceAccess,

    -- ** DescribeOrganizationalUnit
    module Network.AWS.Organizations.DescribeOrganizationalUnit,

    -- ** ListParents (Paginated)
    module Network.AWS.Organizations.ListParents,

    -- ** CreateAccount
    module Network.AWS.Organizations.CreateAccount,

    -- ** DeregisterDelegatedAdministrator
    module Network.AWS.Organizations.DeregisterDelegatedAdministrator,

    -- ** TagResource
    module Network.AWS.Organizations.TagResource,

    -- ** ListCreateAccountStatus (Paginated)
    module Network.AWS.Organizations.ListCreateAccountStatus,

    -- ** ListTargetsForPolicy (Paginated)
    module Network.AWS.Organizations.ListTargetsForPolicy,

    -- ** DeclineHandshake
    module Network.AWS.Organizations.DeclineHandshake,

    -- ** UntagResource
    module Network.AWS.Organizations.UntagResource,

    -- ** AttachPolicy
    module Network.AWS.Organizations.AttachPolicy,

    -- ** ListPoliciesForTarget (Paginated)
    module Network.AWS.Organizations.ListPoliciesForTarget,

    -- ** DescribeOrganization
    module Network.AWS.Organizations.DescribeOrganization,

    -- ** ListHandshakesForOrganization (Paginated)
    module Network.AWS.Organizations.ListHandshakesForOrganization,

    -- ** RegisterDelegatedAdministrator
    module Network.AWS.Organizations.RegisterDelegatedAdministrator,

    -- ** DeleteOrganizationalUnit
    module Network.AWS.Organizations.DeleteOrganizationalUnit,

    -- ** UpdateOrganizationalUnit
    module Network.AWS.Organizations.UpdateOrganizationalUnit,

    -- * Types

    -- ** Parent
    Parent (..),
    mkParent,
    pId,
    pType,

    -- ** HandshakePartyId
    HandshakePartyId (..),

    -- ** PolicyTypeSummary
    PolicyTypeSummary (..),
    mkPolicyTypeSummary,
    ptsStatus,
    ptsType,

    -- ** GenericArn
    GenericArn (..),

    -- ** HandshakeNotes
    HandshakeNotes (..),

    -- ** Email
    Email (..),

    -- ** PolicyTypeStatus
    PolicyTypeStatus (..),

    -- ** Handshake
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

    -- ** ServicePrincipal
    ServicePrincipal (..),

    -- ** PolicyName
    PolicyName (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** EnabledServicePrincipal
    EnabledServicePrincipal (..),
    mkEnabledServicePrincipal,
    espDateEnabled,
    espServicePrincipal,

    -- ** Child
    Child (..),
    mkChild,
    cId,
    cType,

    -- ** CreateAccountState
    CreateAccountState (..),

    -- ** EffectivePolicy
    EffectivePolicy (..),
    mkEffectivePolicy,
    epLastUpdatedTimestamp,
    epPolicyContent,
    epPolicyType,
    epTargetId,

    -- ** AccountJoinedMethod
    AccountJoinedMethod (..),

    -- ** AccountStatus
    AccountStatus (..),

    -- ** PolicyId
    PolicyId (..),

    -- ** HandshakeResourceType
    HandshakeResourceType (..),

    -- ** OrganizationalUnitArn
    OrganizationalUnitArn (..),

    -- ** OrganizationalUnit
    OrganizationalUnit (..),
    mkOrganizationalUnit,
    ouArn,
    ouId,
    ouName,

    -- ** IAMUserAccessToBilling
    IAMUserAccessToBilling (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** RootId
    RootId (..),

    -- ** RoleName
    RoleName (..),

    -- ** AccountName
    AccountName (..),

    -- ** Root
    Root (..),
    mkRoot,
    rArn,
    rId,
    rName,
    rPolicyTypes,

    -- ** DelegatedService
    DelegatedService (..),
    mkDelegatedService,
    dsDelegationEnabledDate,
    dsServicePrincipal,

    -- ** PolicyDescription
    PolicyDescription (..),

    -- ** HandshakePartyType
    HandshakePartyType (..),

    -- ** CreateAccountRequestId
    CreateAccountRequestId (..),

    -- ** TargetType
    TargetType (..),

    -- ** CreateAccountFailureReason
    CreateAccountFailureReason (..),

    -- ** Account
    Account (..),
    mkAccount,
    aArn,
    aEmail,
    aId,
    aJoinedMethod,
    aJoinedTimestamp,
    aName,
    aStatus,

    -- ** AccountId
    AccountId (..),

    -- ** PolicyTargetId
    PolicyTargetId (..),

    -- ** NextToken
    NextToken (..),

    -- ** ParentType
    ParentType (..),

    -- ** AccountArn
    AccountArn (..),

    -- ** PolicySummary
    PolicySummary (..),
    mkPolicySummary,
    psArn,
    psAwsManaged,
    psDescription,
    psId,
    psName,
    psType,

    -- ** HandshakeFilter
    HandshakeFilter (..),
    mkHandshakeFilter,
    hfActionType,
    hfParentHandshakeId,

    -- ** OrganizationalUnitName
    OrganizationalUnitName (..),

    -- ** ChildId
    ChildId (..),

    -- ** ChildType
    ChildType (..),

    -- ** HandshakeId
    HandshakeId (..),

    -- ** PolicyTargetSummary
    PolicyTargetSummary (..),
    mkPolicyTargetSummary,
    ptsfArn,
    ptsfName,
    ptsfTargetId,
    ptsfType,

    -- ** HandshakeResource
    HandshakeResource (..),
    mkHandshakeResource,
    hrResources,
    hrType,
    hrValue,

    -- ** EffectivePolicyType
    EffectivePolicyType (..),

    -- ** OrganizationalUnitId
    OrganizationalUnitId (..),

    -- ** OrganizationFeatureSet
    OrganizationFeatureSet (..),

    -- ** TagKey
    TagKey (..),

    -- ** Policy
    Policy (..),
    mkPolicy,
    pContent,
    pPolicySummary,

    -- ** DelegatedAdministrator
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

    -- ** HandshakeState
    HandshakeState (..),

    -- ** TaggableResourceId
    TaggableResourceId (..),

    -- ** Organization
    Organization (..),
    mkOrganization,
    oArn,
    oAvailablePolicyTypes,
    oFeatureSet,
    oId,
    oMasterAccountArn,
    oMasterAccountEmail,
    oMasterAccountId,

    -- ** ActionType
    ActionType (..),

    -- ** HandshakeParty
    HandshakeParty (..),
    mkHandshakeParty,
    hpId,
    hpType,

    -- ** ParentId
    ParentId (..),

    -- ** PolicyContent
    PolicyContent (..),

    -- ** CreateAccountStatus
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

    -- ** Id
    Id (..),

    -- ** Arn
    Arn (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** TargetId
    TargetId (..),

    -- ** Name
    Name (..),

    -- ** Content
    Content (..),

    -- ** Description
    Description (..),

    -- ** ResourceId
    ResourceId (..),

    -- ** ParentHandshakeId
    ParentHandshakeId (..),

    -- ** SourceParentId
    SourceParentId (..),

    -- ** DestinationParentId
    DestinationParentId (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import qualified Network.AWS.Prelude as Lude

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
