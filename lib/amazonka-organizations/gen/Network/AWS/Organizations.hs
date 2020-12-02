{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Organizations API Reference__
--
-- AWS Organizations is a web service that enables you to consolidate your multiple AWS accounts into an /organization/ and centrally manage your accounts and their resources.
--
-- This guide provides descriptions of the Organizations API. For more information about using this service, see the <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html AWS Organizations User Guide> .
--
-- __API Version__
--
-- This version of the Organizations API Reference documents the Organizations API version 2016-11-28.
--
-- We recommend that you use the AWS SDKs to make programmatic API calls to Organizations. However, you also can use the Organizations Query API to make direct calls to the Organizations web service. To learn more about the Organizations Query API, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_query-requests.html Making Query Requests> in the /AWS Organizations User Guide/ . Organizations supports GET and POST requests for all actions. That is, the API does not require you to use GET for some actions and POST for others. However, GET requests are subject to the limitation size of a URL. Therefore, for operations that require larger sizes, use a POST request.
--
-- __Signing Requests__
--
-- When you send HTTP requests to AWS, you must sign the requests so that AWS can identify who sent them. You sign requests with your AWS access key, which consists of an access key ID and a secret access key. We strongly recommend that you do not create an access key for your root account. Anyone who has the access key for your root account has unrestricted access to all the resources in your account. Instead, create an access key for an IAM user account that has administrative privileges. As another option, use AWS Security Token Service to generate temporary security credentials, and use those credentials to sign requests.
--
-- To sign requests, we recommend that you use <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4> . If you have an existing application that uses Signature Version 2, you do not have to update it to use Signature Version 4. However, some operations now require Signature Version 4. The documentation for operations that require version 4 indicate this requirement.
--
-- When you use the AWS Command Line Interface (AWS CLI) or one of the AWS SDKs to make requests to AWS, these tools automatically sign the requests for you with the access key that you specify when you configure the tools.
--
-- In this release, each organization can have only one root. In a future release, a single organization will support multiple roots.
--
-- __Support and Feedback for AWS Organizations__
--
-- We welcome your feedback. Send your comments to <mailto:feedback-awsorganizations@amazon.com feedback-awsorganizations@amazon.com> or post your feedback and questions in the <http://forums.aws.amazon.com/forum.jspa?forumID=219 AWS Organizations support forum> . For more information about the AWS support forums, see <http://forums.aws.amazon.com/help.jspa Forums Help> .
--
-- __Endpoint to Call When Using the CLI or the AWS API__
--
-- For the current release of Organizations, you must specify the @us-east-1@ region for all AWS API and CLI calls. You can do this in the CLI by using these parameters and commands:
--
--     * Use the following parameter with each command to specify both the endpoint and its region:
--
-- @--endpoint-url https://organizations.us-east-1.amazonaws.com@
--
--     * Use the default endpoint, but configure your default region with this command:
--
-- @aws configure set default.region us-east-1@
--
--     * Use the following parameter with each command to specify the endpoint:
--
-- @--region us-east-1@
--
--
--
-- For the various SDKs used to call the APIs, see the documentation for the SDK of interest to learn how to direct the requests to a specific endpoint. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#sts_region Regions and Endpoints> in the /AWS General Reference/ .
--
-- __How examples are presented__
--
-- The JSON returned by the AWS Organizations service as response to your requests is returned as a single long string without line breaks or formatting whitespace. Both line breaks and whitespace are included in the examples in this guide to improve readability. When example input parameters also would result in long strings that would extend beyond the screen, we insert line breaks to enhance readability. You should always submit the input as a single JSON text string.
--
-- __Recording API Requests__
--
-- AWS Organizations supports AWS CloudTrail, a service that records AWS API calls for your AWS account and delivers log files to an Amazon S3 bucket. By using information collected by AWS CloudTrail, you can determine which requests were successfully made to Organizations, who made the request, when it was made, and so on. For more about AWS Organizations and its support for AWS CloudTrail, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_cloudtrail-integration.html Logging AWS Organizations Events with AWS CloudTrail> in the /AWS Organizations User Guide/ . To learn more about CloudTrail, including how to turn it on and find your log files, see the <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html AWS CloudTrail User Guide> .
--
module Network.AWS.Organizations
    (
    -- * Service Configuration
      organizations

    -- * Errors
    -- $errors

    -- ** PolicyNotFoundException
    , _PolicyNotFoundException

    -- ** PolicyTypeAlreadyEnabledException
    , _PolicyTypeAlreadyEnabledException

    -- ** HandshakeConstraintViolationException
    , _HandshakeConstraintViolationException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** MalformedPolicyDocumentException
    , _MalformedPolicyDocumentException

    -- ** RootNotFoundException
    , _RootNotFoundException

    -- ** MasterCannotLeaveOrganizationException
    , _MasterCannotLeaveOrganizationException

    -- ** AccountNotFoundException
    , _AccountNotFoundException

    -- ** DuplicatePolicyException
    , _DuplicatePolicyException

    -- ** ConstraintViolationException
    , _ConstraintViolationException

    -- ** FinalizingOrganizationException
    , _FinalizingOrganizationException

    -- ** HandshakeNotFoundException
    , _HandshakeNotFoundException

    -- ** PolicyTypeNotAvailableForOrganizationException
    , _PolicyTypeNotAvailableForOrganizationException

    -- ** ChildNotFoundException
    , _ChildNotFoundException

    -- ** OrganizationalUnitNotFoundException
    , _OrganizationalUnitNotFoundException

    -- ** DestinationParentNotFoundException
    , _DestinationParentNotFoundException

    -- ** OrganizationNotEmptyException
    , _OrganizationNotEmptyException

    -- ** PolicyTypeNotEnabledException
    , _PolicyTypeNotEnabledException

    -- ** DuplicateHandshakeException
    , _DuplicateHandshakeException

    -- ** OrganizationalUnitNotEmptyException
    , _OrganizationalUnitNotEmptyException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** ServiceException
    , _ServiceException

    -- ** SourceParentNotFoundException
    , _SourceParentNotFoundException

    -- ** TargetNotFoundException
    , _TargetNotFoundException

    -- ** CreateAccountStatusNotFoundException
    , _CreateAccountStatusNotFoundException

    -- ** AlreadyInOrganizationException
    , _AlreadyInOrganizationException

    -- ** DuplicateOrganizationalUnitException
    , _DuplicateOrganizationalUnitException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** PolicyNotAttachedException
    , _PolicyNotAttachedException

    -- ** ParentNotFoundException
    , _ParentNotFoundException

    -- ** AccessDeniedForDependencyException
    , _AccessDeniedForDependencyException

    -- ** AWSOrganizationsNotInUseException
    , _AWSOrganizationsNotInUseException

    -- ** PolicyInUseException
    , _PolicyInUseException

    -- ** InvalidHandshakeTransitionException
    , _InvalidHandshakeTransitionException

    -- ** HandshakeAlreadyInStateException
    , _HandshakeAlreadyInStateException

    -- ** DuplicateAccountException
    , _DuplicateAccountException

    -- ** DuplicatePolicyAttachmentException
    , _DuplicatePolicyAttachmentException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListHandshakesForAccount (Paginated)
    , module Network.AWS.Organizations.ListHandshakesForAccount

    -- ** DescribeAccount
    , module Network.AWS.Organizations.DescribeAccount

    -- ** ListPolicies (Paginated)
    , module Network.AWS.Organizations.ListPolicies

    -- ** CreatePolicy
    , module Network.AWS.Organizations.CreatePolicy

    -- ** ListRoots (Paginated)
    , module Network.AWS.Organizations.ListRoots

    -- ** AcceptHandshake
    , module Network.AWS.Organizations.AcceptHandshake

    -- ** CreateOrganization
    , module Network.AWS.Organizations.CreateOrganization

    -- ** EnableAllFeatures
    , module Network.AWS.Organizations.EnableAllFeatures

    -- ** DeleteOrganization
    , module Network.AWS.Organizations.DeleteOrganization

    -- ** DescribeHandshake
    , module Network.AWS.Organizations.DescribeHandshake

    -- ** DescribePolicy
    , module Network.AWS.Organizations.DescribePolicy

    -- ** DisableAWSServiceAccess
    , module Network.AWS.Organizations.DisableAWSServiceAccess

    -- ** LeaveOrganization
    , module Network.AWS.Organizations.LeaveOrganization

    -- ** MoveAccount
    , module Network.AWS.Organizations.MoveAccount

    -- ** ListAccounts (Paginated)
    , module Network.AWS.Organizations.ListAccounts

    -- ** InviteAccountToOrganization
    , module Network.AWS.Organizations.InviteAccountToOrganization

    -- ** ListAWSServiceAccessForOrganization (Paginated)
    , module Network.AWS.Organizations.ListAWSServiceAccessForOrganization

    -- ** ListOrganizationalUnitsForParent (Paginated)
    , module Network.AWS.Organizations.ListOrganizationalUnitsForParent

    -- ** CancelHandshake
    , module Network.AWS.Organizations.CancelHandshake

    -- ** ListChildren (Paginated)
    , module Network.AWS.Organizations.ListChildren

    -- ** DeletePolicy
    , module Network.AWS.Organizations.DeletePolicy

    -- ** UpdatePolicy
    , module Network.AWS.Organizations.UpdatePolicy

    -- ** EnablePolicyType
    , module Network.AWS.Organizations.EnablePolicyType

    -- ** DisablePolicyType
    , module Network.AWS.Organizations.DisablePolicyType

    -- ** DescribeCreateAccountStatus
    , module Network.AWS.Organizations.DescribeCreateAccountStatus

    -- ** CreateOrganizationalUnit
    , module Network.AWS.Organizations.CreateOrganizationalUnit

    -- ** ListAccountsForParent (Paginated)
    , module Network.AWS.Organizations.ListAccountsForParent

    -- ** DetachPolicy
    , module Network.AWS.Organizations.DetachPolicy

    -- ** RemoveAccountFromOrganization
    , module Network.AWS.Organizations.RemoveAccountFromOrganization

    -- ** EnableAWSServiceAccess
    , module Network.AWS.Organizations.EnableAWSServiceAccess

    -- ** DescribeOrganizationalUnit
    , module Network.AWS.Organizations.DescribeOrganizationalUnit

    -- ** ListParents (Paginated)
    , module Network.AWS.Organizations.ListParents

    -- ** CreateAccount
    , module Network.AWS.Organizations.CreateAccount

    -- ** ListCreateAccountStatus (Paginated)
    , module Network.AWS.Organizations.ListCreateAccountStatus

    -- ** ListTargetsForPolicy (Paginated)
    , module Network.AWS.Organizations.ListTargetsForPolicy

    -- ** DeclineHandshake
    , module Network.AWS.Organizations.DeclineHandshake

    -- ** AttachPolicy
    , module Network.AWS.Organizations.AttachPolicy

    -- ** ListPoliciesForTarget (Paginated)
    , module Network.AWS.Organizations.ListPoliciesForTarget

    -- ** DescribeOrganization
    , module Network.AWS.Organizations.DescribeOrganization

    -- ** ListHandshakesForOrganization (Paginated)
    , module Network.AWS.Organizations.ListHandshakesForOrganization

    -- ** DeleteOrganizationalUnit
    , module Network.AWS.Organizations.DeleteOrganizationalUnit

    -- ** UpdateOrganizationalUnit
    , module Network.AWS.Organizations.UpdateOrganizationalUnit

    -- * Types

    -- ** AccountJoinedMethod
    , AccountJoinedMethod (..)

    -- ** AccountStatus
    , AccountStatus (..)

    -- ** ActionType
    , ActionType (..)

    -- ** ChildType
    , ChildType (..)

    -- ** CreateAccountFailureReason
    , CreateAccountFailureReason (..)

    -- ** CreateAccountState
    , CreateAccountState (..)

    -- ** HandshakePartyType
    , HandshakePartyType (..)

    -- ** HandshakeResourceType
    , HandshakeResourceType (..)

    -- ** HandshakeState
    , HandshakeState (..)

    -- ** IAMUserAccessToBilling
    , IAMUserAccessToBilling (..)

    -- ** OrganizationFeatureSet
    , OrganizationFeatureSet (..)

    -- ** ParentType
    , ParentType (..)

    -- ** PolicyType
    , PolicyType (..)

    -- ** PolicyTypeStatus
    , PolicyTypeStatus (..)

    -- ** TargetType
    , TargetType (..)

    -- ** Account
    , Account
    , account
    , aStatus
    , aJoinedMethod
    , aEmail
    , aARN
    , aJoinedTimestamp
    , aName
    , aId

    -- ** Child
    , Child
    , child
    , cId
    , cType

    -- ** CreateAccountStatus
    , CreateAccountStatus
    , createAccountStatus
    , casFailureReason
    , casState
    , casCompletedTimestamp
    , casAccountName
    , casAccountId
    , casId
    , casRequestedTimestamp

    -- ** EnabledServicePrincipal
    , EnabledServicePrincipal
    , enabledServicePrincipal
    , espServicePrincipal
    , espDateEnabled

    -- ** Handshake
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

    -- ** HandshakeFilter
    , HandshakeFilter
    , handshakeFilter
    , hfParentHandshakeId
    , hfActionType

    -- ** HandshakeParty
    , HandshakeParty
    , handshakeParty
    , hpId
    , hpType

    -- ** HandshakeResource
    , HandshakeResource
    , handshakeResource
    , hrValue
    , hrResources
    , hrType

    -- ** Organization
    , Organization
    , organization
    , oARN
    , oMasterAccountId
    , oMasterAccountARN
    , oMasterAccountEmail
    , oAvailablePolicyTypes
    , oId
    , oFeatureSet

    -- ** OrganizationalUnit
    , OrganizationalUnit
    , organizationalUnit
    , ouARN
    , ouName
    , ouId

    -- ** Parent
    , Parent
    , parent
    , pId
    , pType

    -- ** Policy
    , Policy
    , policy
    , pContent
    , pPolicySummary

    -- ** PolicySummary
    , PolicySummary
    , policySummary
    , psARN
    , psName
    , psId
    , psAWSManaged
    , psType
    , psDescription

    -- ** PolicyTargetSummary
    , PolicyTargetSummary
    , policyTargetSummary
    , polTargetId
    , polARN
    , polName
    , polType

    -- ** PolicyTypeSummary
    , PolicyTypeSummary
    , policyTypeSummary
    , ptsStatus
    , ptsType

    -- ** Root
    , Root
    , root
    , rARN
    , rName
    , rId
    , rPolicyTypes
    ) where

import Network.AWS.Organizations.AcceptHandshake
import Network.AWS.Organizations.AttachPolicy
import Network.AWS.Organizations.CancelHandshake
import Network.AWS.Organizations.CreateAccount
import Network.AWS.Organizations.CreateOrganization
import Network.AWS.Organizations.CreateOrganizationalUnit
import Network.AWS.Organizations.CreatePolicy
import Network.AWS.Organizations.DeclineHandshake
import Network.AWS.Organizations.DeleteOrganization
import Network.AWS.Organizations.DeleteOrganizationalUnit
import Network.AWS.Organizations.DeletePolicy
import Network.AWS.Organizations.DescribeAccount
import Network.AWS.Organizations.DescribeCreateAccountStatus
import Network.AWS.Organizations.DescribeHandshake
import Network.AWS.Organizations.DescribeOrganization
import Network.AWS.Organizations.DescribeOrganizationalUnit
import Network.AWS.Organizations.DescribePolicy
import Network.AWS.Organizations.DetachPolicy
import Network.AWS.Organizations.DisableAWSServiceAccess
import Network.AWS.Organizations.DisablePolicyType
import Network.AWS.Organizations.EnableAllFeatures
import Network.AWS.Organizations.EnableAWSServiceAccess
import Network.AWS.Organizations.EnablePolicyType
import Network.AWS.Organizations.InviteAccountToOrganization
import Network.AWS.Organizations.LeaveOrganization
import Network.AWS.Organizations.ListAccounts
import Network.AWS.Organizations.ListAccountsForParent
import Network.AWS.Organizations.ListAWSServiceAccessForOrganization
import Network.AWS.Organizations.ListChildren
import Network.AWS.Organizations.ListCreateAccountStatus
import Network.AWS.Organizations.ListHandshakesForAccount
import Network.AWS.Organizations.ListHandshakesForOrganization
import Network.AWS.Organizations.ListOrganizationalUnitsForParent
import Network.AWS.Organizations.ListParents
import Network.AWS.Organizations.ListPolicies
import Network.AWS.Organizations.ListPoliciesForTarget
import Network.AWS.Organizations.ListRoots
import Network.AWS.Organizations.ListTargetsForPolicy
import Network.AWS.Organizations.MoveAccount
import Network.AWS.Organizations.RemoveAccountFromOrganization
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.UpdateOrganizationalUnit
import Network.AWS.Organizations.UpdatePolicy
import Network.AWS.Organizations.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Organizations'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
