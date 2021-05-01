{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Cognito Federated Identities
--
-- Amazon Cognito Federated Identities is a web service that delivers
-- scoped temporary credentials to mobile devices and other untrusted
-- environments. It uniquely identifies a device and supplies the user with
-- a consistent identity over the lifetime of an application.
--
-- Using Amazon Cognito Federated Identities, you can enable authentication
-- with one or more third-party identity providers (Facebook, Google, or
-- Login with Amazon) or an Amazon Cognito user pool, and you can also
-- choose to support unauthenticated access from your app. Cognito delivers
-- a unique identifier for each user and acts as an OpenID token provider
-- trusted by AWS Security Token Service (STS) to access temporary,
-- limited-privilege AWS credentials.
--
-- For a description of the authentication flow from the Amazon Cognito
-- Developer Guide see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Authentication Flow>.
--
-- For more information see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-identity.html Amazon Cognito Federated Identities>.
module Network.AWS.CognitoIdentity
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ExternalServiceException
    _ExternalServiceException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidIdentityPoolConfigurationException
    _InvalidIdentityPoolConfigurationException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** DeveloperUserAlreadyRegisteredException
    _DeveloperUserAlreadyRegisteredException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeIdentityPool
    DescribeIdentityPool (DescribeIdentityPool'),
    newDescribeIdentityPool,
    IdentityPool (IdentityPool'),
    newIdentityPool,

    -- ** GetOpenIdTokenForDeveloperIdentity
    GetOpenIdTokenForDeveloperIdentity (GetOpenIdTokenForDeveloperIdentity'),
    newGetOpenIdTokenForDeveloperIdentity,
    GetOpenIdTokenForDeveloperIdentityResponse (GetOpenIdTokenForDeveloperIdentityResponse'),
    newGetOpenIdTokenForDeveloperIdentityResponse,

    -- ** GetOpenIdToken
    GetOpenIdToken (GetOpenIdToken'),
    newGetOpenIdToken,
    GetOpenIdTokenResponse (GetOpenIdTokenResponse'),
    newGetOpenIdTokenResponse,

    -- ** DeleteIdentities
    DeleteIdentities (DeleteIdentities'),
    newDeleteIdentities,
    DeleteIdentitiesResponse (DeleteIdentitiesResponse'),
    newDeleteIdentitiesResponse,

    -- ** MergeDeveloperIdentities
    MergeDeveloperIdentities (MergeDeveloperIdentities'),
    newMergeDeveloperIdentities,
    MergeDeveloperIdentitiesResponse (MergeDeveloperIdentitiesResponse'),
    newMergeDeveloperIdentitiesResponse,

    -- ** CreateIdentityPool
    CreateIdentityPool (CreateIdentityPool'),
    newCreateIdentityPool,
    IdentityPool (IdentityPool'),
    newIdentityPool,

    -- ** GetPrincipalTagAttributeMap
    GetPrincipalTagAttributeMap (GetPrincipalTagAttributeMap'),
    newGetPrincipalTagAttributeMap,
    GetPrincipalTagAttributeMapResponse (GetPrincipalTagAttributeMapResponse'),
    newGetPrincipalTagAttributeMapResponse,

    -- ** UpdateIdentityPool
    UpdateIdentityPool (UpdateIdentityPool'),
    newUpdateIdentityPool,
    IdentityPool (IdentityPool'),
    newIdentityPool,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteIdentityPool
    DeleteIdentityPool (DeleteIdentityPool'),
    newDeleteIdentityPool,
    DeleteIdentityPoolResponse (DeleteIdentityPoolResponse'),
    newDeleteIdentityPoolResponse,

    -- ** GetIdentityPoolRoles
    GetIdentityPoolRoles (GetIdentityPoolRoles'),
    newGetIdentityPoolRoles,
    GetIdentityPoolRolesResponse (GetIdentityPoolRolesResponse'),
    newGetIdentityPoolRolesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UnlinkIdentity
    UnlinkIdentity (UnlinkIdentity'),
    newUnlinkIdentity,
    UnlinkIdentityResponse (UnlinkIdentityResponse'),
    newUnlinkIdentityResponse,

    -- ** LookupDeveloperIdentity
    LookupDeveloperIdentity (LookupDeveloperIdentity'),
    newLookupDeveloperIdentity,
    LookupDeveloperIdentityResponse (LookupDeveloperIdentityResponse'),
    newLookupDeveloperIdentityResponse,

    -- ** SetIdentityPoolRoles
    SetIdentityPoolRoles (SetIdentityPoolRoles'),
    newSetIdentityPoolRoles,
    SetIdentityPoolRolesResponse (SetIdentityPoolRolesResponse'),
    newSetIdentityPoolRolesResponse,

    -- ** ListIdentityPools (Paginated)
    ListIdentityPools (ListIdentityPools'),
    newListIdentityPools,
    ListIdentityPoolsResponse (ListIdentityPoolsResponse'),
    newListIdentityPoolsResponse,

    -- ** DescribeIdentity
    DescribeIdentity (DescribeIdentity'),
    newDescribeIdentity,
    IdentityDescription (IdentityDescription'),
    newIdentityDescription,

    -- ** GetCredentialsForIdentity
    GetCredentialsForIdentity (GetCredentialsForIdentity'),
    newGetCredentialsForIdentity,
    GetCredentialsForIdentityResponse (GetCredentialsForIdentityResponse'),
    newGetCredentialsForIdentityResponse,

    -- ** UnlinkDeveloperIdentity
    UnlinkDeveloperIdentity (UnlinkDeveloperIdentity'),
    newUnlinkDeveloperIdentity,
    UnlinkDeveloperIdentityResponse (UnlinkDeveloperIdentityResponse'),
    newUnlinkDeveloperIdentityResponse,

    -- ** GetId
    GetId (GetId'),
    newGetId,
    GetIdResponse (GetIdResponse'),
    newGetIdResponse,

    -- ** ListIdentities
    ListIdentities (ListIdentities'),
    newListIdentities,
    ListIdentitiesResponse (ListIdentitiesResponse'),
    newListIdentitiesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** SetPrincipalTagAttributeMap
    SetPrincipalTagAttributeMap (SetPrincipalTagAttributeMap'),
    newSetPrincipalTagAttributeMap,
    SetPrincipalTagAttributeMapResponse (SetPrincipalTagAttributeMapResponse'),
    newSetPrincipalTagAttributeMapResponse,

    -- * Types

    -- ** AmbiguousRoleResolutionType
    AmbiguousRoleResolutionType (..),

    -- ** CognitoErrorCode
    CognitoErrorCode (..),

    -- ** MappingRuleMatchType
    MappingRuleMatchType (..),

    -- ** RoleMappingType
    RoleMappingType (..),

    -- ** CognitoIdentityProvider
    CognitoIdentityProvider (CognitoIdentityProvider'),
    newCognitoIdentityProvider,

    -- ** Credentials
    Credentials (Credentials'),
    newCredentials,

    -- ** IdentityDescription
    IdentityDescription (IdentityDescription'),
    newIdentityDescription,

    -- ** IdentityPool
    IdentityPool (IdentityPool'),
    newIdentityPool,

    -- ** IdentityPoolShortDescription
    IdentityPoolShortDescription (IdentityPoolShortDescription'),
    newIdentityPoolShortDescription,

    -- ** MappingRule
    MappingRule (MappingRule'),
    newMappingRule,

    -- ** RoleMapping
    RoleMapping (RoleMapping'),
    newRoleMapping,

    -- ** RulesConfigurationType
    RulesConfigurationType (RulesConfigurationType'),
    newRulesConfigurationType,

    -- ** UnprocessedIdentityId
    UnprocessedIdentityId (UnprocessedIdentityId'),
    newUnprocessedIdentityId,
  )
where

import Network.AWS.CognitoIdentity.CreateIdentityPool
import Network.AWS.CognitoIdentity.DeleteIdentities
import Network.AWS.CognitoIdentity.DeleteIdentityPool
import Network.AWS.CognitoIdentity.DescribeIdentity
import Network.AWS.CognitoIdentity.DescribeIdentityPool
import Network.AWS.CognitoIdentity.GetCredentialsForIdentity
import Network.AWS.CognitoIdentity.GetId
import Network.AWS.CognitoIdentity.GetIdentityPoolRoles
import Network.AWS.CognitoIdentity.GetOpenIdToken
import Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
import Network.AWS.CognitoIdentity.GetPrincipalTagAttributeMap
import Network.AWS.CognitoIdentity.Lens
import Network.AWS.CognitoIdentity.ListIdentities
import Network.AWS.CognitoIdentity.ListIdentityPools
import Network.AWS.CognitoIdentity.ListTagsForResource
import Network.AWS.CognitoIdentity.LookupDeveloperIdentity
import Network.AWS.CognitoIdentity.MergeDeveloperIdentities
import Network.AWS.CognitoIdentity.SetIdentityPoolRoles
import Network.AWS.CognitoIdentity.SetPrincipalTagAttributeMap
import Network.AWS.CognitoIdentity.TagResource
import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
import Network.AWS.CognitoIdentity.UnlinkIdentity
import Network.AWS.CognitoIdentity.UntagResource
import Network.AWS.CognitoIdentity.UpdateIdentityPool
import Network.AWS.CognitoIdentity.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CognitoIdentity'.

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
