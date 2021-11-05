{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CognitoIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-06-30@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.CognitoIdentity
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidIdentityPoolConfigurationException
    _InvalidIdentityPoolConfigurationException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** ExternalServiceException
    _ExternalServiceException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** DeveloperUserAlreadyRegisteredException
    _DeveloperUserAlreadyRegisteredException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetOpenIdToken
    GetOpenIdToken (GetOpenIdToken'),
    newGetOpenIdToken,
    GetOpenIdTokenResponse (GetOpenIdTokenResponse'),
    newGetOpenIdTokenResponse,

    -- ** GetOpenIdTokenForDeveloperIdentity
    GetOpenIdTokenForDeveloperIdentity (GetOpenIdTokenForDeveloperIdentity'),
    newGetOpenIdTokenForDeveloperIdentity,
    GetOpenIdTokenForDeveloperIdentityResponse (GetOpenIdTokenForDeveloperIdentityResponse'),
    newGetOpenIdTokenForDeveloperIdentityResponse,

    -- ** DescribeIdentityPool
    DescribeIdentityPool (DescribeIdentityPool'),
    newDescribeIdentityPool,
    IdentityPool (IdentityPool'),
    newIdentityPool,

    -- ** SetPrincipalTagAttributeMap
    SetPrincipalTagAttributeMap (SetPrincipalTagAttributeMap'),
    newSetPrincipalTagAttributeMap,
    SetPrincipalTagAttributeMapResponse (SetPrincipalTagAttributeMapResponse'),
    newSetPrincipalTagAttributeMapResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetId
    GetId (GetId'),
    newGetId,
    GetIdResponse (GetIdResponse'),
    newGetIdResponse,

    -- ** DeleteIdentityPool
    DeleteIdentityPool (DeleteIdentityPool'),
    newDeleteIdentityPool,
    DeleteIdentityPoolResponse (DeleteIdentityPoolResponse'),
    newDeleteIdentityPoolResponse,

    -- ** UpdateIdentityPool
    UpdateIdentityPool (UpdateIdentityPool'),
    newUpdateIdentityPool,
    IdentityPool (IdentityPool'),
    newIdentityPool,

    -- ** UnlinkDeveloperIdentity
    UnlinkDeveloperIdentity (UnlinkDeveloperIdentity'),
    newUnlinkDeveloperIdentity,
    UnlinkDeveloperIdentityResponse (UnlinkDeveloperIdentityResponse'),
    newUnlinkDeveloperIdentityResponse,

    -- ** GetIdentityPoolRoles
    GetIdentityPoolRoles (GetIdentityPoolRoles'),
    newGetIdentityPoolRoles,
    GetIdentityPoolRolesResponse (GetIdentityPoolRolesResponse'),
    newGetIdentityPoolRolesResponse,

    -- ** ListIdentityPools (Paginated)
    ListIdentityPools (ListIdentityPools'),
    newListIdentityPools,
    ListIdentityPoolsResponse (ListIdentityPoolsResponse'),
    newListIdentityPoolsResponse,

    -- ** GetCredentialsForIdentity
    GetCredentialsForIdentity (GetCredentialsForIdentity'),
    newGetCredentialsForIdentity,
    GetCredentialsForIdentityResponse (GetCredentialsForIdentityResponse'),
    newGetCredentialsForIdentityResponse,

    -- ** GetPrincipalTagAttributeMap
    GetPrincipalTagAttributeMap (GetPrincipalTagAttributeMap'),
    newGetPrincipalTagAttributeMap,
    GetPrincipalTagAttributeMapResponse (GetPrincipalTagAttributeMapResponse'),
    newGetPrincipalTagAttributeMapResponse,

    -- ** DeleteIdentities
    DeleteIdentities (DeleteIdentities'),
    newDeleteIdentities,
    DeleteIdentitiesResponse (DeleteIdentitiesResponse'),
    newDeleteIdentitiesResponse,

    -- ** SetIdentityPoolRoles
    SetIdentityPoolRoles (SetIdentityPoolRoles'),
    newSetIdentityPoolRoles,
    SetIdentityPoolRolesResponse (SetIdentityPoolRolesResponse'),
    newSetIdentityPoolRolesResponse,

    -- ** ListIdentities
    ListIdentities (ListIdentities'),
    newListIdentities,
    ListIdentitiesResponse (ListIdentitiesResponse'),
    newListIdentitiesResponse,

    -- ** LookupDeveloperIdentity
    LookupDeveloperIdentity (LookupDeveloperIdentity'),
    newLookupDeveloperIdentity,
    LookupDeveloperIdentityResponse (LookupDeveloperIdentityResponse'),
    newLookupDeveloperIdentityResponse,

    -- ** UnlinkIdentity
    UnlinkIdentity (UnlinkIdentity'),
    newUnlinkIdentity,
    UnlinkIdentityResponse (UnlinkIdentityResponse'),
    newUnlinkIdentityResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeIdentity
    DescribeIdentity (DescribeIdentity'),
    newDescribeIdentity,
    IdentityDescription (IdentityDescription'),
    newIdentityDescription,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateIdentityPool
    CreateIdentityPool (CreateIdentityPool'),
    newCreateIdentityPool,
    IdentityPool (IdentityPool'),
    newIdentityPool,

    -- ** MergeDeveloperIdentities
    MergeDeveloperIdentities (MergeDeveloperIdentities'),
    newMergeDeveloperIdentities,
    MergeDeveloperIdentitiesResponse (MergeDeveloperIdentitiesResponse'),
    newMergeDeveloperIdentitiesResponse,

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

import Amazonka.CognitoIdentity.CreateIdentityPool
import Amazonka.CognitoIdentity.DeleteIdentities
import Amazonka.CognitoIdentity.DeleteIdentityPool
import Amazonka.CognitoIdentity.DescribeIdentity
import Amazonka.CognitoIdentity.DescribeIdentityPool
import Amazonka.CognitoIdentity.GetCredentialsForIdentity
import Amazonka.CognitoIdentity.GetId
import Amazonka.CognitoIdentity.GetIdentityPoolRoles
import Amazonka.CognitoIdentity.GetOpenIdToken
import Amazonka.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
import Amazonka.CognitoIdentity.GetPrincipalTagAttributeMap
import Amazonka.CognitoIdentity.Lens
import Amazonka.CognitoIdentity.ListIdentities
import Amazonka.CognitoIdentity.ListIdentityPools
import Amazonka.CognitoIdentity.ListTagsForResource
import Amazonka.CognitoIdentity.LookupDeveloperIdentity
import Amazonka.CognitoIdentity.MergeDeveloperIdentities
import Amazonka.CognitoIdentity.SetIdentityPoolRoles
import Amazonka.CognitoIdentity.SetPrincipalTagAttributeMap
import Amazonka.CognitoIdentity.TagResource
import Amazonka.CognitoIdentity.Types
import Amazonka.CognitoIdentity.UnlinkDeveloperIdentity
import Amazonka.CognitoIdentity.UnlinkIdentity
import Amazonka.CognitoIdentity.UntagResource
import Amazonka.CognitoIdentity.UpdateIdentityPool
import Amazonka.CognitoIdentity.Waiters

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
