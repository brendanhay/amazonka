{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Cognito Federated Identities__ 
--
-- Amazon Cognito Federated Identities is a web service that delivers scoped temporary credentials to mobile devices and other untrusted environments. It uniquely identifies a device and supplies the user with a consistent identity over the lifetime of an application.
-- Using Amazon Cognito Federated Identities, you can enable authentication with one or more third-party identity providers (Facebook, Google, or Login with Amazon) or an Amazon Cognito user pool, and you can also choose to support unauthenticated access from your app. Cognito delivers a unique identifier for each user and acts as an OpenID token provider trusted by AWS Security Token Service (STS) to access temporary, limited-privilege AWS credentials.
-- For a description of the authentication flow from the Amazon Cognito Developer Guide see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Authentication Flow> .
-- For more information see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-identity.html Amazon Cognito Federated Identities> .
module Network.AWS.CognitoIdentity
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidIdentityPoolConfigurationException
    , _InvalidIdentityPoolConfigurationException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** NotAuthorizedException
    , _NotAuthorizedException

    -- ** InternalErrorException
    , _InternalErrorException

    -- ** ExternalServiceException
    , _ExternalServiceException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** ResourceConflictException
    , _ResourceConflictException

    -- ** DeveloperUserAlreadyRegisteredException
    , _DeveloperUserAlreadyRegisteredException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetOpenIdToken 
    , module Network.AWS.CognitoIdentity.GetOpenIdToken

    -- ** GetOpenIdTokenForDeveloperIdentity 
    , module Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity

    -- ** DescribeIdentityPool 
    , module Network.AWS.CognitoIdentity.DescribeIdentityPool

    -- ** ListTagsForResource 
    , module Network.AWS.CognitoIdentity.ListTagsForResource

    -- ** GetId 
    , module Network.AWS.CognitoIdentity.GetId

    -- ** DeleteIdentityPool 
    , module Network.AWS.CognitoIdentity.DeleteIdentityPool

    -- ** UpdateIdentityPool 
    , module Network.AWS.CognitoIdentity.UpdateIdentityPool

    -- ** UnlinkDeveloperIdentity 
    , module Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity

    -- ** GetIdentityPoolRoles 
    , module Network.AWS.CognitoIdentity.GetIdentityPoolRoles

    -- ** ListIdentityPools (Paginated)
    , module Network.AWS.CognitoIdentity.ListIdentityPools

    -- ** GetCredentialsForIdentity 
    , module Network.AWS.CognitoIdentity.GetCredentialsForIdentity

    -- ** DeleteIdentities 
    , module Network.AWS.CognitoIdentity.DeleteIdentities

    -- ** SetIdentityPoolRoles 
    , module Network.AWS.CognitoIdentity.SetIdentityPoolRoles

    -- ** ListIdentities 
    , module Network.AWS.CognitoIdentity.ListIdentities

    -- ** LookupDeveloperIdentity 
    , module Network.AWS.CognitoIdentity.LookupDeveloperIdentity

    -- ** UnlinkIdentity 
    , module Network.AWS.CognitoIdentity.UnlinkIdentity

    -- ** TagResource 
    , module Network.AWS.CognitoIdentity.TagResource

    -- ** DescribeIdentity 
    , module Network.AWS.CognitoIdentity.DescribeIdentity

    -- ** UntagResource 
    , module Network.AWS.CognitoIdentity.UntagResource

    -- ** CreateIdentityPool 
    , module Network.AWS.CognitoIdentity.CreateIdentityPool

    -- ** MergeDeveloperIdentities 
    , module Network.AWS.CognitoIdentity.MergeDeveloperIdentities

    -- * Types

    -- ** RoleType
    , RoleType (..)

    -- ** IdentityProviderToken
    , IdentityProviderToken (..)

    -- ** CognitoIdentityProviderClientId
    , CognitoIdentityProviderClientId (..)

    -- ** DeveloperUserIdentifier
    , DeveloperUserIdentifier (..)

    -- ** SecretKeyString
    , SecretKeyString (..)

    -- ** IdentityPoolId
    , IdentityPoolId (..)

    -- ** MappingRule
    , MappingRule (..)
    , mkMappingRule
    , mrClaim
    , mrMatchType
    , mrValue
    , mrRoleARN

    -- ** UnprocessedIdentityId
    , UnprocessedIdentityId (..)
    , mkUnprocessedIdentityId
    , uiiErrorCode
    , uiiIdentityId

    -- ** IdentityProviderId
    , IdentityProviderId (..)

    -- ** IdentityPoolName
    , IdentityPoolName (..)

    -- ** Credentials
    , Credentials (..)
    , mkCredentials
    , cAccessKeyId
    , cExpiration
    , cSecretKey
    , cSessionToken

    -- ** CognitoIdentityProvider
    , CognitoIdentityProvider (..)
    , mkCognitoIdentityProvider
    , cipClientId
    , cipProviderName
    , cipServerSideTokenCheck

    -- ** IdentityProviderName
    , IdentityProviderName (..)

    -- ** IdentityDescription
    , IdentityDescription (..)
    , mkIdentityDescription
    , idCreationDate
    , idIdentityId
    , idLastModifiedDate
    , idLogins

    -- ** ARNString
    , ARNString (..)

    -- ** DeveloperProviderName
    , DeveloperProviderName (..)

    -- ** RoleMapping
    , RoleMapping (..)
    , mkRoleMapping
    , rmType
    , rmAmbiguousRoleResolution
    , rmRulesConfiguration

    -- ** AmbiguousRoleResolutionType
    , AmbiguousRoleResolutionType (..)

    -- ** MappingRuleMatchType
    , MappingRuleMatchType (..)

    -- ** AccountId
    , AccountId (..)

    -- ** IdentityPool
    , IdentityPool (..)
    , mkIdentityPool
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities
    , ipAllowClassicFlow
    , ipCognitoIdentityProviders
    , ipDeveloperProviderName
    , ipIdentityPoolTags
    , ipOpenIdConnectProviderARNs
    , ipSamlProviderARNs
    , ipSupportedLoginProviders

    -- ** CognitoErrorCode
    , CognitoErrorCode (..)

    -- ** RulesConfigurationType
    , RulesConfigurationType (..)
    , mkRulesConfigurationType
    , rctRules

    -- ** RoleMappingType
    , RoleMappingType (..)

    -- ** IdentityId
    , IdentityId (..)

    -- ** PaginationKey
    , PaginationKey (..)

    -- ** TagKeysType
    , TagKeysType (..)

    -- ** IdentityPoolShortDescription
    , IdentityPoolShortDescription (..)
    , mkIdentityPoolShortDescription
    , ipsdIdentityPoolId
    , ipsdIdentityPoolName

    -- ** TagValueType
    , TagValueType (..)

    -- ** Token
    , Token (..)

    -- ** NextToken
    , NextToken (..)

    -- ** Claim
    , Claim (..)

    -- ** Value
    , Value (..)

    -- ** RoleARN
    , RoleARN (..)

    -- ** CustomRoleArn
    , CustomRoleArn (..)

    -- ** AccessKeyId
    , AccessKeyId (..)

    -- ** SessionToken
    , SessionToken (..)

    -- ** ProviderName
    , ProviderName (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Waiters
import Network.AWS.CognitoIdentity.GetOpenIdToken
import Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
import Network.AWS.CognitoIdentity.DescribeIdentityPool
import Network.AWS.CognitoIdentity.ListTagsForResource
import Network.AWS.CognitoIdentity.GetId
import Network.AWS.CognitoIdentity.DeleteIdentityPool
import Network.AWS.CognitoIdentity.UpdateIdentityPool
import Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
import Network.AWS.CognitoIdentity.GetIdentityPoolRoles
import Network.AWS.CognitoIdentity.ListIdentityPools
import Network.AWS.CognitoIdentity.GetCredentialsForIdentity
import Network.AWS.CognitoIdentity.DeleteIdentities
import Network.AWS.CognitoIdentity.SetIdentityPoolRoles
import Network.AWS.CognitoIdentity.ListIdentities
import Network.AWS.CognitoIdentity.LookupDeveloperIdentity
import Network.AWS.CognitoIdentity.UnlinkIdentity
import Network.AWS.CognitoIdentity.TagResource
import Network.AWS.CognitoIdentity.DescribeIdentity
import Network.AWS.CognitoIdentity.UntagResource
import Network.AWS.CognitoIdentity.CreateIdentityPool
import Network.AWS.CognitoIdentity.MergeDeveloperIdentities
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CognitoIdentity'.
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
