{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Cognito__
--
-- Amazon Cognito is a web service that delivers scoped temporary credentials to mobile devices and other untrusted environments. Amazon Cognito uniquely identifies a device and supplies the user with a consistent identity over the lifetime of an application.
--
-- Using Amazon Cognito, you can enable authentication with one or more third-party identity providers (Facebook, Google, or Login with Amazon), and you can also choose to support unauthenticated access from your app. Cognito delivers a unique identifier for each user and acts as an OpenID token provider trusted by AWS Security Token Service (STS) to access temporary, limited-privilege AWS credentials.
--
-- To provide end-user credentials, first make an unsigned call to 'GetId' . If the end user is authenticated with one of the supported identity providers, set the @Logins@ map with the identity provider token. @GetId@ returns a unique identifier for the user.
--
-- Next, make an unsigned call to 'GetCredentialsForIdentity' . This call expects the same @Logins@ map as the @GetId@ call, as well as the @IdentityID@ originally returned by @GetId@ . Assuming your identity pool has been configured via the 'SetIdentityPoolRoles' operation, @GetCredentialsForIdentity@ will return AWS credentials for your use. If your pool has not been configured with @SetIdentityPoolRoles@ , or if you want to follow legacy flow, make an unsigned call to 'GetOpenIdToken' , which returns the OpenID token necessary to call STS and retrieve AWS credentials. This call expects the same @Logins@ map as the @GetId@ call, as well as the @IdentityID@ originally returned by @GetId@ . The token returned by @GetOpenIdToken@ can be passed to the STS operation <http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html AssumeRoleWithWebIdentity> to retrieve AWS credentials.
--
-- If you want to use Amazon Cognito in an Android, iOS, or Unity application, you will probably want to make API calls via the AWS Mobile SDK. To learn more, see the <http://docs.aws.amazon.com/mobile/index.html AWS Mobile SDK Developer Guide> .
--
module Network.AWS.CognitoIdentity
    (
    -- * Service Configuration
      cognitoIdentity

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

    -- ** ListIdentityPools
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

    -- ** DescribeIdentity
    , module Network.AWS.CognitoIdentity.DescribeIdentity

    -- ** CreateIdentityPool
    , module Network.AWS.CognitoIdentity.CreateIdentityPool

    -- ** MergeDeveloperIdentities
    , module Network.AWS.CognitoIdentity.MergeDeveloperIdentities

    -- * Types

    -- ** AmbiguousRoleResolutionType
    , AmbiguousRoleResolutionType (..)

    -- ** CognitoErrorCode
    , CognitoErrorCode (..)

    -- ** MappingRuleMatchType
    , MappingRuleMatchType (..)

    -- ** RoleMappingType
    , RoleMappingType (..)

    -- ** CognitoIdentityProvider
    , CognitoIdentityProvider
    , cognitoIdentityProvider
    , cipClientId
    , cipServerSideTokenCheck
    , cipProviderName

    -- ** Credentials
    , Credentials
    , credentials
    , cSessionToken
    , cExpiration
    , cSecretKey
    , cAccessKeyId

    -- ** IdentityDescription
    , IdentityDescription
    , identityDescription
    , idLastModifiedDate
    , idCreationDate
    , idLogins
    , idIdentityId

    -- ** IdentityPool
    , IdentityPool
    , identityPool
    , ipSamlProviderARNs
    , ipSupportedLoginProviders
    , ipDeveloperProviderName
    , ipOpenIdConnectProviderARNs
    , ipCognitoIdentityProviders
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities

    -- ** IdentityPoolShortDescription
    , IdentityPoolShortDescription
    , identityPoolShortDescription
    , ipsdIdentityPoolId
    , ipsdIdentityPoolName

    -- ** MappingRule
    , MappingRule
    , mappingRule
    , mrClaim
    , mrMatchType
    , mrValue
    , mrRoleARN

    -- ** RoleMapping
    , RoleMapping
    , roleMapping
    , rmRulesConfiguration
    , rmAmbiguousRoleResolution
    , rmType

    -- ** RulesConfigurationType
    , RulesConfigurationType
    , rulesConfigurationType
    , rctRules

    -- ** UnprocessedIdentityId
    , UnprocessedIdentityId
    , unprocessedIdentityId
    , uiiErrorCode
    , uiiIdentityId
    ) where

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
import Network.AWS.CognitoIdentity.ListIdentities
import Network.AWS.CognitoIdentity.ListIdentityPools
import Network.AWS.CognitoIdentity.LookupDeveloperIdentity
import Network.AWS.CognitoIdentity.MergeDeveloperIdentities
import Network.AWS.CognitoIdentity.SetIdentityPoolRoles
import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
import Network.AWS.CognitoIdentity.UnlinkIdentity
import Network.AWS.CognitoIdentity.UpdateIdentityPool
import Network.AWS.CognitoIdentity.Waiters

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
