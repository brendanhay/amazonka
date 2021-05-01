{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Security Token Service
--
-- AWS Security Token Service (STS) enables you to request temporary,
-- limited-privilege credentials for AWS Identity and Access Management
-- (IAM) users or for users that you authenticate (federated users). This
-- guide provides descriptions of the STS API. For more information about
-- using this service, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials>.
module Network.AWS.STS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** RegionDisabledException
    _RegionDisabledException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** PackedPolicyTooLargeException
    _PackedPolicyTooLargeException,

    -- ** InvalidIdentityTokenException
    _InvalidIdentityTokenException,

    -- ** ExpiredTokenException
    _ExpiredTokenException,

    -- ** InvalidAuthorizationMessageException
    _InvalidAuthorizationMessageException,

    -- ** IDPRejectedClaimException
    _IDPRejectedClaimException,

    -- ** IDPCommunicationErrorException
    _IDPCommunicationErrorException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssumeRoleWithWebIdentity
    AssumeRoleWithWebIdentity (AssumeRoleWithWebIdentity'),
    newAssumeRoleWithWebIdentity,
    AssumeRoleWithWebIdentityResponse (AssumeRoleWithWebIdentityResponse'),
    newAssumeRoleWithWebIdentityResponse,

    -- ** GetAccessKeyInfo
    GetAccessKeyInfo (GetAccessKeyInfo'),
    newGetAccessKeyInfo,
    GetAccessKeyInfoResponse (GetAccessKeyInfoResponse'),
    newGetAccessKeyInfoResponse,

    -- ** GetSessionToken
    GetSessionToken (GetSessionToken'),
    newGetSessionToken,
    GetSessionTokenResponse (GetSessionTokenResponse'),
    newGetSessionTokenResponse,

    -- ** AssumeRole
    AssumeRole (AssumeRole'),
    newAssumeRole,
    AssumeRoleResponse (AssumeRoleResponse'),
    newAssumeRoleResponse,

    -- ** GetCallerIdentity
    GetCallerIdentity (GetCallerIdentity'),
    newGetCallerIdentity,
    GetCallerIdentityResponse (GetCallerIdentityResponse'),
    newGetCallerIdentityResponse,

    -- ** DecodeAuthorizationMessage
    DecodeAuthorizationMessage (DecodeAuthorizationMessage'),
    newDecodeAuthorizationMessage,
    DecodeAuthorizationMessageResponse (DecodeAuthorizationMessageResponse'),
    newDecodeAuthorizationMessageResponse,

    -- ** AssumeRoleWithSAML
    AssumeRoleWithSAML (AssumeRoleWithSAML'),
    newAssumeRoleWithSAML,
    AssumeRoleWithSAMLResponse (AssumeRoleWithSAMLResponse'),
    newAssumeRoleWithSAMLResponse,

    -- ** GetFederationToken
    GetFederationToken (GetFederationToken'),
    newGetFederationToken,
    GetFederationTokenResponse (GetFederationTokenResponse'),
    newGetFederationTokenResponse,

    -- * Types

    -- ** AssumedRoleUser
    AssumedRoleUser (AssumedRoleUser'),
    newAssumedRoleUser,

    -- ** FederatedUser
    FederatedUser (FederatedUser'),
    newFederatedUser,

    -- ** PolicyDescriptorType
    PolicyDescriptorType (PolicyDescriptorType'),
    newPolicyDescriptorType,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.STS.AssumeRole
import Network.AWS.STS.AssumeRoleWithSAML
import Network.AWS.STS.AssumeRoleWithWebIdentity
import Network.AWS.STS.DecodeAuthorizationMessage
import Network.AWS.STS.GetAccessKeyInfo
import Network.AWS.STS.GetCallerIdentity
import Network.AWS.STS.GetFederationToken
import Network.AWS.STS.GetSessionToken
import Network.AWS.STS.Lens
import Network.AWS.STS.Types
import Network.AWS.STS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'STS'.

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
