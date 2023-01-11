{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.STS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2011-06-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Security Token Service
--
-- Security Token Service (STS) enables you to request temporary,
-- limited-privilege credentials for Identity and Access Management (IAM)
-- users or for users that you authenticate (federated users). This guide
-- provides descriptions of the STS API. For more information about using
-- this service, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials>.
module Amazonka.STS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ExpiredTokenException
    _ExpiredTokenException,

    -- ** IDPCommunicationErrorException
    _IDPCommunicationErrorException,

    -- ** IDPRejectedClaimException
    _IDPRejectedClaimException,

    -- ** InvalidAuthorizationMessageException
    _InvalidAuthorizationMessageException,

    -- ** InvalidIdentityTokenException
    _InvalidIdentityTokenException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** PackedPolicyTooLargeException
    _PackedPolicyTooLargeException,

    -- ** RegionDisabledException
    _RegionDisabledException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssumeRole
    AssumeRole (AssumeRole'),
    newAssumeRole,
    AssumeRoleResponse (AssumeRoleResponse'),
    newAssumeRoleResponse,

    -- ** AssumeRoleWithSAML
    AssumeRoleWithSAML (AssumeRoleWithSAML'),
    newAssumeRoleWithSAML,
    AssumeRoleWithSAMLResponse (AssumeRoleWithSAMLResponse'),
    newAssumeRoleWithSAMLResponse,

    -- ** AssumeRoleWithWebIdentity
    AssumeRoleWithWebIdentity (AssumeRoleWithWebIdentity'),
    newAssumeRoleWithWebIdentity,
    AssumeRoleWithWebIdentityResponse (AssumeRoleWithWebIdentityResponse'),
    newAssumeRoleWithWebIdentityResponse,

    -- ** DecodeAuthorizationMessage
    DecodeAuthorizationMessage (DecodeAuthorizationMessage'),
    newDecodeAuthorizationMessage,
    DecodeAuthorizationMessageResponse (DecodeAuthorizationMessageResponse'),
    newDecodeAuthorizationMessageResponse,

    -- ** GetAccessKeyInfo
    GetAccessKeyInfo (GetAccessKeyInfo'),
    newGetAccessKeyInfo,
    GetAccessKeyInfoResponse (GetAccessKeyInfoResponse'),
    newGetAccessKeyInfoResponse,

    -- ** GetCallerIdentity
    GetCallerIdentity (GetCallerIdentity'),
    newGetCallerIdentity,
    GetCallerIdentityResponse (GetCallerIdentityResponse'),
    newGetCallerIdentityResponse,

    -- ** GetFederationToken
    GetFederationToken (GetFederationToken'),
    newGetFederationToken,
    GetFederationTokenResponse (GetFederationTokenResponse'),
    newGetFederationTokenResponse,

    -- ** GetSessionToken
    GetSessionToken (GetSessionToken'),
    newGetSessionToken,
    GetSessionTokenResponse (GetSessionTokenResponse'),
    newGetSessionTokenResponse,

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

import Amazonka.STS.AssumeRole
import Amazonka.STS.AssumeRoleWithSAML
import Amazonka.STS.AssumeRoleWithWebIdentity
import Amazonka.STS.DecodeAuthorizationMessage
import Amazonka.STS.GetAccessKeyInfo
import Amazonka.STS.GetCallerIdentity
import Amazonka.STS.GetFederationToken
import Amazonka.STS.GetSessionToken
import Amazonka.STS.Lens
import Amazonka.STS.Types
import Amazonka.STS.Waiters

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
