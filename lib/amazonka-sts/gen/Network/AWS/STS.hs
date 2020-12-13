{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Security Token Service__
--
-- AWS Security Token Service (STS) enables you to request temporary, limited-privilege credentials for AWS Identity and Access Management (IAM) users or for users that you authenticate (federated users). This guide provides descriptions of the STS API. For more information about using this service, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials> .
module Network.AWS.STS
  ( -- * Service configuration
    stsService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetCallerIdentity
    module Network.AWS.STS.GetCallerIdentity,

    -- ** AssumeRole
    module Network.AWS.STS.AssumeRole,

    -- ** GetAccessKeyInfo
    module Network.AWS.STS.GetAccessKeyInfo,

    -- ** DecodeAuthorizationMessage
    module Network.AWS.STS.DecodeAuthorizationMessage,

    -- ** AssumeRoleWithWebIdentity
    module Network.AWS.STS.AssumeRoleWithWebIdentity,

    -- ** GetFederationToken
    module Network.AWS.STS.GetFederationToken,

    -- ** GetSessionToken
    module Network.AWS.STS.GetSessionToken,

    -- ** AssumeRoleWithSAML
    module Network.AWS.STS.AssumeRoleWithSAML,

    -- * Types

    -- ** AssumedRoleUser
    AssumedRoleUser (..),
    mkAssumedRoleUser,
    aruARN,
    aruAssumedRoleId,

    -- ** FederatedUser
    FederatedUser (..),
    mkFederatedUser,
    fuARN,
    fuFederatedUserId,

    -- ** PolicyDescriptorType
    PolicyDescriptorType (..),
    mkPolicyDescriptorType,
    pdtArn,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.STS.AssumeRole
import Network.AWS.STS.AssumeRoleWithSAML
import Network.AWS.STS.AssumeRoleWithWebIdentity
import Network.AWS.STS.DecodeAuthorizationMessage
import Network.AWS.STS.GetAccessKeyInfo
import Network.AWS.STS.GetCallerIdentity
import Network.AWS.STS.GetFederationToken
import Network.AWS.STS.GetSessionToken
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
