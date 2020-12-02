{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Security Token Service__
--
-- The AWS Security Token Service (STS) is a web service that enables you to request temporary, limited-privilege credentials for AWS Identity and Access Management (IAM) users or for users that you authenticate (federated users). This guide provides descriptions of the STS API. For more detailed information about using this service, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials> .
--
-- For information about setting up signatures and authorization through the API, go to <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests> in the /AWS General Reference/ . For general information about the Query API, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests> in /Using IAM/ . For information about using security tokens with other AWS products, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-services-that-work-with-iam.html AWS Services That Work with IAM> in the /IAM User Guide/ .
--
-- If you're new to AWS and need additional technical information about a specific AWS product, you can find the product's technical documentation at <http://aws.amazon.com/documentation/ http://aws.amazon.com/documentation/> .
--
-- __Endpoints__
--
-- The AWS Security Token Service (STS) has a default endpoint of https://sts.amazonaws.com that maps to the US East (N. Virginia) region. Additional regions are available and are activated by default. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /IAM User Guide/ .
--
-- For information about STS endpoints, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#sts_region Regions and Endpoints> in the /AWS General Reference/ .
--
-- __Recording API requests__
--
-- STS supports AWS CloudTrail, which is a service that records AWS calls for your AWS account and delivers log files to an Amazon S3 bucket. By using information collected by CloudTrail, you can determine what requests were successfully made to STS, who made the request, when it was made, and so on. To learn more about CloudTrail, including how to turn it on and find your log files, see the <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html AWS CloudTrail User Guide> .
--
module Network.AWS.STS
    (
    -- * Service Configuration
      sts

    -- * Errors
    -- $errors

    -- ** MalformedPolicyDocumentException
    , _MalformedPolicyDocumentException

    -- ** InvalidAuthorizationMessageException
    , _InvalidAuthorizationMessageException

    -- ** PackedPolicyTooLargeException
    , _PackedPolicyTooLargeException

    -- ** RegionDisabledException
    , _RegionDisabledException

    -- ** IdPCommunicationErrorException
    , _IdPCommunicationErrorException

    -- ** InvalidIdentityTokenException
    , _InvalidIdentityTokenException

    -- ** ExpiredTokenException
    , _ExpiredTokenException

    -- ** IdPRejectedClaimException
    , _IdPRejectedClaimException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetCallerIdentity
    , module Network.AWS.STS.GetCallerIdentity

    -- ** AssumeRole
    , module Network.AWS.STS.AssumeRole

    -- ** DecodeAuthorizationMessage
    , module Network.AWS.STS.DecodeAuthorizationMessage

    -- ** AssumeRoleWithWebIdentity
    , module Network.AWS.STS.AssumeRoleWithWebIdentity

    -- ** GetFederationToken
    , module Network.AWS.STS.GetFederationToken

    -- ** GetSessionToken
    , module Network.AWS.STS.GetSessionToken

    -- ** AssumeRoleWithSAML
    , module Network.AWS.STS.AssumeRoleWithSAML

    -- * Types

    -- ** AssumedRoleUser
    , AssumedRoleUser
    , assumedRoleUser
    , aruAssumedRoleId
    , aruARN

    -- ** FederatedUser
    , FederatedUser
    , federatedUser
    , fuFederatedUserId
    , fuARN
    ) where

import Network.AWS.STS.AssumeRole
import Network.AWS.STS.AssumeRoleWithSAML
import Network.AWS.STS.AssumeRoleWithWebIdentity
import Network.AWS.STS.DecodeAuthorizationMessage
import Network.AWS.STS.GetCallerIdentity
import Network.AWS.STS.GetFederationToken
import Network.AWS.STS.GetSessionToken
import Network.AWS.STS.Types
import Network.AWS.STS.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'STS'.
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
