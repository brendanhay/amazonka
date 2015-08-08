{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Amazon Simple Email Service
--
-- This is the API Reference for Amazon Simple Email Service (Amazon SES).
-- This documentation is intended to be used in conjunction with the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html Amazon SES Developer Guide>.
--
-- For a list of Amazon SES endpoints to use in service requests, see
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/regions.html Regions and Amazon SES>
-- in the Amazon SES Developer Guide.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.SES
    (
    -- * Service Description
      SES

    -- * Error Matchers
    -- $errors
    , _MessageRejected
    , _InvalidPolicyException

    -- * Operations
    -- $operations

    -- ** GetSendQuota
    , module Network.AWS.SES.GetSendQuota

    -- ** DeleteIdentityPolicy
    , module Network.AWS.SES.DeleteIdentityPolicy

    -- ** PutIdentityPolicy
    , module Network.AWS.SES.PutIdentityPolicy

    -- ** SetIdentityDkimEnabled
    , module Network.AWS.SES.SetIdentityDkimEnabled

    -- ** GetIdentityNotificationAttributes
    , module Network.AWS.SES.GetIdentityNotificationAttributes

    -- ** ListIdentityPolicies
    , module Network.AWS.SES.ListIdentityPolicies

    -- ** SetIdentityFeedbackForwardingEnabled
    , module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled

    -- ** GetIdentityVerificationAttributes
    , module Network.AWS.SES.GetIdentityVerificationAttributes

    -- ** GetIdentityPolicies
    , module Network.AWS.SES.GetIdentityPolicies

    -- ** VerifyDomainIdentity
    , module Network.AWS.SES.VerifyDomainIdentity

    -- ** VerifyDomainDkim
    , module Network.AWS.SES.VerifyDomainDkim

    -- ** SendRawEmail
    , module Network.AWS.SES.SendRawEmail

    -- ** GetIdentityDkimAttributes
    , module Network.AWS.SES.GetIdentityDkimAttributes

    -- ** DeleteIdentity
    , module Network.AWS.SES.DeleteIdentity

    -- ** GetSendStatistics
    , module Network.AWS.SES.GetSendStatistics

    -- ** ListIdentities (Paginated)
    , module Network.AWS.SES.ListIdentities
    -- $pager

    -- ** DeleteVerifiedEmailAddress
    , module Network.AWS.SES.DeleteVerifiedEmailAddress

    -- ** VerifyEmailAddress
    , module Network.AWS.SES.VerifyEmailAddress

    -- ** VerifyEmailIdentity
    , module Network.AWS.SES.VerifyEmailIdentity

    -- ** SendEmail
    , module Network.AWS.SES.SendEmail

    -- ** ListVerifiedEmailAddresses
    , module Network.AWS.SES.ListVerifiedEmailAddresses

    -- ** SetIdentityNotificationTopic
    , module Network.AWS.SES.SetIdentityNotificationTopic

    -- * Types

    -- ** IdentityType
    , IdentityType (..)

    -- ** NotificationType
    , NotificationType (..)

    -- ** VerificationStatus
    , VerificationStatus (..)

    -- ** Body
    , Body
    , body
    , bText
    , bHTML

    -- ** Content
    , Content
    , content
    , cCharset
    , cData

    -- ** Destination
    , Destination
    , destination
    , dBCCAddresses
    , dCCAddresses
    , dToAddresses

    -- ** IdentityDkimAttributes
    , IdentityDkimAttributes
    , identityDkimAttributes
    , idaDkimTokens
    , idaDkimEnabled
    , idaDkimVerificationStatus

    -- ** IdentityNotificationAttributes
    , IdentityNotificationAttributes
    , identityNotificationAttributes
    , inaBounceTopic
    , inaComplaintTopic
    , inaDeliveryTopic
    , inaForwardingEnabled

    -- ** IdentityVerificationAttributes
    , IdentityVerificationAttributes
    , identityVerificationAttributes
    , ivaVerificationToken
    , ivaVerificationStatus

    -- ** Message
    , Message
    , message
    , mSubject
    , mBody

    -- ** RawMessage
    , RawMessage
    , rawMessage
    , rmData

    -- ** SendDataPoint
    , SendDataPoint
    , sendDataPoint
    , sdpRejects
    , sdpComplaints
    , sdpDeliveryAttempts
    , sdpBounces
    , sdpTimestamp
    ) where

import           Network.AWS.SES.DeleteIdentity
import           Network.AWS.SES.DeleteIdentityPolicy
import           Network.AWS.SES.DeleteVerifiedEmailAddress
import           Network.AWS.SES.GetIdentityDkimAttributes
import           Network.AWS.SES.GetIdentityNotificationAttributes
import           Network.AWS.SES.GetIdentityPolicies
import           Network.AWS.SES.GetIdentityVerificationAttributes
import           Network.AWS.SES.GetSendQuota
import           Network.AWS.SES.GetSendStatistics
import           Network.AWS.SES.ListIdentities
import           Network.AWS.SES.ListIdentityPolicies
import           Network.AWS.SES.ListVerifiedEmailAddresses
import           Network.AWS.SES.PutIdentityPolicy
import           Network.AWS.SES.SendEmail
import           Network.AWS.SES.SendRawEmail
import           Network.AWS.SES.SetIdentityDkimEnabled
import           Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
import           Network.AWS.SES.SetIdentityNotificationTopic
import           Network.AWS.SES.Types
import           Network.AWS.SES.VerifyDomainDkim
import           Network.AWS.SES.VerifyDomainIdentity
import           Network.AWS.SES.VerifyEmailAddress
import           Network.AWS.SES.VerifyEmailIdentity
import           Network.AWS.SES.Waiters

{- $errors
Error matchers are intended to be used with the <http://hackage.haskell.org/package/lens lens>
library functions provided by the "Control.Exception.Lens" module. This allows
the user to catch (and rethrow) service specific errors returned by 'SES'.
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
Waiters poll by repeatedly send a request until some remote success condition
specified by the 'Wait' configuration is fulfilled. The 'Wait' configuration
specifies how many attempts should be made, in addition to delay and retry strategies.
-}

{- $pager
This operation can return paginated results.
-}
