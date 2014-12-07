-- Module      : Network.AWS.SES
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Simple Email Service (Amazon SES) is a cost-effective outbound-only
-- email-sending service built on the reliable and scalable infrastructure that
-- Amazon.com has developed to serve its own customer base. With Amazon SES, you
-- can send transactional email, marketing messages, or any other type of
-- high-quality content and you only pay for what you use. Along with high
-- deliverability, Amazon SES provides easy, real-time access to your sending
-- statistics and built-in notifications for bounces, complaints, and deliveries
-- to help you fine-tune your email-sending strategy.
module Network.AWS.SES
    ( module Network.AWS.SES.DeleteIdentity
    , module Network.AWS.SES.DeleteVerifiedEmailAddress
    , module Network.AWS.SES.GetIdentityDkimAttributes
    , module Network.AWS.SES.GetIdentityNotificationAttributes
    , module Network.AWS.SES.GetIdentityVerificationAttributes
    , module Network.AWS.SES.GetSendQuota
    , module Network.AWS.SES.GetSendStatistics
    , module Network.AWS.SES.ListIdentities
    , module Network.AWS.SES.ListVerifiedEmailAddresses
    , module Network.AWS.SES.SendEmail
    , module Network.AWS.SES.SendRawEmail
    , module Network.AWS.SES.SetIdentityDkimEnabled
    , module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
    , module Network.AWS.SES.SetIdentityNotificationTopic
    , module Network.AWS.SES.Types
    , module Network.AWS.SES.VerifyDomainDkim
    , module Network.AWS.SES.VerifyDomainIdentity
    , module Network.AWS.SES.VerifyEmailAddress
    , module Network.AWS.SES.VerifyEmailIdentity
    , module Network.AWS.SES.Waiters
    ) where

import Network.AWS.SES.DeleteIdentity
import Network.AWS.SES.DeleteVerifiedEmailAddress
import Network.AWS.SES.GetIdentityDkimAttributes
import Network.AWS.SES.GetIdentityNotificationAttributes
import Network.AWS.SES.GetIdentityVerificationAttributes
import Network.AWS.SES.GetSendQuota
import Network.AWS.SES.GetSendStatistics
import Network.AWS.SES.ListIdentities
import Network.AWS.SES.ListVerifiedEmailAddresses
import Network.AWS.SES.SendEmail
import Network.AWS.SES.SendRawEmail
import Network.AWS.SES.SetIdentityDkimEnabled
import Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
import Network.AWS.SES.SetIdentityNotificationTopic
import Network.AWS.SES.Types
import Network.AWS.SES.VerifyDomainDkim
import Network.AWS.SES.VerifyDomainIdentity
import Network.AWS.SES.VerifyEmailAddress
import Network.AWS.SES.VerifyEmailIdentity
import Network.AWS.SES.Waiters
