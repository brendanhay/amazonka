{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Email Service (Amazon SES) is a cost-effective outbound-only
-- email-sending service built on the reliable and scalable infrastructure
-- that Amazon.com has developed to serve its own customer base. With Amazon
-- SES, you can send transactional email, marketing messages, or any other
-- type of high-quality content and you only pay for what you use. Along with
-- high deliverability, Amazon SES provides easy, real-time access to your
-- sending statistics and built-in notifications for bounces, complaints, and
-- deliveries to help you fine-tune your email-sending strategy.
module Network.AWS.SES.V2010_12_01
    ( module Network.AWS.SES.V2010_12_01.DeleteIdentity
    , module Network.AWS.SES.V2010_12_01.DeleteVerifiedEmailAddress
    , module Network.AWS.SES.V2010_12_01.GetIdentityDkimAttributes
    , module Network.AWS.SES.V2010_12_01.GetIdentityNotificationAttributes
    , module Network.AWS.SES.V2010_12_01.GetIdentityVerificationAttributes
    , module Network.AWS.SES.V2010_12_01.GetSendQuota
    , module Network.AWS.SES.V2010_12_01.GetSendStatistics
    , module Network.AWS.SES.V2010_12_01.Lenses
    , module Network.AWS.SES.V2010_12_01.ListIdentities
    , module Network.AWS.SES.V2010_12_01.ListVerifiedEmailAddresses
    , module Network.AWS.SES.V2010_12_01.SendEmail
    , module Network.AWS.SES.V2010_12_01.SendRawEmail
    , module Network.AWS.SES.V2010_12_01.SetIdentityDkimEnabled
    , module Network.AWS.SES.V2010_12_01.SetIdentityFeedbackForwardingEnabled
    , module Network.AWS.SES.V2010_12_01.SetIdentityNotificationTopic
    , module Network.AWS.SES.V2010_12_01.Types
    , module Network.AWS.SES.V2010_12_01.VerifyDomainDkim
    , module Network.AWS.SES.V2010_12_01.VerifyDomainIdentity
    , module Network.AWS.SES.V2010_12_01.VerifyEmailAddress
    , module Network.AWS.SES.V2010_12_01.VerifyEmailIdentity
    ) where

import Network.AWS.SES.V2010_12_01.DeleteIdentity
import Network.AWS.SES.V2010_12_01.DeleteVerifiedEmailAddress
import Network.AWS.SES.V2010_12_01.GetIdentityDkimAttributes
import Network.AWS.SES.V2010_12_01.GetIdentityNotificationAttributes
import Network.AWS.SES.V2010_12_01.GetIdentityVerificationAttributes
import Network.AWS.SES.V2010_12_01.GetSendQuota
import Network.AWS.SES.V2010_12_01.GetSendStatistics
import Network.AWS.SES.V2010_12_01.Lenses
import Network.AWS.SES.V2010_12_01.ListIdentities
import Network.AWS.SES.V2010_12_01.ListVerifiedEmailAddresses
import Network.AWS.SES.V2010_12_01.SendEmail
import Network.AWS.SES.V2010_12_01.SendRawEmail
import Network.AWS.SES.V2010_12_01.SetIdentityDkimEnabled
import Network.AWS.SES.V2010_12_01.SetIdentityFeedbackForwardingEnabled
import Network.AWS.SES.V2010_12_01.SetIdentityNotificationTopic
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.SES.V2010_12_01.VerifyDomainDkim
import Network.AWS.SES.V2010_12_01.VerifyDomainIdentity
import Network.AWS.SES.V2010_12_01.VerifyEmailAddress
import Network.AWS.SES.V2010_12_01.VerifyEmailIdentity
