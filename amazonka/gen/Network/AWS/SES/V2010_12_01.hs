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
module Network.AWS.SES.V2010_12_01 (module Export) where

import Network.AWS.SES.V2010_12_01.DeleteIdentity as Export
import Network.AWS.SES.V2010_12_01.DeleteVerifiedEmailAddress as Export
import Network.AWS.SES.V2010_12_01.GetIdentityDkimAttributes as Export
import Network.AWS.SES.V2010_12_01.GetIdentityNotificationAttributes as Export
import Network.AWS.SES.V2010_12_01.GetIdentityVerificationAttributes as Export
import Network.AWS.SES.V2010_12_01.GetSendQuota as Export
import Network.AWS.SES.V2010_12_01.GetSendStatistics as Export
import Network.AWS.SES.V2010_12_01.ListIdentities as Export
import Network.AWS.SES.V2010_12_01.ListVerifiedEmailAddresses as Export
import Network.AWS.SES.V2010_12_01.SendEmail as Export
import Network.AWS.SES.V2010_12_01.SendRawEmail as Export
import Network.AWS.SES.V2010_12_01.SetIdentityDkimEnabled as Export
import Network.AWS.SES.V2010_12_01.SetIdentityFeedbackForwardingEnabled as Export
import Network.AWS.SES.V2010_12_01.SetIdentityNotificationTopic as Export
import Network.AWS.SES.V2010_12_01.Types as Export
import Network.AWS.SES.V2010_12_01.VerifyDomainDkim as Export
import Network.AWS.SES.V2010_12_01.VerifyDomainIdentity as Export
import Network.AWS.SES.V2010_12_01.VerifyEmailAddress as Export
import Network.AWS.SES.V2010_12_01.VerifyEmailIdentity as Export
