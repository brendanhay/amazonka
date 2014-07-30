{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SES.V2010_12_01.Lenses where

import Control.Lens.TH
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.SES.V2010_12_01.GetSendQuota
import Network.AWS.SES.V2010_12_01.GetIdentityNotificationAttributes
import Network.AWS.SES.V2010_12_01.SetIdentityDkimEnabled
import Network.AWS.SES.V2010_12_01.SetIdentityFeedbackForwardingEnabled
import Network.AWS.SES.V2010_12_01.GetIdentityVerificationAttributes
import Network.AWS.SES.V2010_12_01.VerifyDomainIdentity
import Network.AWS.SES.V2010_12_01.GetIdentityDkimAttributes
import Network.AWS.SES.V2010_12_01.VerifyDomainDkim
import Network.AWS.SES.V2010_12_01.SendRawEmail
import Network.AWS.SES.V2010_12_01.GetSendStatistics
import Network.AWS.SES.V2010_12_01.DeleteIdentity
import Network.AWS.SES.V2010_12_01.ListIdentities
import Network.AWS.SES.V2010_12_01.VerifyEmailIdentity
import Network.AWS.SES.V2010_12_01.VerifyEmailAddress
import Network.AWS.SES.V2010_12_01.DeleteVerifiedEmailAddress
import Network.AWS.SES.V2010_12_01.ListVerifiedEmailAddresses
import Network.AWS.SES.V2010_12_01.SetIdentityNotificationTopic
import Network.AWS.SES.V2010_12_01.SendEmail

-- Newtypes
makeIso ''RawMessage

-- Products
makeLenses ''Body
makeLenses ''Content
makeLenses ''Destination
makeLenses ''IdentityDkimAttributes
makeLenses ''IdentityNotificationAttributes
makeLenses ''IdentityVerificationAttributes
makeLenses ''Message
makeLenses ''SendDataPoint

-- Requests
makeLenses ''GetSendQuota
makeLenses ''GetIdentityNotificationAttributes
makeLenses ''SetIdentityDkimEnabled
makeLenses ''SetIdentityFeedbackForwardingEnabled
makeLenses ''GetIdentityVerificationAttributes
makeLenses ''VerifyDomainIdentity
makeLenses ''GetIdentityDkimAttributes
makeLenses ''VerifyDomainDkim
makeLenses ''SendRawEmail
makeLenses ''GetSendStatistics
makeLenses ''DeleteIdentity
makeLenses ''ListIdentities
makeLenses ''VerifyEmailIdentity
makeLenses ''VerifyEmailAddress
makeLenses ''DeleteVerifiedEmailAddress
makeLenses ''ListVerifiedEmailAddresses
makeLenses ''SetIdentityNotificationTopic
makeLenses ''SendEmail

-- Responses
makeLenses ''GetSendQuotaResponse
makeLenses ''GetIdentityNotificationAttributesResponse
makeLenses ''SetIdentityDkimEnabledResponse
makeLenses ''SetIdentityFeedbackForwardingEnabledResponse
makeLenses ''GetIdentityVerificationAttributesResponse
makeLenses ''VerifyDomainIdentityResponse
makeLenses ''GetIdentityDkimAttributesResponse
makeLenses ''VerifyDomainDkimResponse
makeLenses ''SendRawEmailResponse
makeLenses ''GetSendStatisticsResponse
makeLenses ''DeleteIdentityResponse
makeLenses ''ListIdentitiesResponse
makeLenses ''VerifyEmailIdentityResponse
makeLenses ''VerifyEmailAddressResponse
makeLenses ''DeleteVerifiedEmailAddressResponse
makeLenses ''ListVerifiedEmailAddressesResponse
makeLenses ''SetIdentityNotificationTopicResponse
makeLenses ''SendEmailResponse
