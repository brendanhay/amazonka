-- Module      : Network.AWS.SES
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Simple Email Service
--
-- This is the API Reference for Amazon Simple Email Service (Amazon SES).
-- This documentation is intended to be used in conjunction with the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html Amazon SES Developer Guide>.
--
-- For a list of Amazon SES endpoints to use in service requests, see
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/regions.html Regions and Amazon SES>
-- in the Amazon SES Developer Guide.
module Network.AWS.SES
    ( module Export
    ) where

import Network.AWS.SES.DeleteIdentity as Export
import Network.AWS.SES.DeleteVerifiedEmailAddress as Export
import Network.AWS.SES.GetIdentityDkimAttributes as Export
import Network.AWS.SES.GetIdentityNotificationAttributes as Export
import Network.AWS.SES.GetIdentityVerificationAttributes as Export
import Network.AWS.SES.GetSendQuota as Export
import Network.AWS.SES.GetSendStatistics as Export
import Network.AWS.SES.ListIdentities as Export
import Network.AWS.SES.ListVerifiedEmailAddresses as Export
import Network.AWS.SES.SendEmail as Export
import Network.AWS.SES.SendRawEmail as Export
import Network.AWS.SES.SetIdentityDkimEnabled as Export
import Network.AWS.SES.SetIdentityFeedbackForwardingEnabled as Export
import Network.AWS.SES.SetIdentityNotificationTopic as Export
import Network.AWS.SES.Types as Export
import Network.AWS.SES.VerifyDomainDkim as Export
import Network.AWS.SES.VerifyDomainIdentity as Export
import Network.AWS.SES.VerifyEmailAddress as Export
import Network.AWS.SES.VerifyEmailIdentity as Export
import Network.AWS.SES.Waiters as Export
