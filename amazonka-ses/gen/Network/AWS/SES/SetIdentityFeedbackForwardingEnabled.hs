{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given an identity (email address or domain), enables or disables whether
-- Amazon SES forwards bounce and complaint notifications as email. Feedback
-- forwarding can only be disabled when Amazon Simple Notification Service
-- (Amazon SNS) topics are specified for both bounces and complaints. Feedback
-- forwarding does not apply to delivery notifications. Delivery notifications
-- are only available through Amazon SNS. This action is throttled at one
-- request per second. For more information about using notifications with
-- Amazon SES, see the Amazon SES Developer Guide. POST / HTTP/1.1 Date: Fri,
-- 15 Jun 2012 20:31:21 GMT Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=juNpmD6UJaN+r7gcLa2ZNZpO3AmF1ZfOkD6PgxgNhRA=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 188
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Action=SetIdentityFeedbackForwardingEnabled &ForwardingEnabled=true
-- &Identity=user%40example.com &Timestamp=2012-06-15T20%3A31%3A21.000Z
-- &Version=2010-12-01 299f4af4-b72a-11e1-901f-1fbd90e8104f.
module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
    (
    -- * Request
      SetIdentityFeedbackForwardingEnabled
    -- ** Request constructor
    , mkSetIdentityFeedbackForwardingEnabled
    -- ** Request lenses
    , siffeIdentity
    , siffeForwardingEnabled

    -- * Response
    , SetIdentityFeedbackForwardingEnabledResponse
    -- ** Response constructor
    , mkSetIdentityFeedbackForwardingEnabledResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.Types
import Network.AWS.Prelude

data SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabled
    { _siffeIdentity :: !Text
    , _siffeForwardingEnabled :: !Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetIdentityFeedbackForwardingEnabled' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Identity ::@ @Text@
--
-- * @ForwardingEnabled ::@ @Bool@
--
mkSetIdentityFeedbackForwardingEnabled :: Text -- ^ 'siffeIdentity'
                                       -> Bool -- ^ 'siffeForwardingEnabled'
                                       -> SetIdentityFeedbackForwardingEnabled
mkSetIdentityFeedbackForwardingEnabled p1 p2 = SetIdentityFeedbackForwardingEnabled
    { _siffeIdentity = p1
    , _siffeForwardingEnabled = p2
    }

-- | The identity for which to set bounce and complaint notification forwarding.
-- Examples: user@example.com, example.com.
siffeIdentity :: Lens' SetIdentityFeedbackForwardingEnabled Text
siffeIdentity = lens _siffeIdentity (\s a -> s { _siffeIdentity = a })

-- | Sets whether Amazon SES will forward bounce and complaint notifications as
-- email. true specifies that Amazon SES will forward bounce and complaint
-- notifications as email, in addition to any Amazon SNS topic publishing
-- otherwise specified. false specifies that Amazon SES will publish bounce
-- and complaint notifications only through Amazon SNS. This value can only be
-- set to false when Amazon SNS topics are set for both Bounce and Complaint
-- notification types.
siffeForwardingEnabled :: Lens' SetIdentityFeedbackForwardingEnabled Bool
siffeForwardingEnabled =
    lens _siffeForwardingEnabled (\s a -> s { _siffeForwardingEnabled = a })

instance ToQuery SetIdentityFeedbackForwardingEnabled where
    toQuery = genericQuery def

-- | An empty element. Receiving this element indicates that the request
-- completed successfully.
data SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetIdentityFeedbackForwardingEnabledResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSetIdentityFeedbackForwardingEnabledResponse :: SetIdentityFeedbackForwardingEnabledResponse
mkSetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse

instance AWSRequest SetIdentityFeedbackForwardingEnabled where
    type Sv SetIdentityFeedbackForwardingEnabled = SES
    type Rs SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabledResponse

    request = post "SetIdentityFeedbackForwardingEnabled"
    response _ = nullaryResponse SetIdentityFeedbackForwardingEnabledResponse
