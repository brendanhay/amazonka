{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.SetIdentityFeedbackForwardingEnabled
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
module Network.AWS.SES.V2010_12_01.SetIdentityFeedbackForwardingEnabled
    (
    -- * Request
      SetIdentityFeedbackForwardingEnabled
    -- ** Request constructor
    , setIdentityFeedbackForwardingEnabled
    -- ** Request lenses
    , sifferForwardingEnabled
    , sifferIdentity

    -- * Response
    , SetIdentityFeedbackForwardingEnabledResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'SetIdentityFeedbackForwardingEnabled' request.
setIdentityFeedbackForwardingEnabled :: Bool -- ^ 'sifferForwardingEnabled'
                                     -> Text -- ^ 'sifferIdentity'
                                     -> SetIdentityFeedbackForwardingEnabled
setIdentityFeedbackForwardingEnabled p1 p2 = SetIdentityFeedbackForwardingEnabled
    { _sifferForwardingEnabled = p1
    , _sifferIdentity = p2
    }
{-# INLINE setIdentityFeedbackForwardingEnabled #-}

data SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabled
    { _sifferForwardingEnabled :: Bool
      -- ^ Sets whether Amazon SES will forward bounce and complaint
      -- notifications as email. true specifies that Amazon SES will
      -- forward bounce and complaint notifications as email, in addition
      -- to any Amazon SNS topic publishing otherwise specified. false
      -- specifies that Amazon SES will publish bounce and complaint
      -- notifications only through Amazon SNS. This value can only be set
      -- to false when Amazon SNS topics are set for both Bounce and
      -- Complaint notification types.
    , _sifferIdentity :: Text
      -- ^ The identity for which to set bounce and complaint notification
      -- forwarding. Examples: user@example.com, example.com.
    } deriving (Show, Generic)

-- | Sets whether Amazon SES will forward bounce and complaint notifications as
-- email. true specifies that Amazon SES will forward bounce and complaint
-- notifications as email, in addition to any Amazon SNS topic publishing
-- otherwise specified. false specifies that Amazon SES will publish bounce
-- and complaint notifications only through Amazon SNS. This value can only be
-- set to false when Amazon SNS topics are set for both Bounce and Complaint
-- notification types.
sifferForwardingEnabled :: Lens' SetIdentityFeedbackForwardingEnabled (Bool)
sifferForwardingEnabled f x =
    f (_sifferForwardingEnabled x)
        <&> \y -> x { _sifferForwardingEnabled = y }
{-# INLINE sifferForwardingEnabled #-}

-- | The identity for which to set bounce and complaint notification forwarding.
-- Examples: user@example.com, example.com.
sifferIdentity :: Lens' SetIdentityFeedbackForwardingEnabled (Text)
sifferIdentity f x =
    f (_sifferIdentity x)
        <&> \y -> x { _sifferIdentity = y }
{-# INLINE sifferIdentity #-}

instance ToQuery SetIdentityFeedbackForwardingEnabled where
    toQuery = genericQuery def

data SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetIdentityFeedbackForwardingEnabled where
    type Sv SetIdentityFeedbackForwardingEnabled = SES
    type Rs SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabledResponse

    request = post "SetIdentityFeedbackForwardingEnabled"
    response _ = nullaryResponse SetIdentityFeedbackForwardingEnabledResponse
