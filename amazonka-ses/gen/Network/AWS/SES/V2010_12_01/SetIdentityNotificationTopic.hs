{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.SetIdentityNotificationTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given an identity (email address or domain), sets the Amazon Simple
-- Notification Service (Amazon SNS) topic to which Amazon SES will publish
-- bounce, complaint, and/or delivery notifications for emails sent with that
-- identity as the Source. Unless feedback forwarding is enabled, you must
-- specify Amazon SNS topics for bounce and complaint notifications. For more
-- information, see SetIdentityFeedbackForwardingEnabled. This action is
-- throttled at one request per second. For more information about feedback
-- notification, see the Amazon SES Developer Guide. POST / HTTP/1.1 Date:
-- Sat, 12 May 2012 05:27:54 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=3+KQ4VHx991T7Kb41HmFcZJxuHz4/6mf2H5FxY+tuLc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 203
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=SetIdentityNotificationTopic
-- &Identity=user@example.com
-- &SnsTopic=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3Aexample
-- &NotificationType=Bounce
-- &Timestamp=2012-05-12T05%3A27%3A54.000Z&Version=2010-12-01
-- 299f4af4-b72a-11e1-901f-1fbd90e8104f.
module Network.AWS.SES.V2010_12_01.SetIdentityNotificationTopic
    (
    -- * Request
      SetIdentityNotificationTopic
    -- ** Request constructor
    , setIdentityNotificationTopic
    -- ** Request lenses
    , sintrIdentity
    , sintrNotificationType
    , sintrSnsTopic

    -- * Response
    , SetIdentityNotificationTopicResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'SetIdentityNotificationTopic' request.
setIdentityNotificationTopic :: Text -- ^ 'sintrIdentity'
                             -> NotificationType -- ^ 'sintrNotificationType'
                             -> SetIdentityNotificationTopic
setIdentityNotificationTopic p1 p2 = SetIdentityNotificationTopic
    { _sintrIdentity = p1
    , _sintrNotificationType = p2
    , _sintrSnsTopic = Nothing
    }

data SetIdentityNotificationTopic = SetIdentityNotificationTopic
    { _sintrIdentity :: Text
      -- ^ The identity for which the Amazon SNS topic will be set.
      -- Examples: user@example.com, example.com.
    , _sintrNotificationType :: NotificationType
      -- ^ The type of notifications that will be published to the specified
      -- Amazon SNS topic.
    , _sintrSnsTopic :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic. If the
      -- parameter is omitted from the request or a null value is passed,
      -- SnsTopic is cleared and publishing is disabled.
    } deriving (Show, Generic)

-- | The identity for which the Amazon SNS topic will be set. Examples:
-- user@example.com, example.com.
sintrIdentity
    :: Functor f
    => (Text
    -> f (Text))
    -> SetIdentityNotificationTopic
    -> f SetIdentityNotificationTopic
sintrIdentity f x =
    (\y -> x { _sintrIdentity = y })
       <$> f (_sintrIdentity x)
{-# INLINE sintrIdentity #-}

-- | The type of notifications that will be published to the specified Amazon
-- SNS topic.
sintrNotificationType
    :: Functor f
    => (NotificationType
    -> f (NotificationType))
    -> SetIdentityNotificationTopic
    -> f SetIdentityNotificationTopic
sintrNotificationType f x =
    (\y -> x { _sintrNotificationType = y })
       <$> f (_sintrNotificationType x)
{-# INLINE sintrNotificationType #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter is
-- omitted from the request or a null value is passed, SnsTopic is cleared and
-- publishing is disabled.
sintrSnsTopic
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SetIdentityNotificationTopic
    -> f SetIdentityNotificationTopic
sintrSnsTopic f x =
    (\y -> x { _sintrSnsTopic = y })
       <$> f (_sintrSnsTopic x)
{-# INLINE sintrSnsTopic #-}

instance ToQuery SetIdentityNotificationTopic where
    toQuery = genericQuery def

data SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetIdentityNotificationTopic where
    type Sv SetIdentityNotificationTopic = SES
    type Rs SetIdentityNotificationTopic = SetIdentityNotificationTopicResponse

    request = post "SetIdentityNotificationTopic"
    response _ = nullaryResponse SetIdentityNotificationTopicResponse
