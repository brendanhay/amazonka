{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SES.SetIdentityNotificationTopic
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

-- | Given an identity (email address or domain), sets the Amazon Simple
-- Notification Service (Amazon SNS) topic to which Amazon SES will publish
-- bounce, complaint, and\/or delivery notifications for emails sent with
-- that identity as the @Source@.
--
-- Unless feedback forwarding is enabled, you must specify Amazon SNS
-- topics for bounce and complaint notifications. For more information, see
-- @SetIdentityFeedbackForwardingEnabled@.
--
-- This action is throttled at one request per second.
--
-- For more information about feedback notification, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide>.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_SetIdentityNotificationTopic.html>
module Network.AWS.SES.SetIdentityNotificationTopic
    (
    -- * Request
      SetIdentityNotificationTopic
    -- ** Request constructor
    , setIdentityNotificationTopic
    -- ** Request lenses
    , sintSNSTopic
    , sintIdentity
    , sintNotificationType

    -- * Response
    , SetIdentityNotificationTopicResponse
    -- ** Response constructor
    , setIdentityNotificationTopicResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.SES.Types

-- | /See:/ 'setIdentityNotificationTopic' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sintSNSTopic'
--
-- * 'sintIdentity'
--
-- * 'sintNotificationType'
data SetIdentityNotificationTopic = SetIdentityNotificationTopic'{_sintSNSTopic :: Maybe Text, _sintIdentity :: Text, _sintNotificationType :: NotificationType} deriving (Eq, Read, Show)

-- | 'SetIdentityNotificationTopic' smart constructor.
setIdentityNotificationTopic :: Text -> NotificationType -> SetIdentityNotificationTopic
setIdentityNotificationTopic pIdentity pNotificationType = SetIdentityNotificationTopic'{_sintSNSTopic = Nothing, _sintIdentity = pIdentity, _sintNotificationType = pNotificationType};

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter
-- is omitted from the request or a null value is passed, @SnsTopic@ is
-- cleared and publishing is disabled.
sintSNSTopic :: Lens' SetIdentityNotificationTopic (Maybe Text)
sintSNSTopic = lens _sintSNSTopic (\ s a -> s{_sintSNSTopic = a});

-- | The identity for which the Amazon SNS topic will be set. Examples:
-- @user\@example.com@, @example.com@.
sintIdentity :: Lens' SetIdentityNotificationTopic Text
sintIdentity = lens _sintIdentity (\ s a -> s{_sintIdentity = a});

-- | The type of notifications that will be published to the specified Amazon
-- SNS topic.
sintNotificationType :: Lens' SetIdentityNotificationTopic NotificationType
sintNotificationType = lens _sintNotificationType (\ s a -> s{_sintNotificationType = a});

instance AWSRequest SetIdentityNotificationTopic
         where
        type Sv SetIdentityNotificationTopic = SES
        type Rs SetIdentityNotificationTopic =
             SetIdentityNotificationTopicResponse
        request = post
        response
          = receiveNullWrapper
              "SetIdentityNotificationTopicResult"
              SetIdentityNotificationTopicResponse'

instance ToHeaders SetIdentityNotificationTopic where
        toHeaders = const mempty

instance ToPath SetIdentityNotificationTopic where
        toPath = const "/"

instance ToQuery SetIdentityNotificationTopic where
        toQuery SetIdentityNotificationTopic'{..}
          = mconcat
              ["Action" =:
                 ("SetIdentityNotificationTopic" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "SnsTopic" =: _sintSNSTopic,
               "Identity" =: _sintIdentity,
               "NotificationType" =: _sintNotificationType]

-- | /See:/ 'setIdentityNotificationTopicResponse' smart constructor.
data SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse' deriving (Eq, Read, Show)

-- | 'SetIdentityNotificationTopicResponse' smart constructor.
setIdentityNotificationTopicResponse :: SetIdentityNotificationTopicResponse
setIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse';
