{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetIdentityNotificationTopic
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given an identity (email address or domain), sets the Amazon Simple
-- Notification Service (Amazon SNS) topic to which Amazon SES will publish
-- bounce, complaint, and\/or delivery notifications for emails sent with
-- that identity as the 'Source'.
--
-- Unless feedback forwarding is enabled, you must specify Amazon SNS
-- topics for bounce and complaint notifications. For more information, see
-- 'SetIdentityFeedbackForwardingEnabled'.
--
-- This action is throttled at one request per second.
--
-- For more information about feedback notification, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide>.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_SetIdentityNotificationTopic.html AWS API Reference> for SetIdentityNotificationTopic.
module Network.AWS.SES.SetIdentityNotificationTopic
    (
    -- * Creating a Request
      setIdentityNotificationTopic
    , SetIdentityNotificationTopic
    -- * Request Lenses
    , sintSNSTopic
    , sintIdentity
    , sintNotificationType

    -- * Destructuring the Response
    , setIdentityNotificationTopicResponse
    , SetIdentityNotificationTopicResponse
    -- * Response Lenses
    , sintrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | Represents a request to set or clear an identity\'s notification topic.
--
-- /See:/ 'setIdentityNotificationTopic' smart constructor.
data SetIdentityNotificationTopic = SetIdentityNotificationTopic'
    { _sintSNSTopic         :: !(Maybe Text)
    , _sintIdentity         :: !Text
    , _sintNotificationType :: !NotificationType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetIdentityNotificationTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sintSNSTopic'
--
-- * 'sintIdentity'
--
-- * 'sintNotificationType'
setIdentityNotificationTopic
    :: Text -- ^ 'sintIdentity'
    -> NotificationType -- ^ 'sintNotificationType'
    -> SetIdentityNotificationTopic
setIdentityNotificationTopic pIdentity_ pNotificationType_ =
    SetIdentityNotificationTopic'
    { _sintSNSTopic = Nothing
    , _sintIdentity = pIdentity_
    , _sintNotificationType = pNotificationType_
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter
-- is omitted from the request or a null value is passed, 'SnsTopic' is
-- cleared and publishing is disabled.
sintSNSTopic :: Lens' SetIdentityNotificationTopic (Maybe Text)
sintSNSTopic = lens _sintSNSTopic (\ s a -> s{_sintSNSTopic = a});

-- | The identity for which the Amazon SNS topic will be set. You can specify
-- an identity by using its name or by using its Amazon Resource Name
-- (ARN). Examples: 'user\'example.com', 'example.com',
-- 'arn:aws:ses:us-east-1:123456789012:identity\/example.com'.
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
        request = postQuery
        response
          = receiveXMLWrapper
              "SetIdentityNotificationTopicResult"
              (\ s h x ->
                 SetIdentityNotificationTopicResponse' <$>
                   (pure (fromEnum s)))

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

-- | An empty element. Receiving this element indicates that the request
-- completed successfully.
--
-- /See:/ 'setIdentityNotificationTopicResponse' smart constructor.
newtype SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse'
    { _sintrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetIdentityNotificationTopicResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sintrsStatus'
setIdentityNotificationTopicResponse
    :: Int -- ^ 'sintrsStatus'
    -> SetIdentityNotificationTopicResponse
setIdentityNotificationTopicResponse pStatus_ =
    SetIdentityNotificationTopicResponse'
    { _sintrsStatus = pStatus_
    }

-- | The response status code.
sintrsStatus :: Lens' SetIdentityNotificationTopicResponse Int
sintrsStatus = lens _sintrsStatus (\ s a -> s{_sintrsStatus = a});
