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
-- Module      : Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given an identity (an email address or a domain), sets whether Amazon SES includes the original email headers in the Amazon Simple Notification Service (Amazon SNS) notifications of a specified type.
--
--
-- You can execute this operation no more than once per second.
--
-- For more information about using notifications with Amazon SES, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide> .
--
module Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled
    (
    -- * Creating a Request
      setIdentityHeadersInNotificationsEnabled
    , SetIdentityHeadersInNotificationsEnabled
    -- * Request Lenses
    , sihineIdentity
    , sihineNotificationType
    , sihineEnabled

    -- * Destructuring the Response
    , setIdentityHeadersInNotificationsEnabledResponse
    , SetIdentityHeadersInNotificationsEnabledResponse
    -- * Response Lenses
    , sihinersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to set whether Amazon SES includes the original email headers in the Amazon SNS notifications of a specified type. For information about notifications, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'setIdentityHeadersInNotificationsEnabled' smart constructor.
data SetIdentityHeadersInNotificationsEnabled = SetIdentityHeadersInNotificationsEnabled'
  { _sihineIdentity         :: !Text
  , _sihineNotificationType :: !NotificationType
  , _sihineEnabled          :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetIdentityHeadersInNotificationsEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sihineIdentity' - The identity for which to enable or disable headers in notifications. Examples: @user@example.com@ , @example.com@ .
--
-- * 'sihineNotificationType' - The notification type for which to enable or disable headers in notifications.
--
-- * 'sihineEnabled' - Sets whether Amazon SES includes the original email headers in Amazon SNS notifications of the specified notification type. A value of @true@ specifies that Amazon SES will include headers in notifications, and a value of @false@ specifies that Amazon SES will not include headers in notifications. This value can only be set when @NotificationType@ is already set to use a particular Amazon SNS topic.
setIdentityHeadersInNotificationsEnabled
    :: Text -- ^ 'sihineIdentity'
    -> NotificationType -- ^ 'sihineNotificationType'
    -> Bool -- ^ 'sihineEnabled'
    -> SetIdentityHeadersInNotificationsEnabled
setIdentityHeadersInNotificationsEnabled pIdentity_ pNotificationType_ pEnabled_ =
  SetIdentityHeadersInNotificationsEnabled'
    { _sihineIdentity = pIdentity_
    , _sihineNotificationType = pNotificationType_
    , _sihineEnabled = pEnabled_
    }


-- | The identity for which to enable or disable headers in notifications. Examples: @user@example.com@ , @example.com@ .
sihineIdentity :: Lens' SetIdentityHeadersInNotificationsEnabled Text
sihineIdentity = lens _sihineIdentity (\ s a -> s{_sihineIdentity = a})

-- | The notification type for which to enable or disable headers in notifications.
sihineNotificationType :: Lens' SetIdentityHeadersInNotificationsEnabled NotificationType
sihineNotificationType = lens _sihineNotificationType (\ s a -> s{_sihineNotificationType = a})

-- | Sets whether Amazon SES includes the original email headers in Amazon SNS notifications of the specified notification type. A value of @true@ specifies that Amazon SES will include headers in notifications, and a value of @false@ specifies that Amazon SES will not include headers in notifications. This value can only be set when @NotificationType@ is already set to use a particular Amazon SNS topic.
sihineEnabled :: Lens' SetIdentityHeadersInNotificationsEnabled Bool
sihineEnabled = lens _sihineEnabled (\ s a -> s{_sihineEnabled = a})

instance AWSRequest
           SetIdentityHeadersInNotificationsEnabled
         where
        type Rs SetIdentityHeadersInNotificationsEnabled =
             SetIdentityHeadersInNotificationsEnabledResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "SetIdentityHeadersInNotificationsEnabledResult"
              (\ s h x ->
                 SetIdentityHeadersInNotificationsEnabledResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           SetIdentityHeadersInNotificationsEnabled
         where

instance NFData
           SetIdentityHeadersInNotificationsEnabled
         where

instance ToHeaders
           SetIdentityHeadersInNotificationsEnabled
         where
        toHeaders = const mempty

instance ToPath
           SetIdentityHeadersInNotificationsEnabled
         where
        toPath = const "/"

instance ToQuery
           SetIdentityHeadersInNotificationsEnabled
         where
        toQuery SetIdentityHeadersInNotificationsEnabled'{..}
          = mconcat
              ["Action" =:
                 ("SetIdentityHeadersInNotificationsEnabled" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identity" =: _sihineIdentity,
               "NotificationType" =: _sihineNotificationType,
               "Enabled" =: _sihineEnabled]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'setIdentityHeadersInNotificationsEnabledResponse' smart constructor.
newtype SetIdentityHeadersInNotificationsEnabledResponse = SetIdentityHeadersInNotificationsEnabledResponse'
  { _sihinersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetIdentityHeadersInNotificationsEnabledResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sihinersResponseStatus' - -- | The response status code.
setIdentityHeadersInNotificationsEnabledResponse
    :: Int -- ^ 'sihinersResponseStatus'
    -> SetIdentityHeadersInNotificationsEnabledResponse
setIdentityHeadersInNotificationsEnabledResponse pResponseStatus_ =
  SetIdentityHeadersInNotificationsEnabledResponse'
    {_sihinersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sihinersResponseStatus :: Lens' SetIdentityHeadersInNotificationsEnabledResponse Int
sihinersResponseStatus = lens _sihinersResponseStatus (\ s a -> s{_sihinersResponseStatus = a})

instance NFData
           SetIdentityHeadersInNotificationsEnabledResponse
         where
