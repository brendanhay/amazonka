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
-- Module      : Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given an identity (email address or domain), enables or disables whether
-- Amazon SES forwards bounce and complaint notifications as email.
-- Feedback forwarding can only be disabled when Amazon Simple Notification
-- Service (Amazon SNS) topics are specified for both bounces and
-- complaints.
--
-- Feedback forwarding does not apply to delivery notifications. Delivery
-- notifications are only available through Amazon SNS.
--
-- This action is throttled at one request per second.
--
-- For more information about using notifications with Amazon SES, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide>.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_SetIdentityFeedbackForwardingEnabled.html AWS API Reference> for SetIdentityFeedbackForwardingEnabled.
module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
    (
    -- * Creating a Request
      setIdentityFeedbackForwardingEnabled
    , SetIdentityFeedbackForwardingEnabled
    -- * Request Lenses
    , siffeIdentity
    , siffeForwardingEnabled

    -- * Destructuring the Response
    , setIdentityFeedbackForwardingEnabledResponse
    , SetIdentityFeedbackForwardingEnabledResponse
    -- * Response Lenses
    , siffersStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'setIdentityFeedbackForwardingEnabled' smart constructor.
data SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabled'
    { _siffeIdentity          :: !Text
    , _siffeForwardingEnabled :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetIdentityFeedbackForwardingEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siffeIdentity'
--
-- * 'siffeForwardingEnabled'
setIdentityFeedbackForwardingEnabled
    :: Text -- ^ 'siffeIdentity'
    -> Bool -- ^ 'siffeForwardingEnabled'
    -> SetIdentityFeedbackForwardingEnabled
setIdentityFeedbackForwardingEnabled pIdentity_ pForwardingEnabled_ =
    SetIdentityFeedbackForwardingEnabled'
    { _siffeIdentity = pIdentity_
    , _siffeForwardingEnabled = pForwardingEnabled_
    }

-- | The identity for which to set bounce and complaint notification
-- forwarding. Examples: 'user\'example.com', 'example.com'.
siffeIdentity :: Lens' SetIdentityFeedbackForwardingEnabled Text
siffeIdentity = lens _siffeIdentity (\ s a -> s{_siffeIdentity = a});

-- | Sets whether Amazon SES will forward bounce and complaint notifications
-- as email. 'true' specifies that Amazon SES will forward bounce and
-- complaint notifications as email, in addition to any Amazon SNS topic
-- publishing otherwise specified. 'false' specifies that Amazon SES will
-- publish bounce and complaint notifications only through Amazon SNS. This
-- value can only be set to 'false' when Amazon SNS topics are set for both
-- 'Bounce' and 'Complaint' notification types.
siffeForwardingEnabled :: Lens' SetIdentityFeedbackForwardingEnabled Bool
siffeForwardingEnabled = lens _siffeForwardingEnabled (\ s a -> s{_siffeForwardingEnabled = a});

instance AWSRequest
         SetIdentityFeedbackForwardingEnabled where
        type Rs SetIdentityFeedbackForwardingEnabled =
             SetIdentityFeedbackForwardingEnabledResponse
        request = postQuery sES
        response
          = receiveXMLWrapper
              "SetIdentityFeedbackForwardingEnabledResult"
              (\ s h x ->
                 SetIdentityFeedbackForwardingEnabledResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders
         SetIdentityFeedbackForwardingEnabled where
        toHeaders = const mempty

instance ToPath SetIdentityFeedbackForwardingEnabled
         where
        toPath = const "/"

instance ToQuery SetIdentityFeedbackForwardingEnabled
         where
        toQuery SetIdentityFeedbackForwardingEnabled'{..}
          = mconcat
              ["Action" =:
                 ("SetIdentityFeedbackForwardingEnabled" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identity" =: _siffeIdentity,
               "ForwardingEnabled" =: _siffeForwardingEnabled]

-- | An empty element. Receiving this element indicates that the request
-- completed successfully.
--
-- /See:/ 'setIdentityFeedbackForwardingEnabledResponse' smart constructor.
newtype SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse'
    { _siffersStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetIdentityFeedbackForwardingEnabledResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siffersStatus'
setIdentityFeedbackForwardingEnabledResponse
    :: Int -- ^ 'siffersStatus'
    -> SetIdentityFeedbackForwardingEnabledResponse
setIdentityFeedbackForwardingEnabledResponse pStatus_ =
    SetIdentityFeedbackForwardingEnabledResponse'
    { _siffersStatus = pStatus_
    }

-- | The response status code.
siffersStatus :: Lens' SetIdentityFeedbackForwardingEnabledResponse Int
siffersStatus = lens _siffersStatus (\ s a -> s{_siffersStatus = a});
