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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given an identity (an email address or a domain), enables or disables whether Amazon SES forwards bounce and complaint notifications as email. Feedback forwarding can only be disabled when Amazon Simple Notification Service (Amazon SNS) topics are specified for both bounces and complaints.
--
--
-- You can execute this operation no more than once per second.
--
-- For more information about using notifications with Amazon SES, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide> .
--
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
    , siffersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to enable or disable whether Amazon SES forwards you bounce and complaint notifications through email. For information about email feedback forwarding, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-email.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'setIdentityFeedbackForwardingEnabled' smart constructor.
data SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabled'
  { _siffeIdentity          :: !Text
  , _siffeForwardingEnabled :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetIdentityFeedbackForwardingEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siffeIdentity' - The identity for which to set bounce and complaint notification forwarding. Examples: @user@example.com@ , @example.com@ .
--
-- * 'siffeForwardingEnabled' - Sets whether Amazon SES will forward bounce and complaint notifications as email. @true@ specifies that Amazon SES will forward bounce and complaint notifications as email, in addition to any Amazon SNS topic publishing otherwise specified. @false@ specifies that Amazon SES will publish bounce and complaint notifications only through Amazon SNS. This value can only be set to @false@ when Amazon SNS topics are set for both @Bounce@ and @Complaint@ notification types.
setIdentityFeedbackForwardingEnabled
    :: Text -- ^ 'siffeIdentity'
    -> Bool -- ^ 'siffeForwardingEnabled'
    -> SetIdentityFeedbackForwardingEnabled
setIdentityFeedbackForwardingEnabled pIdentity_ pForwardingEnabled_ =
  SetIdentityFeedbackForwardingEnabled'
    {_siffeIdentity = pIdentity_, _siffeForwardingEnabled = pForwardingEnabled_}


-- | The identity for which to set bounce and complaint notification forwarding. Examples: @user@example.com@ , @example.com@ .
siffeIdentity :: Lens' SetIdentityFeedbackForwardingEnabled Text
siffeIdentity = lens _siffeIdentity (\ s a -> s{_siffeIdentity = a})

-- | Sets whether Amazon SES will forward bounce and complaint notifications as email. @true@ specifies that Amazon SES will forward bounce and complaint notifications as email, in addition to any Amazon SNS topic publishing otherwise specified. @false@ specifies that Amazon SES will publish bounce and complaint notifications only through Amazon SNS. This value can only be set to @false@ when Amazon SNS topics are set for both @Bounce@ and @Complaint@ notification types.
siffeForwardingEnabled :: Lens' SetIdentityFeedbackForwardingEnabled Bool
siffeForwardingEnabled = lens _siffeForwardingEnabled (\ s a -> s{_siffeForwardingEnabled = a})

instance AWSRequest
           SetIdentityFeedbackForwardingEnabled
         where
        type Rs SetIdentityFeedbackForwardingEnabled =
             SetIdentityFeedbackForwardingEnabledResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "SetIdentityFeedbackForwardingEnabledResult"
              (\ s h x ->
                 SetIdentityFeedbackForwardingEnabledResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           SetIdentityFeedbackForwardingEnabled
         where

instance NFData SetIdentityFeedbackForwardingEnabled
         where

instance ToHeaders
           SetIdentityFeedbackForwardingEnabled
         where
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

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'setIdentityFeedbackForwardingEnabledResponse' smart constructor.
newtype SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse'
  { _siffersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetIdentityFeedbackForwardingEnabledResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siffersResponseStatus' - -- | The response status code.
setIdentityFeedbackForwardingEnabledResponse
    :: Int -- ^ 'siffersResponseStatus'
    -> SetIdentityFeedbackForwardingEnabledResponse
setIdentityFeedbackForwardingEnabledResponse pResponseStatus_ =
  SetIdentityFeedbackForwardingEnabledResponse'
    {_siffersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
siffersResponseStatus :: Lens' SetIdentityFeedbackForwardingEnabledResponse Int
siffersResponseStatus = lens _siffersResponseStatus (\ s a -> s{_siffersResponseStatus = a})

instance NFData
           SetIdentityFeedbackForwardingEnabledResponse
         where
