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
-- Module      : Network.AWS.FMS.GetNotificationChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the Amazon Simple Notification Service (SNS) topic that is used to record AWS Firewall Manager SNS logs.
--
--
module Network.AWS.FMS.GetNotificationChannel
    (
    -- * Creating a Request
      getNotificationChannel
    , GetNotificationChannel

    -- * Destructuring the Response
    , getNotificationChannelResponse
    , GetNotificationChannelResponse
    -- * Response Lenses
    , gncrsSNSTopicARN
    , gncrsSNSRoleName
    , gncrsResponseStatus
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getNotificationChannel' smart constructor.
data GetNotificationChannel =
  GetNotificationChannel'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetNotificationChannel' with the minimum fields required to make a request.
--
getNotificationChannel
    :: GetNotificationChannel
getNotificationChannel = GetNotificationChannel'


instance AWSRequest GetNotificationChannel where
        type Rs GetNotificationChannel =
             GetNotificationChannelResponse
        request = postJSON fms
        response
          = receiveJSON
              (\ s h x ->
                 GetNotificationChannelResponse' <$>
                   (x .?> "SnsTopicArn") <*> (x .?> "SnsRoleName") <*>
                     (pure (fromEnum s)))

instance Hashable GetNotificationChannel where

instance NFData GetNotificationChannel where

instance ToHeaders GetNotificationChannel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.GetNotificationChannel" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetNotificationChannel where
        toJSON = const (Object mempty)

instance ToPath GetNotificationChannel where
        toPath = const "/"

instance ToQuery GetNotificationChannel where
        toQuery = const mempty

-- | /See:/ 'getNotificationChannelResponse' smart constructor.
data GetNotificationChannelResponse = GetNotificationChannelResponse'
  { _gncrsSNSTopicARN    :: !(Maybe Text)
  , _gncrsSNSRoleName    :: !(Maybe Text)
  , _gncrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetNotificationChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gncrsSNSTopicARN' - The SNS topic that records AWS Firewall Manager activity.
--
-- * 'gncrsSNSRoleName' - The IAM role that is used by AWS Firewall Manager to record activity to SNS.
--
-- * 'gncrsResponseStatus' - -- | The response status code.
getNotificationChannelResponse
    :: Int -- ^ 'gncrsResponseStatus'
    -> GetNotificationChannelResponse
getNotificationChannelResponse pResponseStatus_ =
  GetNotificationChannelResponse'
    { _gncrsSNSTopicARN = Nothing
    , _gncrsSNSRoleName = Nothing
    , _gncrsResponseStatus = pResponseStatus_
    }


-- | The SNS topic that records AWS Firewall Manager activity.
gncrsSNSTopicARN :: Lens' GetNotificationChannelResponse (Maybe Text)
gncrsSNSTopicARN = lens _gncrsSNSTopicARN (\ s a -> s{_gncrsSNSTopicARN = a})

-- | The IAM role that is used by AWS Firewall Manager to record activity to SNS.
gncrsSNSRoleName :: Lens' GetNotificationChannelResponse (Maybe Text)
gncrsSNSRoleName = lens _gncrsSNSRoleName (\ s a -> s{_gncrsSNSRoleName = a})

-- | -- | The response status code.
gncrsResponseStatus :: Lens' GetNotificationChannelResponse Int
gncrsResponseStatus = lens _gncrsResponseStatus (\ s a -> s{_gncrsResponseStatus = a})

instance NFData GetNotificationChannelResponse where
