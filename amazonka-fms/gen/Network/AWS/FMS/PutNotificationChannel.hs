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
-- Module      : Network.AWS.FMS.PutNotificationChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Designates the IAM role and Amazon Simple Notification Service (SNS) topic that AWS Firewall Manager uses to record SNS logs.
--
--
module Network.AWS.FMS.PutNotificationChannel
    (
    -- * Creating a Request
      putNotificationChannel
    , PutNotificationChannel
    -- * Request Lenses
    , pncSNSTopicARN
    , pncSNSRoleName

    -- * Destructuring the Response
    , putNotificationChannelResponse
    , PutNotificationChannelResponse
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putNotificationChannel' smart constructor.
data PutNotificationChannel = PutNotificationChannel'
  { _pncSNSTopicARN :: !Text
  , _pncSNSRoleName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutNotificationChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pncSNSTopicARN' - The Amazon Resource Name (ARN) of the SNS topic that collects notifications from AWS Firewall Manager.
--
-- * 'pncSNSRoleName' - The Amazon Resource Name (ARN) of the IAM role that allows Amazon SNS to record AWS Firewall Manager activity.
putNotificationChannel
    :: Text -- ^ 'pncSNSTopicARN'
    -> Text -- ^ 'pncSNSRoleName'
    -> PutNotificationChannel
putNotificationChannel pSNSTopicARN_ pSNSRoleName_ =
  PutNotificationChannel'
    {_pncSNSTopicARN = pSNSTopicARN_, _pncSNSRoleName = pSNSRoleName_}


-- | The Amazon Resource Name (ARN) of the SNS topic that collects notifications from AWS Firewall Manager.
pncSNSTopicARN :: Lens' PutNotificationChannel Text
pncSNSTopicARN = lens _pncSNSTopicARN (\ s a -> s{_pncSNSTopicARN = a})

-- | The Amazon Resource Name (ARN) of the IAM role that allows Amazon SNS to record AWS Firewall Manager activity.
pncSNSRoleName :: Lens' PutNotificationChannel Text
pncSNSRoleName = lens _pncSNSRoleName (\ s a -> s{_pncSNSRoleName = a})

instance AWSRequest PutNotificationChannel where
        type Rs PutNotificationChannel =
             PutNotificationChannelResponse
        request = postJSON fms
        response
          = receiveNull PutNotificationChannelResponse'

instance Hashable PutNotificationChannel where

instance NFData PutNotificationChannel where

instance ToHeaders PutNotificationChannel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.PutNotificationChannel" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutNotificationChannel where
        toJSON PutNotificationChannel'{..}
          = object
              (catMaybes
                 [Just ("SnsTopicArn" .= _pncSNSTopicARN),
                  Just ("SnsRoleName" .= _pncSNSRoleName)])

instance ToPath PutNotificationChannel where
        toPath = const "/"

instance ToQuery PutNotificationChannel where
        toQuery = const mempty

-- | /See:/ 'putNotificationChannelResponse' smart constructor.
data PutNotificationChannelResponse =
  PutNotificationChannelResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutNotificationChannelResponse' with the minimum fields required to make a request.
--
putNotificationChannelResponse
    :: PutNotificationChannelResponse
putNotificationChannelResponse = PutNotificationChannelResponse'


instance NFData PutNotificationChannelResponse where
