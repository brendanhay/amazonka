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
-- Module      : Network.AWS.Config.PutDeliveryChannel
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new delivery channel object to deliver the configuration
-- information to an Amazon S3 bucket, and to an Amazon SNS topic.
--
-- You can use this action to change the Amazon S3 bucket or an Amazon SNS
-- topic of the existing delivery channel. To change the Amazon S3 bucket
-- or an Amazon SNS topic, call this action and specify the changed values
-- for the S3 bucket and the SNS topic. If you specify a different value
-- for either the S3 bucket or the SNS topic, this action will keep the
-- existing value for the parameter that is not changed.
--
-- Currently, you can specify only one delivery channel per account.
--
-- /See:/ <http://docs.aws.amazon.com/config/latest/APIReference/API_PutDeliveryChannel.html AWS API Reference> for PutDeliveryChannel.
module Network.AWS.Config.PutDeliveryChannel
    (
    -- * Creating a Request
      putDeliveryChannel
    , PutDeliveryChannel
    -- * Request Lenses
    , pdcDeliveryChannel

    -- * Destructuring the Response
    , putDeliveryChannelResponse
    , PutDeliveryChannelResponse
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Config.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the PutDeliveryChannel action.
--
-- /See:/ 'putDeliveryChannel' smart constructor.
newtype PutDeliveryChannel = PutDeliveryChannel'
    { _pdcDeliveryChannel :: DeliveryChannel
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutDeliveryChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdcDeliveryChannel'
putDeliveryChannel
    :: DeliveryChannel -- ^ 'pdcDeliveryChannel'
    -> PutDeliveryChannel
putDeliveryChannel pDeliveryChannel_ =
    PutDeliveryChannel'
    { _pdcDeliveryChannel = pDeliveryChannel_
    }

-- | The configuration delivery channel object that delivers the
-- configuration information to an Amazon S3 bucket, and to an Amazon SNS
-- topic.
pdcDeliveryChannel :: Lens' PutDeliveryChannel DeliveryChannel
pdcDeliveryChannel = lens _pdcDeliveryChannel (\ s a -> s{_pdcDeliveryChannel = a});

instance AWSRequest PutDeliveryChannel where
        type Rs PutDeliveryChannel =
             PutDeliveryChannelResponse
        request = postJSON config
        response = receiveNull PutDeliveryChannelResponse'

instance ToHeaders PutDeliveryChannel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.PutDeliveryChannel" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutDeliveryChannel where
        toJSON PutDeliveryChannel'{..}
          = object
              (catMaybes
                 [Just ("DeliveryChannel" .= _pdcDeliveryChannel)])

instance ToPath PutDeliveryChannel where
        toPath = const "/"

instance ToQuery PutDeliveryChannel where
        toQuery = const mempty

-- | /See:/ 'putDeliveryChannelResponse' smart constructor.
data PutDeliveryChannelResponse =
    PutDeliveryChannelResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutDeliveryChannelResponse' with the minimum fields required to make a request.
--
putDeliveryChannelResponse
    :: PutDeliveryChannelResponse
putDeliveryChannelResponse = PutDeliveryChannelResponse'
