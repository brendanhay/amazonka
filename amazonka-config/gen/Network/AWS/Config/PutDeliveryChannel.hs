{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.Config.PutDeliveryChannel
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new delivery channel object to deliver the configuration
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
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_PutDeliveryChannel.html>
module Network.AWS.Config.PutDeliveryChannel
    (
    -- * Request
      PutDeliveryChannel
    -- ** Request constructor
    , putDeliveryChannel
    -- ** Request lenses
    , pdcDeliveryChannel

    -- * Response
    , PutDeliveryChannelResponse
    -- ** Response constructor
    , putDeliveryChannelResponse
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the PutDeliveryChannel action.
--
-- /See:/ 'putDeliveryChannel' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdcDeliveryChannel'
newtype PutDeliveryChannel = PutDeliveryChannel'
    { _pdcDeliveryChannel :: DeliveryChannel
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutDeliveryChannel' smart constructor.
putDeliveryChannel :: DeliveryChannel -> PutDeliveryChannel
putDeliveryChannel pDeliveryChannel =
    PutDeliveryChannel'
    { _pdcDeliveryChannel = pDeliveryChannel
    }

-- | The configuration delivery channel object that delivers the
-- configuration information to an Amazon S3 bucket, and to an Amazon SNS
-- topic.
pdcDeliveryChannel :: Lens' PutDeliveryChannel DeliveryChannel
pdcDeliveryChannel = lens _pdcDeliveryChannel (\ s a -> s{_pdcDeliveryChannel = a});

instance AWSRequest PutDeliveryChannel where
        type Sv PutDeliveryChannel = Config
        type Rs PutDeliveryChannel =
             PutDeliveryChannelResponse
        request = postJSON
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
          = object ["DeliveryChannel" .= _pdcDeliveryChannel]

instance ToPath PutDeliveryChannel where
        toPath = const "/"

instance ToQuery PutDeliveryChannel where
        toQuery = const mempty

-- | /See:/ 'putDeliveryChannelResponse' smart constructor.
data PutDeliveryChannelResponse =
    PutDeliveryChannelResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutDeliveryChannelResponse' smart constructor.
putDeliveryChannelResponse :: PutDeliveryChannelResponse
putDeliveryChannelResponse = PutDeliveryChannelResponse'
