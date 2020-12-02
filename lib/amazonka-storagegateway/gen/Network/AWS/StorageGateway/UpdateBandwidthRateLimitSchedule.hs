{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bandwidth rate limit schedule for a specified gateway. By default, gateways do not have bandwidth rate limit schedules, which means no bandwidth rate limiting is in effect. Use this to initiate or update a gateway's bandwidth rate limit schedule. This operation is supported in the volume and tape gateway types.
module Network.AWS.StorageGateway.UpdateBandwidthRateLimitSchedule
  ( -- * Creating a Request
    updateBandwidthRateLimitSchedule,
    UpdateBandwidthRateLimitSchedule,

    -- * Request Lenses
    ubrlsGatewayARN,
    ubrlsBandwidthRateLimitIntervals,

    -- * Destructuring the Response
    updateBandwidthRateLimitScheduleResponse,
    UpdateBandwidthRateLimitScheduleResponse,

    -- * Response Lenses
    ubrlsrsGatewayARN,
    ubrlsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'updateBandwidthRateLimitSchedule' smart constructor.
data UpdateBandwidthRateLimitSchedule = UpdateBandwidthRateLimitSchedule'
  { _ubrlsGatewayARN ::
      !Text,
    _ubrlsBandwidthRateLimitIntervals ::
      ![BandwidthRateLimitInterval]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateBandwidthRateLimitSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubrlsGatewayARN' - Undocumented member.
--
-- * 'ubrlsBandwidthRateLimitIntervals' - An array containing bandwidth rate limit schedule intervals for a gateway. When no bandwidth rate limit intervals have been scheduled, the array is empty.
updateBandwidthRateLimitSchedule ::
  -- | 'ubrlsGatewayARN'
  Text ->
  UpdateBandwidthRateLimitSchedule
updateBandwidthRateLimitSchedule pGatewayARN_ =
  UpdateBandwidthRateLimitSchedule'
    { _ubrlsGatewayARN =
        pGatewayARN_,
      _ubrlsBandwidthRateLimitIntervals = mempty
    }

-- | Undocumented member.
ubrlsGatewayARN :: Lens' UpdateBandwidthRateLimitSchedule Text
ubrlsGatewayARN = lens _ubrlsGatewayARN (\s a -> s {_ubrlsGatewayARN = a})

-- | An array containing bandwidth rate limit schedule intervals for a gateway. When no bandwidth rate limit intervals have been scheduled, the array is empty.
ubrlsBandwidthRateLimitIntervals :: Lens' UpdateBandwidthRateLimitSchedule [BandwidthRateLimitInterval]
ubrlsBandwidthRateLimitIntervals = lens _ubrlsBandwidthRateLimitIntervals (\s a -> s {_ubrlsBandwidthRateLimitIntervals = a}) . _Coerce

instance AWSRequest UpdateBandwidthRateLimitSchedule where
  type
    Rs UpdateBandwidthRateLimitSchedule =
      UpdateBandwidthRateLimitScheduleResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          UpdateBandwidthRateLimitScheduleResponse'
            <$> (x .?> "GatewayARN") <*> (pure (fromEnum s))
      )

instance Hashable UpdateBandwidthRateLimitSchedule

instance NFData UpdateBandwidthRateLimitSchedule

instance ToHeaders UpdateBandwidthRateLimitSchedule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StorageGateway_20130630.UpdateBandwidthRateLimitSchedule" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateBandwidthRateLimitSchedule where
  toJSON UpdateBandwidthRateLimitSchedule' {..} =
    object
      ( catMaybes
          [ Just ("GatewayARN" .= _ubrlsGatewayARN),
            Just
              ( "BandwidthRateLimitIntervals"
                  .= _ubrlsBandwidthRateLimitIntervals
              )
          ]
      )

instance ToPath UpdateBandwidthRateLimitSchedule where
  toPath = const "/"

instance ToQuery UpdateBandwidthRateLimitSchedule where
  toQuery = const mempty

-- | /See:/ 'updateBandwidthRateLimitScheduleResponse' smart constructor.
data UpdateBandwidthRateLimitScheduleResponse = UpdateBandwidthRateLimitScheduleResponse'
  { _ubrlsrsGatewayARN ::
      !( Maybe
           Text
       ),
    _ubrlsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateBandwidthRateLimitScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubrlsrsGatewayARN' - Undocumented member.
--
-- * 'ubrlsrsResponseStatus' - -- | The response status code.
updateBandwidthRateLimitScheduleResponse ::
  -- | 'ubrlsrsResponseStatus'
  Int ->
  UpdateBandwidthRateLimitScheduleResponse
updateBandwidthRateLimitScheduleResponse pResponseStatus_ =
  UpdateBandwidthRateLimitScheduleResponse'
    { _ubrlsrsGatewayARN =
        Nothing,
      _ubrlsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ubrlsrsGatewayARN :: Lens' UpdateBandwidthRateLimitScheduleResponse (Maybe Text)
ubrlsrsGatewayARN = lens _ubrlsrsGatewayARN (\s a -> s {_ubrlsrsGatewayARN = a})

-- | -- | The response status code.
ubrlsrsResponseStatus :: Lens' UpdateBandwidthRateLimitScheduleResponse Int
ubrlsrsResponseStatus = lens _ubrlsrsResponseStatus (\s a -> s {_ubrlsrsResponseStatus = a})

instance NFData UpdateBandwidthRateLimitScheduleResponse
