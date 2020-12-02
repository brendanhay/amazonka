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
-- Module      : Network.AWS.Snowball.UpdateJobShipmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the state when a the shipment states changes to a different state.
module Network.AWS.Snowball.UpdateJobShipmentState
  ( -- * Creating a Request
    updateJobShipmentState,
    UpdateJobShipmentState,

    -- * Request Lenses
    ujssJobId,
    ujssShipmentState,

    -- * Destructuring the Response
    updateJobShipmentStateResponse,
    UpdateJobShipmentStateResponse,

    -- * Response Lenses
    ujssrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types

-- | /See:/ 'updateJobShipmentState' smart constructor.
data UpdateJobShipmentState = UpdateJobShipmentState'
  { _ujssJobId ::
      !Text,
    _ujssShipmentState :: !ShipmentState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJobShipmentState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujssJobId' - The job ID of the job whose shipment date you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- * 'ujssShipmentState' - The state of a device when it is being shipped.  Set to @RECEIVED@ when the device arrives at your location. Set to @RETURNED@ when you have returned the device to AWS.
updateJobShipmentState ::
  -- | 'ujssJobId'
  Text ->
  -- | 'ujssShipmentState'
  ShipmentState ->
  UpdateJobShipmentState
updateJobShipmentState pJobId_ pShipmentState_ =
  UpdateJobShipmentState'
    { _ujssJobId = pJobId_,
      _ujssShipmentState = pShipmentState_
    }

-- | The job ID of the job whose shipment date you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
ujssJobId :: Lens' UpdateJobShipmentState Text
ujssJobId = lens _ujssJobId (\s a -> s {_ujssJobId = a})

-- | The state of a device when it is being shipped.  Set to @RECEIVED@ when the device arrives at your location. Set to @RETURNED@ when you have returned the device to AWS.
ujssShipmentState :: Lens' UpdateJobShipmentState ShipmentState
ujssShipmentState = lens _ujssShipmentState (\s a -> s {_ujssShipmentState = a})

instance AWSRequest UpdateJobShipmentState where
  type Rs UpdateJobShipmentState = UpdateJobShipmentStateResponse
  request = postJSON snowball
  response =
    receiveEmpty
      ( \s h x ->
          UpdateJobShipmentStateResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateJobShipmentState

instance NFData UpdateJobShipmentState

instance ToHeaders UpdateJobShipmentState where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSIESnowballJobManagementService.UpdateJobShipmentState" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateJobShipmentState where
  toJSON UpdateJobShipmentState' {..} =
    object
      ( catMaybes
          [ Just ("JobId" .= _ujssJobId),
            Just ("ShipmentState" .= _ujssShipmentState)
          ]
      )

instance ToPath UpdateJobShipmentState where
  toPath = const "/"

instance ToQuery UpdateJobShipmentState where
  toQuery = const mempty

-- | /See:/ 'updateJobShipmentStateResponse' smart constructor.
newtype UpdateJobShipmentStateResponse = UpdateJobShipmentStateResponse'
  { _ujssrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJobShipmentStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujssrsResponseStatus' - -- | The response status code.
updateJobShipmentStateResponse ::
  -- | 'ujssrsResponseStatus'
  Int ->
  UpdateJobShipmentStateResponse
updateJobShipmentStateResponse pResponseStatus_ =
  UpdateJobShipmentStateResponse'
    { _ujssrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ujssrsResponseStatus :: Lens' UpdateJobShipmentStateResponse Int
ujssrsResponseStatus = lens _ujssrsResponseStatus (\s a -> s {_ujssrsResponseStatus = a})

instance NFData UpdateJobShipmentStateResponse
