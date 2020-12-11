{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    UpdateJobShipmentState (..),
    mkUpdateJobShipmentState,

    -- ** Request lenses
    ujssJobId,
    ujssShipmentState,

    -- * Destructuring the response
    UpdateJobShipmentStateResponse (..),
    mkUpdateJobShipmentStateResponse,

    -- ** Response lenses
    ujssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkUpdateJobShipmentState' smart constructor.
data UpdateJobShipmentState = UpdateJobShipmentState'
  { jobId ::
      Lude.Text,
    shipmentState :: ShipmentState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJobShipmentState' with the minimum fields required to make a request.
--
-- * 'jobId' - The job ID of the job whose shipment date you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
-- * 'shipmentState' - The state of a device when it is being shipped.
--
-- Set to @RECEIVED@ when the device arrives at your location.
-- Set to @RETURNED@ when you have returned the device to AWS.
mkUpdateJobShipmentState ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'shipmentState'
  ShipmentState ->
  UpdateJobShipmentState
mkUpdateJobShipmentState pJobId_ pShipmentState_ =
  UpdateJobShipmentState'
    { jobId = pJobId_,
      shipmentState = pShipmentState_
    }

-- | The job ID of the job whose shipment date you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujssJobId :: Lens.Lens' UpdateJobShipmentState Lude.Text
ujssJobId = Lens.lens (jobId :: UpdateJobShipmentState -> Lude.Text) (\s a -> s {jobId = a} :: UpdateJobShipmentState)
{-# DEPRECATED ujssJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The state of a device when it is being shipped.
--
-- Set to @RECEIVED@ when the device arrives at your location.
-- Set to @RETURNED@ when you have returned the device to AWS.
--
-- /Note:/ Consider using 'shipmentState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujssShipmentState :: Lens.Lens' UpdateJobShipmentState ShipmentState
ujssShipmentState = Lens.lens (shipmentState :: UpdateJobShipmentState -> ShipmentState) (\s a -> s {shipmentState = a} :: UpdateJobShipmentState)
{-# DEPRECATED ujssShipmentState "Use generic-lens or generic-optics with 'shipmentState' instead." #-}

instance Lude.AWSRequest UpdateJobShipmentState where
  type Rs UpdateJobShipmentState = UpdateJobShipmentStateResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateJobShipmentStateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateJobShipmentState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.UpdateJobShipmentState" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateJobShipmentState where
  toJSON UpdateJobShipmentState' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobId" Lude..= jobId),
            Lude.Just ("ShipmentState" Lude..= shipmentState)
          ]
      )

instance Lude.ToPath UpdateJobShipmentState where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateJobShipmentState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateJobShipmentStateResponse' smart constructor.
newtype UpdateJobShipmentStateResponse = UpdateJobShipmentStateResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJobShipmentStateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateJobShipmentStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateJobShipmentStateResponse
mkUpdateJobShipmentStateResponse pResponseStatus_ =
  UpdateJobShipmentStateResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujssrsResponseStatus :: Lens.Lens' UpdateJobShipmentStateResponse Lude.Int
ujssrsResponseStatus = Lens.lens (responseStatus :: UpdateJobShipmentStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateJobShipmentStateResponse)
{-# DEPRECATED ujssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
