{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the discovery of any smart home appliances associated with the room.
module Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
  ( -- * Creating a request
    StartSmartHomeApplianceDiscovery (..),
    mkStartSmartHomeApplianceDiscovery,

    -- ** Request lenses
    sshadRoomARN,

    -- * Destructuring the response
    StartSmartHomeApplianceDiscoveryResponse (..),
    mkStartSmartHomeApplianceDiscoveryResponse,

    -- ** Response lenses
    sshadrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartSmartHomeApplianceDiscovery' smart constructor.
newtype StartSmartHomeApplianceDiscovery = StartSmartHomeApplianceDiscovery'
  { roomARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSmartHomeApplianceDiscovery' with the minimum fields required to make a request.
--
-- * 'roomARN' - The room where smart home appliance discovery was initiated.
mkStartSmartHomeApplianceDiscovery ::
  -- | 'roomARN'
  Lude.Text ->
  StartSmartHomeApplianceDiscovery
mkStartSmartHomeApplianceDiscovery pRoomARN_ =
  StartSmartHomeApplianceDiscovery' {roomARN = pRoomARN_}

-- | The room where smart home appliance discovery was initiated.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshadRoomARN :: Lens.Lens' StartSmartHomeApplianceDiscovery Lude.Text
sshadRoomARN = Lens.lens (roomARN :: StartSmartHomeApplianceDiscovery -> Lude.Text) (\s a -> s {roomARN = a} :: StartSmartHomeApplianceDiscovery)
{-# DEPRECATED sshadRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

instance Lude.AWSRequest StartSmartHomeApplianceDiscovery where
  type
    Rs StartSmartHomeApplianceDiscovery =
      StartSmartHomeApplianceDiscoveryResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartSmartHomeApplianceDiscoveryResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartSmartHomeApplianceDiscovery where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.StartSmartHomeApplianceDiscovery" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartSmartHomeApplianceDiscovery where
  toJSON StartSmartHomeApplianceDiscovery' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RoomArn" Lude..= roomARN)])

instance Lude.ToPath StartSmartHomeApplianceDiscovery where
  toPath = Lude.const "/"

instance Lude.ToQuery StartSmartHomeApplianceDiscovery where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartSmartHomeApplianceDiscoveryResponse' smart constructor.
newtype StartSmartHomeApplianceDiscoveryResponse = StartSmartHomeApplianceDiscoveryResponse'
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

-- | Creates a value of 'StartSmartHomeApplianceDiscoveryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartSmartHomeApplianceDiscoveryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartSmartHomeApplianceDiscoveryResponse
mkStartSmartHomeApplianceDiscoveryResponse pResponseStatus_ =
  StartSmartHomeApplianceDiscoveryResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshadrsResponseStatus :: Lens.Lens' StartSmartHomeApplianceDiscoveryResponse Lude.Int
sshadrsResponseStatus = Lens.lens (responseStatus :: StartSmartHomeApplianceDiscoveryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartSmartHomeApplianceDiscoveryResponse)
{-# DEPRECATED sshadrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
