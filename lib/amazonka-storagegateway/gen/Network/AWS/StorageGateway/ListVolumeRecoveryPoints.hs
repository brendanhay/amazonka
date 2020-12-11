{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListVolumeRecoveryPoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the recovery points for a specified gateway. This operation is only supported in the cached volume gateway type.
--
-- Each cache volume has one recovery point. A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot or clone a new cached volume from a source volume. To create a snapshot from a volume recovery point use the 'CreateSnapshotFromVolumeRecoveryPoint' operation.
module Network.AWS.StorageGateway.ListVolumeRecoveryPoints
  ( -- * Creating a request
    ListVolumeRecoveryPoints (..),
    mkListVolumeRecoveryPoints,

    -- ** Request lenses
    lvrpGatewayARN,

    -- * Destructuring the response
    ListVolumeRecoveryPointsResponse (..),
    mkListVolumeRecoveryPointsResponse,

    -- ** Response lenses
    lvrprsVolumeRecoveryPointInfos,
    lvrprsGatewayARN,
    lvrprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkListVolumeRecoveryPoints' smart constructor.
newtype ListVolumeRecoveryPoints = ListVolumeRecoveryPoints'
  { gatewayARN ::
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

-- | Creates a value of 'ListVolumeRecoveryPoints' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
mkListVolumeRecoveryPoints ::
  -- | 'gatewayARN'
  Lude.Text ->
  ListVolumeRecoveryPoints
mkListVolumeRecoveryPoints pGatewayARN_ =
  ListVolumeRecoveryPoints' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrpGatewayARN :: Lens.Lens' ListVolumeRecoveryPoints Lude.Text
lvrpGatewayARN = Lens.lens (gatewayARN :: ListVolumeRecoveryPoints -> Lude.Text) (\s a -> s {gatewayARN = a} :: ListVolumeRecoveryPoints)
{-# DEPRECATED lvrpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest ListVolumeRecoveryPoints where
  type Rs ListVolumeRecoveryPoints = ListVolumeRecoveryPointsResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListVolumeRecoveryPointsResponse'
            Lude.<$> (x Lude..?> "VolumeRecoveryPointInfos" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "GatewayARN")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVolumeRecoveryPoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.ListVolumeRecoveryPoints" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListVolumeRecoveryPoints where
  toJSON ListVolumeRecoveryPoints' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath ListVolumeRecoveryPoints where
  toPath = Lude.const "/"

instance Lude.ToQuery ListVolumeRecoveryPoints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListVolumeRecoveryPointsResponse' smart constructor.
data ListVolumeRecoveryPointsResponse = ListVolumeRecoveryPointsResponse'
  { volumeRecoveryPointInfos ::
      Lude.Maybe
        [VolumeRecoveryPointInfo],
    gatewayARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVolumeRecoveryPointsResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'volumeRecoveryPointInfos' - An array of 'VolumeRecoveryPointInfo' objects.
mkListVolumeRecoveryPointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVolumeRecoveryPointsResponse
mkListVolumeRecoveryPointsResponse pResponseStatus_ =
  ListVolumeRecoveryPointsResponse'
    { volumeRecoveryPointInfos =
        Lude.Nothing,
      gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'VolumeRecoveryPointInfo' objects.
--
-- /Note:/ Consider using 'volumeRecoveryPointInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrprsVolumeRecoveryPointInfos :: Lens.Lens' ListVolumeRecoveryPointsResponse (Lude.Maybe [VolumeRecoveryPointInfo])
lvrprsVolumeRecoveryPointInfos = Lens.lens (volumeRecoveryPointInfos :: ListVolumeRecoveryPointsResponse -> Lude.Maybe [VolumeRecoveryPointInfo]) (\s a -> s {volumeRecoveryPointInfos = a} :: ListVolumeRecoveryPointsResponse)
{-# DEPRECATED lvrprsVolumeRecoveryPointInfos "Use generic-lens or generic-optics with 'volumeRecoveryPointInfos' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrprsGatewayARN :: Lens.Lens' ListVolumeRecoveryPointsResponse (Lude.Maybe Lude.Text)
lvrprsGatewayARN = Lens.lens (gatewayARN :: ListVolumeRecoveryPointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: ListVolumeRecoveryPointsResponse)
{-# DEPRECATED lvrprsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrprsResponseStatus :: Lens.Lens' ListVolumeRecoveryPointsResponse Lude.Int
lvrprsResponseStatus = Lens.lens (responseStatus :: ListVolumeRecoveryPointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVolumeRecoveryPointsResponse)
{-# DEPRECATED lvrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
