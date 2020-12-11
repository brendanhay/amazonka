{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListVolumeInitiators
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists iSCSI initiators that are connected to a volume. You can use this operation to determine whether a volume is being used or not. This operation is only supported in the cached volume and stored volume gateway types.
module Network.AWS.StorageGateway.ListVolumeInitiators
  ( -- * Creating a request
    ListVolumeInitiators (..),
    mkListVolumeInitiators,

    -- ** Request lenses
    lviVolumeARN,

    -- * Destructuring the response
    ListVolumeInitiatorsResponse (..),
    mkListVolumeInitiatorsResponse,

    -- ** Response lenses
    lvirsInitiators,
    lvirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | ListVolumeInitiatorsInput
--
-- /See:/ 'mkListVolumeInitiators' smart constructor.
newtype ListVolumeInitiators = ListVolumeInitiators'
  { volumeARN ::
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

-- | Creates a value of 'ListVolumeInitiators' with the minimum fields required to make a request.
--
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes for the gateway.
mkListVolumeInitiators ::
  -- | 'volumeARN'
  Lude.Text ->
  ListVolumeInitiators
mkListVolumeInitiators pVolumeARN_ =
  ListVolumeInitiators' {volumeARN = pVolumeARN_}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes for the gateway.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lviVolumeARN :: Lens.Lens' ListVolumeInitiators Lude.Text
lviVolumeARN = Lens.lens (volumeARN :: ListVolumeInitiators -> Lude.Text) (\s a -> s {volumeARN = a} :: ListVolumeInitiators)
{-# DEPRECATED lviVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

instance Lude.AWSRequest ListVolumeInitiators where
  type Rs ListVolumeInitiators = ListVolumeInitiatorsResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListVolumeInitiatorsResponse'
            Lude.<$> (x Lude..?> "Initiators" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVolumeInitiators where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.ListVolumeInitiators" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListVolumeInitiators where
  toJSON ListVolumeInitiators' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("VolumeARN" Lude..= volumeARN)])

instance Lude.ToPath ListVolumeInitiators where
  toPath = Lude.const "/"

instance Lude.ToQuery ListVolumeInitiators where
  toQuery = Lude.const Lude.mempty

-- | ListVolumeInitiatorsOutput
--
-- /See:/ 'mkListVolumeInitiatorsResponse' smart constructor.
data ListVolumeInitiatorsResponse = ListVolumeInitiatorsResponse'
  { initiators ::
      Lude.Maybe [Lude.Text],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVolumeInitiatorsResponse' with the minimum fields required to make a request.
--
-- * 'initiators' - The host names and port numbers of all iSCSI initiators that are connected to the gateway.
-- * 'responseStatus' - The response status code.
mkListVolumeInitiatorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVolumeInitiatorsResponse
mkListVolumeInitiatorsResponse pResponseStatus_ =
  ListVolumeInitiatorsResponse'
    { initiators = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The host names and port numbers of all iSCSI initiators that are connected to the gateway.
--
-- /Note:/ Consider using 'initiators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvirsInitiators :: Lens.Lens' ListVolumeInitiatorsResponse (Lude.Maybe [Lude.Text])
lvirsInitiators = Lens.lens (initiators :: ListVolumeInitiatorsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {initiators = a} :: ListVolumeInitiatorsResponse)
{-# DEPRECATED lvirsInitiators "Use generic-lens or generic-optics with 'initiators' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvirsResponseStatus :: Lens.Lens' ListVolumeInitiatorsResponse Lude.Int
lvirsResponseStatus = Lens.lens (responseStatus :: ListVolumeInitiatorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVolumeInitiatorsResponse)
{-# DEPRECATED lvirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
