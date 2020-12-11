{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.DeleteHSM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified HSM. To specify an HSM, you can use its identifier (ID), the IP address of the HSM's elastic network interface (ENI), or the ID of the HSM's ENI. You need to specify only one of these values. To find these values, use 'DescribeClusters' .
module Network.AWS.CloudHSMv2.DeleteHSM
  ( -- * Creating a request
    DeleteHSM (..),
    mkDeleteHSM,

    -- ** Request lenses
    dhEniId,
    dhHSMId,
    dhEniIP,
    dhClusterId,

    -- * Destructuring the response
    DeleteHSMResponse (..),
    mkDeleteHSMResponse,

    -- ** Response lenses
    dhrsHSMId,
    dhrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteHSM' smart constructor.
data DeleteHSM = DeleteHSM'
  { eniId :: Lude.Maybe Lude.Text,
    hsmId :: Lude.Maybe Lude.Text,
    eniIP :: Lude.Maybe Lude.Text,
    clusterId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHSM' with the minimum fields required to make a request.
--
-- * 'clusterId' - The identifier (ID) of the cluster that contains the HSM that you are deleting.
-- * 'eniIP' - The IP address of the elastic network interface (ENI) of the HSM that you are deleting.
-- * 'eniId' - The identifier (ID) of the elastic network interface (ENI) of the HSM that you are deleting.
-- * 'hsmId' - The identifier (ID) of the HSM that you are deleting.
mkDeleteHSM ::
  -- | 'clusterId'
  Lude.Text ->
  DeleteHSM
mkDeleteHSM pClusterId_ =
  DeleteHSM'
    { eniId = Lude.Nothing,
      hsmId = Lude.Nothing,
      eniIP = Lude.Nothing,
      clusterId = pClusterId_
    }

-- | The identifier (ID) of the elastic network interface (ENI) of the HSM that you are deleting.
--
-- /Note:/ Consider using 'eniId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhEniId :: Lens.Lens' DeleteHSM (Lude.Maybe Lude.Text)
dhEniId = Lens.lens (eniId :: DeleteHSM -> Lude.Maybe Lude.Text) (\s a -> s {eniId = a} :: DeleteHSM)
{-# DEPRECATED dhEniId "Use generic-lens or generic-optics with 'eniId' instead." #-}

-- | The identifier (ID) of the HSM that you are deleting.
--
-- /Note:/ Consider using 'hsmId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhHSMId :: Lens.Lens' DeleteHSM (Lude.Maybe Lude.Text)
dhHSMId = Lens.lens (hsmId :: DeleteHSM -> Lude.Maybe Lude.Text) (\s a -> s {hsmId = a} :: DeleteHSM)
{-# DEPRECATED dhHSMId "Use generic-lens or generic-optics with 'hsmId' instead." #-}

-- | The IP address of the elastic network interface (ENI) of the HSM that you are deleting.
--
-- /Note:/ Consider using 'eniIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhEniIP :: Lens.Lens' DeleteHSM (Lude.Maybe Lude.Text)
dhEniIP = Lens.lens (eniIP :: DeleteHSM -> Lude.Maybe Lude.Text) (\s a -> s {eniIP = a} :: DeleteHSM)
{-# DEPRECATED dhEniIP "Use generic-lens or generic-optics with 'eniIP' instead." #-}

-- | The identifier (ID) of the cluster that contains the HSM that you are deleting.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhClusterId :: Lens.Lens' DeleteHSM Lude.Text
dhClusterId = Lens.lens (clusterId :: DeleteHSM -> Lude.Text) (\s a -> s {clusterId = a} :: DeleteHSM)
{-# DEPRECATED dhClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest DeleteHSM where
  type Rs DeleteHSM = DeleteHSMResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteHSMResponse'
            Lude.<$> (x Lude..?> "HsmId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteHSM where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.DeleteHsm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteHSM where
  toJSON DeleteHSM' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EniId" Lude..=) Lude.<$> eniId,
            ("HsmId" Lude..=) Lude.<$> hsmId,
            ("EniIp" Lude..=) Lude.<$> eniIP,
            Lude.Just ("ClusterId" Lude..= clusterId)
          ]
      )

instance Lude.ToPath DeleteHSM where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteHSM where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteHSMResponse' smart constructor.
data DeleteHSMResponse = DeleteHSMResponse'
  { hsmId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeleteHSMResponse' with the minimum fields required to make a request.
--
-- * 'hsmId' - The identifier (ID) of the HSM that was deleted.
-- * 'responseStatus' - The response status code.
mkDeleteHSMResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteHSMResponse
mkDeleteHSMResponse pResponseStatus_ =
  DeleteHSMResponse'
    { hsmId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier (ID) of the HSM that was deleted.
--
-- /Note:/ Consider using 'hsmId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrsHSMId :: Lens.Lens' DeleteHSMResponse (Lude.Maybe Lude.Text)
dhrsHSMId = Lens.lens (hsmId :: DeleteHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {hsmId = a} :: DeleteHSMResponse)
{-# DEPRECATED dhrsHSMId "Use generic-lens or generic-optics with 'hsmId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrsResponseStatus :: Lens.Lens' DeleteHSMResponse Lude.Int
dhrsResponseStatus = Lens.lens (responseStatus :: DeleteHSMResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteHSMResponse)
{-# DEPRECATED dhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
