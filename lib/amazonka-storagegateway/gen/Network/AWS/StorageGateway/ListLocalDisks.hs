{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListLocalDisks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the gateway's local disks. To specify which gateway to describe, you use the Amazon Resource Name (ARN) of the gateway in the body of the request.
--
-- The request returns a list of all disks, specifying which are configured as working storage, cache storage, or stored volume or not configured at all. The response includes a @DiskStatus@ field. This field can have a value of present (the disk is available to use), missing (the disk is no longer connected to the gateway), or mismatch (the disk node is occupied by a disk that has incorrect metadata or the disk content is corrupted).
module Network.AWS.StorageGateway.ListLocalDisks
  ( -- * Creating a request
    ListLocalDisks (..),
    mkListLocalDisks,

    -- ** Request lenses
    lldGatewayARN,

    -- * Destructuring the response
    ListLocalDisksResponse (..),
    mkListLocalDisksResponse,

    -- ** Response lenses
    lldrsGatewayARN,
    lldrsDisks,
    lldrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'mkListLocalDisks' smart constructor.
newtype ListLocalDisks = ListLocalDisks'
  { gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLocalDisks' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkListLocalDisks ::
  -- | 'gatewayARN'
  Lude.Text ->
  ListLocalDisks
mkListLocalDisks pGatewayARN_ =
  ListLocalDisks' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldGatewayARN :: Lens.Lens' ListLocalDisks Lude.Text
lldGatewayARN = Lens.lens (gatewayARN :: ListLocalDisks -> Lude.Text) (\s a -> s {gatewayARN = a} :: ListLocalDisks)
{-# DEPRECATED lldGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest ListLocalDisks where
  type Rs ListLocalDisks = ListLocalDisksResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLocalDisksResponse'
            Lude.<$> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "Disks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLocalDisks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.ListLocalDisks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListLocalDisks where
  toJSON ListLocalDisks' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath ListLocalDisks where
  toPath = Lude.const "/"

instance Lude.ToQuery ListLocalDisks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListLocalDisksResponse' smart constructor.
data ListLocalDisksResponse = ListLocalDisksResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | A JSON object containing the following fields:
    --
    --
    --     * 'ListLocalDisksOutput$Disks'
    disks :: Lude.Maybe [Disk],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLocalDisksResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'disks' - A JSON object containing the following fields:
--
--
--     * 'ListLocalDisksOutput$Disks'
--
--
-- * 'responseStatus' - The response status code.
mkListLocalDisksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLocalDisksResponse
mkListLocalDisksResponse pResponseStatus_ =
  ListLocalDisksResponse'
    { gatewayARN = Lude.Nothing,
      disks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrsGatewayARN :: Lens.Lens' ListLocalDisksResponse (Lude.Maybe Lude.Text)
lldrsGatewayARN = Lens.lens (gatewayARN :: ListLocalDisksResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: ListLocalDisksResponse)
{-# DEPRECATED lldrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | A JSON object containing the following fields:
--
--
--     * 'ListLocalDisksOutput$Disks'
--
--
--
-- /Note:/ Consider using 'disks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrsDisks :: Lens.Lens' ListLocalDisksResponse (Lude.Maybe [Disk])
lldrsDisks = Lens.lens (disks :: ListLocalDisksResponse -> Lude.Maybe [Disk]) (\s a -> s {disks = a} :: ListLocalDisksResponse)
{-# DEPRECATED lldrsDisks "Use generic-lens or generic-optics with 'disks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrsResponseStatus :: Lens.Lens' ListLocalDisksResponse Lude.Int
lldrsResponseStatus = Lens.lens (responseStatus :: ListLocalDisksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLocalDisksResponse)
{-# DEPRECATED lldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
