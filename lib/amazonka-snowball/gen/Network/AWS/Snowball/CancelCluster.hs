{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CancelCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a cluster job. You can only cancel a cluster job while it's in the @AwaitingQuorum@ status. You'll have at least an hour after creating a cluster job to cancel it.
module Network.AWS.Snowball.CancelCluster
  ( -- * Creating a request
    CancelCluster (..),
    mkCancelCluster,

    -- ** Request lenses
    ccClusterId,

    -- * Destructuring the response
    CancelClusterResponse (..),
    mkCancelClusterResponse,

    -- ** Response lenses
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkCancelCluster' smart constructor.
newtype CancelCluster = CancelCluster' {clusterId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelCluster' with the minimum fields required to make a request.
--
-- * 'clusterId' - The 39-character ID for the cluster that you want to cancel, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
mkCancelCluster ::
  -- | 'clusterId'
  Lude.Text ->
  CancelCluster
mkCancelCluster pClusterId_ =
  CancelCluster' {clusterId = pClusterId_}

-- | The 39-character ID for the cluster that you want to cancel, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterId :: Lens.Lens' CancelCluster Lude.Text
ccClusterId = Lens.lens (clusterId :: CancelCluster -> Lude.Text) (\s a -> s {clusterId = a} :: CancelCluster)
{-# DEPRECATED ccClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest CancelCluster where
  type Rs CancelCluster = CancelClusterResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelClusterResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.CancelCluster" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelCluster where
  toJSON CancelCluster' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ClusterId" Lude..= clusterId)])

instance Lude.ToPath CancelCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelClusterResponse' smart constructor.
newtype CancelClusterResponse = CancelClusterResponse'
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

-- | Creates a value of 'CancelClusterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCancelClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelClusterResponse
mkCancelClusterResponse pResponseStatus_ =
  CancelClusterResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CancelClusterResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CancelClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelClusterResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
