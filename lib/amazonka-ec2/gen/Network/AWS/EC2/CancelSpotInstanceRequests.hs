{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelSpotInstanceRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels one or more Spot Instance requests.
--
-- /Important:/ Canceling a Spot Instance request does not terminate running Spot Instances associated with the request.
module Network.AWS.EC2.CancelSpotInstanceRequests
  ( -- * Creating a request
    CancelSpotInstanceRequests (..),
    mkCancelSpotInstanceRequests,

    -- ** Request lenses
    csirDryRun,
    csirSpotInstanceRequestIds,

    -- * Destructuring the response
    CancelSpotInstanceRequestsResponse (..),
    mkCancelSpotInstanceRequestsResponse,

    -- ** Response lenses
    csirrsCancelledSpotInstanceRequests,
    csirrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CancelSpotInstanceRequests.
--
-- /See:/ 'mkCancelSpotInstanceRequests' smart constructor.
data CancelSpotInstanceRequests = CancelSpotInstanceRequests'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    spotInstanceRequestIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelSpotInstanceRequests' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'spotInstanceRequestIds' - One or more Spot Instance request IDs.
mkCancelSpotInstanceRequests ::
  CancelSpotInstanceRequests
mkCancelSpotInstanceRequests =
  CancelSpotInstanceRequests'
    { dryRun = Lude.Nothing,
      spotInstanceRequestIds = Lude.mempty
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirDryRun :: Lens.Lens' CancelSpotInstanceRequests (Lude.Maybe Lude.Bool)
csirDryRun = Lens.lens (dryRun :: CancelSpotInstanceRequests -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CancelSpotInstanceRequests)
{-# DEPRECATED csirDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more Spot Instance request IDs.
--
-- /Note:/ Consider using 'spotInstanceRequestIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirSpotInstanceRequestIds :: Lens.Lens' CancelSpotInstanceRequests [Lude.Text]
csirSpotInstanceRequestIds = Lens.lens (spotInstanceRequestIds :: CancelSpotInstanceRequests -> [Lude.Text]) (\s a -> s {spotInstanceRequestIds = a} :: CancelSpotInstanceRequests)
{-# DEPRECATED csirSpotInstanceRequestIds "Use generic-lens or generic-optics with 'spotInstanceRequestIds' instead." #-}

instance Lude.AWSRequest CancelSpotInstanceRequests where
  type
    Rs CancelSpotInstanceRequests =
      CancelSpotInstanceRequestsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CancelSpotInstanceRequestsResponse'
            Lude.<$> ( x Lude..@? "spotInstanceRequestSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelSpotInstanceRequests where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelSpotInstanceRequests where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelSpotInstanceRequests where
  toQuery CancelSpotInstanceRequests' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CancelSpotInstanceRequests" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "SpotInstanceRequestId" spotInstanceRequestIds
      ]

-- | Contains the output of CancelSpotInstanceRequests.
--
-- /See:/ 'mkCancelSpotInstanceRequestsResponse' smart constructor.
data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse'
  { cancelledSpotInstanceRequests ::
      Lude.Maybe
        [CancelledSpotInstanceRequest],
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

-- | Creates a value of 'CancelSpotInstanceRequestsResponse' with the minimum fields required to make a request.
--
-- * 'cancelledSpotInstanceRequests' - One or more Spot Instance requests.
-- * 'responseStatus' - The response status code.
mkCancelSpotInstanceRequestsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelSpotInstanceRequestsResponse
mkCancelSpotInstanceRequestsResponse pResponseStatus_ =
  CancelSpotInstanceRequestsResponse'
    { cancelledSpotInstanceRequests =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | One or more Spot Instance requests.
--
-- /Note:/ Consider using 'cancelledSpotInstanceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirrsCancelledSpotInstanceRequests :: Lens.Lens' CancelSpotInstanceRequestsResponse (Lude.Maybe [CancelledSpotInstanceRequest])
csirrsCancelledSpotInstanceRequests = Lens.lens (cancelledSpotInstanceRequests :: CancelSpotInstanceRequestsResponse -> Lude.Maybe [CancelledSpotInstanceRequest]) (\s a -> s {cancelledSpotInstanceRequests = a} :: CancelSpotInstanceRequestsResponse)
{-# DEPRECATED csirrsCancelledSpotInstanceRequests "Use generic-lens or generic-optics with 'cancelledSpotInstanceRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirrsResponseStatus :: Lens.Lens' CancelSpotInstanceRequestsResponse Lude.Int
csirrsResponseStatus = Lens.lens (responseStatus :: CancelSpotInstanceRequestsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelSpotInstanceRequestsResponse)
{-# DEPRECATED csirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
