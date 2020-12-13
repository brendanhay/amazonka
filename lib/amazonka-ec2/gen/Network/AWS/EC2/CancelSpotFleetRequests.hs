{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelSpotFleetRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Spot Fleet requests.
--
-- After you cancel a Spot Fleet request, the Spot Fleet launches no new Spot Instances. You must specify whether the Spot Fleet should also terminate its Spot Instances. If you terminate the instances, the Spot Fleet request enters the @cancelled_terminating@ state. Otherwise, the Spot Fleet request enters the @cancelled_running@ state and the instances continue to run until they are interrupted or you terminate them manually.
module Network.AWS.EC2.CancelSpotFleetRequests
  ( -- * Creating a request
    CancelSpotFleetRequests (..),
    mkCancelSpotFleetRequests,

    -- ** Request lenses
    csfrSpotFleetRequestIds,
    csfrTerminateInstances,
    csfrDryRun,

    -- * Destructuring the response
    CancelSpotFleetRequestsResponse (..),
    mkCancelSpotFleetRequestsResponse,

    -- ** Response lenses
    csfrrsSuccessfulFleetRequests,
    csfrrsUnsuccessfulFleetRequests,
    csfrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CancelSpotFleetRequests.
--
-- /See:/ 'mkCancelSpotFleetRequests' smart constructor.
data CancelSpotFleetRequests = CancelSpotFleetRequests'
  { -- | The IDs of the Spot Fleet requests.
    spotFleetRequestIds :: [Lude.Text],
    -- | Indicates whether to terminate instances for a Spot Fleet request if it is canceled successfully.
    terminateInstances :: Lude.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelSpotFleetRequests' with the minimum fields required to make a request.
--
-- * 'spotFleetRequestIds' - The IDs of the Spot Fleet requests.
-- * 'terminateInstances' - Indicates whether to terminate instances for a Spot Fleet request if it is canceled successfully.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCancelSpotFleetRequests ::
  -- | 'terminateInstances'
  Lude.Bool ->
  CancelSpotFleetRequests
mkCancelSpotFleetRequests pTerminateInstances_ =
  CancelSpotFleetRequests'
    { spotFleetRequestIds = Lude.mempty,
      terminateInstances = pTerminateInstances_,
      dryRun = Lude.Nothing
    }

-- | The IDs of the Spot Fleet requests.
--
-- /Note:/ Consider using 'spotFleetRequestIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrSpotFleetRequestIds :: Lens.Lens' CancelSpotFleetRequests [Lude.Text]
csfrSpotFleetRequestIds = Lens.lens (spotFleetRequestIds :: CancelSpotFleetRequests -> [Lude.Text]) (\s a -> s {spotFleetRequestIds = a} :: CancelSpotFleetRequests)
{-# DEPRECATED csfrSpotFleetRequestIds "Use generic-lens or generic-optics with 'spotFleetRequestIds' instead." #-}

-- | Indicates whether to terminate instances for a Spot Fleet request if it is canceled successfully.
--
-- /Note:/ Consider using 'terminateInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrTerminateInstances :: Lens.Lens' CancelSpotFleetRequests Lude.Bool
csfrTerminateInstances = Lens.lens (terminateInstances :: CancelSpotFleetRequests -> Lude.Bool) (\s a -> s {terminateInstances = a} :: CancelSpotFleetRequests)
{-# DEPRECATED csfrTerminateInstances "Use generic-lens or generic-optics with 'terminateInstances' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrDryRun :: Lens.Lens' CancelSpotFleetRequests (Lude.Maybe Lude.Bool)
csfrDryRun = Lens.lens (dryRun :: CancelSpotFleetRequests -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CancelSpotFleetRequests)
{-# DEPRECATED csfrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CancelSpotFleetRequests where
  type Rs CancelSpotFleetRequests = CancelSpotFleetRequestsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CancelSpotFleetRequestsResponse'
            Lude.<$> ( x Lude..@? "successfulFleetRequestSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "unsuccessfulFleetRequestSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelSpotFleetRequests where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelSpotFleetRequests where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelSpotFleetRequests where
  toQuery CancelSpotFleetRequests' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CancelSpotFleetRequests" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "SpotFleetRequestId" spotFleetRequestIds,
        "TerminateInstances" Lude.=: terminateInstances,
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of CancelSpotFleetRequests.
--
-- /See:/ 'mkCancelSpotFleetRequestsResponse' smart constructor.
data CancelSpotFleetRequestsResponse = CancelSpotFleetRequestsResponse'
  { -- | Information about the Spot Fleet requests that are successfully canceled.
    successfulFleetRequests :: Lude.Maybe [CancelSpotFleetRequestsSuccessItem],
    -- | Information about the Spot Fleet requests that are not successfully canceled.
    unsuccessfulFleetRequests :: Lude.Maybe [CancelSpotFleetRequestsErrorItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelSpotFleetRequestsResponse' with the minimum fields required to make a request.
--
-- * 'successfulFleetRequests' - Information about the Spot Fleet requests that are successfully canceled.
-- * 'unsuccessfulFleetRequests' - Information about the Spot Fleet requests that are not successfully canceled.
-- * 'responseStatus' - The response status code.
mkCancelSpotFleetRequestsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelSpotFleetRequestsResponse
mkCancelSpotFleetRequestsResponse pResponseStatus_ =
  CancelSpotFleetRequestsResponse'
    { successfulFleetRequests =
        Lude.Nothing,
      unsuccessfulFleetRequests = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Spot Fleet requests that are successfully canceled.
--
-- /Note:/ Consider using 'successfulFleetRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrrsSuccessfulFleetRequests :: Lens.Lens' CancelSpotFleetRequestsResponse (Lude.Maybe [CancelSpotFleetRequestsSuccessItem])
csfrrsSuccessfulFleetRequests = Lens.lens (successfulFleetRequests :: CancelSpotFleetRequestsResponse -> Lude.Maybe [CancelSpotFleetRequestsSuccessItem]) (\s a -> s {successfulFleetRequests = a} :: CancelSpotFleetRequestsResponse)
{-# DEPRECATED csfrrsSuccessfulFleetRequests "Use generic-lens or generic-optics with 'successfulFleetRequests' instead." #-}

-- | Information about the Spot Fleet requests that are not successfully canceled.
--
-- /Note:/ Consider using 'unsuccessfulFleetRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrrsUnsuccessfulFleetRequests :: Lens.Lens' CancelSpotFleetRequestsResponse (Lude.Maybe [CancelSpotFleetRequestsErrorItem])
csfrrsUnsuccessfulFleetRequests = Lens.lens (unsuccessfulFleetRequests :: CancelSpotFleetRequestsResponse -> Lude.Maybe [CancelSpotFleetRequestsErrorItem]) (\s a -> s {unsuccessfulFleetRequests = a} :: CancelSpotFleetRequestsResponse)
{-# DEPRECATED csfrrsUnsuccessfulFleetRequests "Use generic-lens or generic-optics with 'unsuccessfulFleetRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrrsResponseStatus :: Lens.Lens' CancelSpotFleetRequestsResponse Lude.Int
csfrrsResponseStatus = Lens.lens (responseStatus :: CancelSpotFleetRequestsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelSpotFleetRequestsResponse)
{-# DEPRECATED csfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
