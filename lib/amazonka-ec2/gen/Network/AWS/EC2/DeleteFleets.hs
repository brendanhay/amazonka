{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified EC2 Fleet.
--
-- After you delete an EC2 Fleet, it launches no new instances.
-- You must specify whether a deleted EC2 Fleet should also terminate its instances. If you choose to terminate the instances, the EC2 Fleet enters the @deleted_terminating@ state. Otherwise, the EC2 Fleet enters the @deleted_running@ state, and the instances continue to run until they are interrupted or you terminate them manually.
-- For @instant@ fleets, EC2 Fleet must terminate the instances when the fleet is deleted. A deleted @instant@ fleet with running instances is not supported.
-- __Restrictions__
--
--     * You can delete up to 25 @instant@ fleets in a single request. If you exceed this number, no @instant@ fleets are deleted and an error is returned. There is no restriction on the number of fleets of type @maintain@ or @request@ that can be deleted in a single request.
--
--
--     * Up to 1000 instances can be terminated in a single request to delete @instant@ fleets.
--
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/manage-ec2-fleet.html#delete-fleet Deleting an EC2 Fleet> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DeleteFleets
  ( -- * Creating a request
    DeleteFleets (..),
    mkDeleteFleets,

    -- ** Request lenses
    dfDryRun,
    dfFleetIds,
    dfTerminateInstances,

    -- * Destructuring the response
    DeleteFleetsResponse (..),
    mkDeleteFleetsResponse,

    -- ** Response lenses
    dfrsSuccessfulFleetDeletions,
    dfrsUnsuccessfulFleetDeletions,
    dfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFleets' smart constructor.
data DeleteFleets = DeleteFleets'
  { dryRun :: Lude.Maybe Lude.Bool,
    fleetIds :: [Lude.Text],
    terminateInstances :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFleets' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'fleetIds' - The IDs of the EC2 Fleets.
-- * 'terminateInstances' - Indicates whether to terminate the instances when the EC2 Fleet is deleted. The default is to terminate the instances.
--
-- To let the instances continue to run after the EC2 Fleet is deleted, specify @NoTerminateInstances@ . Supported only for fleets of type @maintain@ and @request@ .
-- For @instant@ fleets, you cannot specify @NoTerminateInstances@ . A deleted @instant@ fleet with running instances is not supported.
mkDeleteFleets ::
  -- | 'terminateInstances'
  Lude.Bool ->
  DeleteFleets
mkDeleteFleets pTerminateInstances_ =
  DeleteFleets'
    { dryRun = Lude.Nothing,
      fleetIds = Lude.mempty,
      terminateInstances = pTerminateInstances_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfDryRun :: Lens.Lens' DeleteFleets (Lude.Maybe Lude.Bool)
dfDryRun = Lens.lens (dryRun :: DeleteFleets -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteFleets)
{-# DEPRECATED dfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IDs of the EC2 Fleets.
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFleetIds :: Lens.Lens' DeleteFleets [Lude.Text]
dfFleetIds = Lens.lens (fleetIds :: DeleteFleets -> [Lude.Text]) (\s a -> s {fleetIds = a} :: DeleteFleets)
{-# DEPRECATED dfFleetIds "Use generic-lens or generic-optics with 'fleetIds' instead." #-}

-- | Indicates whether to terminate the instances when the EC2 Fleet is deleted. The default is to terminate the instances.
--
-- To let the instances continue to run after the EC2 Fleet is deleted, specify @NoTerminateInstances@ . Supported only for fleets of type @maintain@ and @request@ .
-- For @instant@ fleets, you cannot specify @NoTerminateInstances@ . A deleted @instant@ fleet with running instances is not supported.
--
-- /Note:/ Consider using 'terminateInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfTerminateInstances :: Lens.Lens' DeleteFleets Lude.Bool
dfTerminateInstances = Lens.lens (terminateInstances :: DeleteFleets -> Lude.Bool) (\s a -> s {terminateInstances = a} :: DeleteFleets)
{-# DEPRECATED dfTerminateInstances "Use generic-lens or generic-optics with 'terminateInstances' instead." #-}

instance Lude.AWSRequest DeleteFleets where
  type Rs DeleteFleets = DeleteFleetsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteFleetsResponse'
            Lude.<$> ( x Lude..@? "successfulFleetDeletionSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "unsuccessfulFleetDeletionSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFleets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteFleets where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteFleets where
  toQuery DeleteFleets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteFleets" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "FleetId" fleetIds,
        "TerminateInstances" Lude.=: terminateInstances
      ]

-- | /See:/ 'mkDeleteFleetsResponse' smart constructor.
data DeleteFleetsResponse = DeleteFleetsResponse'
  { successfulFleetDeletions ::
      Lude.Maybe [DeleteFleetSuccessItem],
    unsuccessfulFleetDeletions ::
      Lude.Maybe [DeleteFleetErrorItem],
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

-- | Creates a value of 'DeleteFleetsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'successfulFleetDeletions' - Information about the EC2 Fleets that are successfully deleted.
-- * 'unsuccessfulFleetDeletions' - Information about the EC2 Fleets that are not successfully deleted.
mkDeleteFleetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFleetsResponse
mkDeleteFleetsResponse pResponseStatus_ =
  DeleteFleetsResponse'
    { successfulFleetDeletions = Lude.Nothing,
      unsuccessfulFleetDeletions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the EC2 Fleets that are successfully deleted.
--
-- /Note:/ Consider using 'successfulFleetDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsSuccessfulFleetDeletions :: Lens.Lens' DeleteFleetsResponse (Lude.Maybe [DeleteFleetSuccessItem])
dfrsSuccessfulFleetDeletions = Lens.lens (successfulFleetDeletions :: DeleteFleetsResponse -> Lude.Maybe [DeleteFleetSuccessItem]) (\s a -> s {successfulFleetDeletions = a} :: DeleteFleetsResponse)
{-# DEPRECATED dfrsSuccessfulFleetDeletions "Use generic-lens or generic-optics with 'successfulFleetDeletions' instead." #-}

-- | Information about the EC2 Fleets that are not successfully deleted.
--
-- /Note:/ Consider using 'unsuccessfulFleetDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsUnsuccessfulFleetDeletions :: Lens.Lens' DeleteFleetsResponse (Lude.Maybe [DeleteFleetErrorItem])
dfrsUnsuccessfulFleetDeletions = Lens.lens (unsuccessfulFleetDeletions :: DeleteFleetsResponse -> Lude.Maybe [DeleteFleetErrorItem]) (\s a -> s {unsuccessfulFleetDeletions = a} :: DeleteFleetsResponse)
{-# DEPRECATED dfrsUnsuccessfulFleetDeletions "Use generic-lens or generic-optics with 'unsuccessfulFleetDeletions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsResponseStatus :: Lens.Lens' DeleteFleetsResponse Lude.Int
dfrsResponseStatus = Lens.lens (responseStatus :: DeleteFleetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFleetsResponse)
{-# DEPRECATED dfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
