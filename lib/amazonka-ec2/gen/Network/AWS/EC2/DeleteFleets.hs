{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteFleets (..)
    , mkDeleteFleets
    -- ** Request lenses
    , dfFleetIds
    , dfTerminateInstances
    , dfDryRun

    -- * Destructuring the response
    , DeleteFleetsResponse (..)
    , mkDeleteFleetsResponse
    -- ** Response lenses
    , dfrrsSuccessfulFleetDeletions
    , dfrrsUnsuccessfulFleetDeletions
    , dfrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFleets' smart constructor.
data DeleteFleets = DeleteFleets'
  { fleetIds :: [Types.FleetId]
    -- ^ The IDs of the EC2 Fleets.
  , terminateInstances :: Core.Bool
    -- ^ Indicates whether to terminate the instances when the EC2 Fleet is deleted. The default is to terminate the instances.
--
-- To let the instances continue to run after the EC2 Fleet is deleted, specify @NoTerminateInstances@ . Supported only for fleets of type @maintain@ and @request@ .
-- For @instant@ fleets, you cannot specify @NoTerminateInstances@ . A deleted @instant@ fleet with running instances is not supported.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFleets' value with any optional fields omitted.
mkDeleteFleets
    :: Core.Bool -- ^ 'terminateInstances'
    -> DeleteFleets
mkDeleteFleets terminateInstances
  = DeleteFleets'{fleetIds = Core.mempty, terminateInstances,
                  dryRun = Core.Nothing}

-- | The IDs of the EC2 Fleets.
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFleetIds :: Lens.Lens' DeleteFleets [Types.FleetId]
dfFleetIds = Lens.field @"fleetIds"
{-# INLINEABLE dfFleetIds #-}
{-# DEPRECATED fleetIds "Use generic-lens or generic-optics with 'fleetIds' instead"  #-}

-- | Indicates whether to terminate the instances when the EC2 Fleet is deleted. The default is to terminate the instances.
--
-- To let the instances continue to run after the EC2 Fleet is deleted, specify @NoTerminateInstances@ . Supported only for fleets of type @maintain@ and @request@ .
-- For @instant@ fleets, you cannot specify @NoTerminateInstances@ . A deleted @instant@ fleet with running instances is not supported.
--
-- /Note:/ Consider using 'terminateInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfTerminateInstances :: Lens.Lens' DeleteFleets Core.Bool
dfTerminateInstances = Lens.field @"terminateInstances"
{-# INLINEABLE dfTerminateInstances #-}
{-# DEPRECATED terminateInstances "Use generic-lens or generic-optics with 'terminateInstances' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfDryRun :: Lens.Lens' DeleteFleets (Core.Maybe Core.Bool)
dfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteFleets where
        toQuery DeleteFleets{..}
          = Core.toQueryPair "Action" ("DeleteFleets" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "FleetId" fleetIds
              Core.<> Core.toQueryPair "TerminateInstances" terminateInstances
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteFleets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteFleets where
        type Rs DeleteFleets = DeleteFleetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DeleteFleetsResponse' Core.<$>
                   (x Core..@? "successfulFleetDeletionSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*>
                     x Core..@? "unsuccessfulFleetDeletionSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFleetsResponse' smart constructor.
data DeleteFleetsResponse = DeleteFleetsResponse'
  { successfulFleetDeletions :: Core.Maybe [Types.DeleteFleetSuccessItem]
    -- ^ Information about the EC2 Fleets that are successfully deleted.
  , unsuccessfulFleetDeletions :: Core.Maybe [Types.DeleteFleetErrorItem]
    -- ^ Information about the EC2 Fleets that are not successfully deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFleetsResponse' value with any optional fields omitted.
mkDeleteFleetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteFleetsResponse
mkDeleteFleetsResponse responseStatus
  = DeleteFleetsResponse'{successfulFleetDeletions = Core.Nothing,
                          unsuccessfulFleetDeletions = Core.Nothing, responseStatus}

-- | Information about the EC2 Fleets that are successfully deleted.
--
-- /Note:/ Consider using 'successfulFleetDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsSuccessfulFleetDeletions :: Lens.Lens' DeleteFleetsResponse (Core.Maybe [Types.DeleteFleetSuccessItem])
dfrrsSuccessfulFleetDeletions = Lens.field @"successfulFleetDeletions"
{-# INLINEABLE dfrrsSuccessfulFleetDeletions #-}
{-# DEPRECATED successfulFleetDeletions "Use generic-lens or generic-optics with 'successfulFleetDeletions' instead"  #-}

-- | Information about the EC2 Fleets that are not successfully deleted.
--
-- /Note:/ Consider using 'unsuccessfulFleetDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsUnsuccessfulFleetDeletions :: Lens.Lens' DeleteFleetsResponse (Core.Maybe [Types.DeleteFleetErrorItem])
dfrrsUnsuccessfulFleetDeletions = Lens.field @"unsuccessfulFleetDeletions"
{-# INLINEABLE dfrrsUnsuccessfulFleetDeletions #-}
{-# DEPRECATED unsuccessfulFleetDeletions "Use generic-lens or generic-optics with 'unsuccessfulFleetDeletions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsResponseStatus :: Lens.Lens' DeleteFleetsResponse Core.Int
dfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
