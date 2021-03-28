{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified snapshot. You can specify only one attribute at a time.
--
-- For more information about EBS snapshots, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html Amazon EBS Snapshots> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeSnapshotAttribute
    (
    -- * Creating a request
      DescribeSnapshotAttribute (..)
    , mkDescribeSnapshotAttribute
    -- ** Request lenses
    , dsaAttribute
    , dsaSnapshotId
    , dsaDryRun

    -- * Destructuring the response
    , DescribeSnapshotAttributeResponse (..)
    , mkDescribeSnapshotAttributeResponse
    -- ** Response lenses
    , dsarrsCreateVolumePermissions
    , dsarrsProductCodes
    , dsarrsSnapshotId
    , dsarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSnapshotAttribute' smart constructor.
data DescribeSnapshotAttribute = DescribeSnapshotAttribute'
  { attribute :: Types.SnapshotAttributeName
    -- ^ The snapshot attribute you would like to view.
  , snapshotId :: Types.SnapshotId
    -- ^ The ID of the EBS snapshot.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotAttribute' value with any optional fields omitted.
mkDescribeSnapshotAttribute
    :: Types.SnapshotAttributeName -- ^ 'attribute'
    -> Types.SnapshotId -- ^ 'snapshotId'
    -> DescribeSnapshotAttribute
mkDescribeSnapshotAttribute attribute snapshotId
  = DescribeSnapshotAttribute'{attribute, snapshotId,
                               dryRun = Core.Nothing}

-- | The snapshot attribute you would like to view.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaAttribute :: Lens.Lens' DescribeSnapshotAttribute Types.SnapshotAttributeName
dsaAttribute = Lens.field @"attribute"
{-# INLINEABLE dsaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The ID of the EBS snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaSnapshotId :: Lens.Lens' DescribeSnapshotAttribute Types.SnapshotId
dsaSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE dsaSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaDryRun :: Lens.Lens' DescribeSnapshotAttribute (Core.Maybe Core.Bool)
dsaDryRun = Lens.field @"dryRun"
{-# INLINEABLE dsaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DescribeSnapshotAttribute where
        toQuery DescribeSnapshotAttribute{..}
          = Core.toQueryPair "Action"
              ("DescribeSnapshotAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Attribute" attribute
              Core.<> Core.toQueryPair "SnapshotId" snapshotId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DescribeSnapshotAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSnapshotAttribute where
        type Rs DescribeSnapshotAttribute =
             DescribeSnapshotAttributeResponse
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
                 DescribeSnapshotAttributeResponse' Core.<$>
                   (x Core..@? "createVolumePermission" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*>
                     x Core..@? "productCodes" Core..<@> Core.parseXMLList "item"
                     Core.<*> x Core..@? "snapshotId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeSnapshotAttributeResponse' smart constructor.
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse'
  { createVolumePermissions :: Core.Maybe [Types.CreateVolumePermission]
    -- ^ The users and groups that have the permissions for creating volumes from the snapshot.
  , productCodes :: Core.Maybe [Types.ProductCode]
    -- ^ The product codes.
  , snapshotId :: Core.Maybe Core.Text
    -- ^ The ID of the EBS snapshot.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotAttributeResponse' value with any optional fields omitted.
mkDescribeSnapshotAttributeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSnapshotAttributeResponse
mkDescribeSnapshotAttributeResponse responseStatus
  = DescribeSnapshotAttributeResponse'{createVolumePermissions =
                                         Core.Nothing,
                                       productCodes = Core.Nothing, snapshotId = Core.Nothing,
                                       responseStatus}

-- | The users and groups that have the permissions for creating volumes from the snapshot.
--
-- /Note:/ Consider using 'createVolumePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsCreateVolumePermissions :: Lens.Lens' DescribeSnapshotAttributeResponse (Core.Maybe [Types.CreateVolumePermission])
dsarrsCreateVolumePermissions = Lens.field @"createVolumePermissions"
{-# INLINEABLE dsarrsCreateVolumePermissions #-}
{-# DEPRECATED createVolumePermissions "Use generic-lens or generic-optics with 'createVolumePermissions' instead"  #-}

-- | The product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsProductCodes :: Lens.Lens' DescribeSnapshotAttributeResponse (Core.Maybe [Types.ProductCode])
dsarrsProductCodes = Lens.field @"productCodes"
{-# INLINEABLE dsarrsProductCodes #-}
{-# DEPRECATED productCodes "Use generic-lens or generic-optics with 'productCodes' instead"  #-}

-- | The ID of the EBS snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsSnapshotId :: Lens.Lens' DescribeSnapshotAttributeResponse (Core.Maybe Core.Text)
dsarrsSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE dsarrsSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsResponseStatus :: Lens.Lens' DescribeSnapshotAttributeResponse Core.Int
dsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
