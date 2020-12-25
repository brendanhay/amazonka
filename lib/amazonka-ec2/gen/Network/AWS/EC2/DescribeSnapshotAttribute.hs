{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeSnapshotAttribute (..),
    mkDescribeSnapshotAttribute,

    -- ** Request lenses
    dsaAttribute,
    dsaSnapshotId,
    dsaDryRun,

    -- * Destructuring the response
    DescribeSnapshotAttributeResponse (..),
    mkDescribeSnapshotAttributeResponse,

    -- ** Response lenses
    dsarrsCreateVolumePermissions,
    dsarrsProductCodes,
    dsarrsSnapshotId,
    dsarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSnapshotAttribute' smart constructor.
data DescribeSnapshotAttribute = DescribeSnapshotAttribute'
  { -- | The snapshot attribute you would like to view.
    attribute :: Types.SnapshotAttributeName,
    -- | The ID of the EBS snapshot.
    snapshotId :: Types.SnapshotId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotAttribute' value with any optional fields omitted.
mkDescribeSnapshotAttribute ::
  -- | 'attribute'
  Types.SnapshotAttributeName ->
  -- | 'snapshotId'
  Types.SnapshotId ->
  DescribeSnapshotAttribute
mkDescribeSnapshotAttribute attribute snapshotId =
  DescribeSnapshotAttribute'
    { attribute,
      snapshotId,
      dryRun = Core.Nothing
    }

-- | The snapshot attribute you would like to view.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaAttribute :: Lens.Lens' DescribeSnapshotAttribute Types.SnapshotAttributeName
dsaAttribute = Lens.field @"attribute"
{-# DEPRECATED dsaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the EBS snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaSnapshotId :: Lens.Lens' DescribeSnapshotAttribute Types.SnapshotId
dsaSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED dsaSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaDryRun :: Lens.Lens' DescribeSnapshotAttribute (Core.Maybe Core.Bool)
dsaDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DescribeSnapshotAttribute where
  type
    Rs DescribeSnapshotAttribute =
      DescribeSnapshotAttributeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeSnapshotAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Attribute" attribute)
                Core.<> (Core.toQueryValue "SnapshotId" snapshotId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSnapshotAttributeResponse'
            Core.<$> ( x Core..@? "createVolumePermission"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "productCodes" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "snapshotId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeSnapshotAttributeResponse' smart constructor.
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse'
  { -- | The users and groups that have the permissions for creating volumes from the snapshot.
    createVolumePermissions :: Core.Maybe [Types.CreateVolumePermission],
    -- | The product codes.
    productCodes :: Core.Maybe [Types.ProductCode],
    -- | The ID of the EBS snapshot.
    snapshotId :: Core.Maybe Types.SnapshotId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotAttributeResponse' value with any optional fields omitted.
mkDescribeSnapshotAttributeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSnapshotAttributeResponse
mkDescribeSnapshotAttributeResponse responseStatus =
  DescribeSnapshotAttributeResponse'
    { createVolumePermissions =
        Core.Nothing,
      productCodes = Core.Nothing,
      snapshotId = Core.Nothing,
      responseStatus
    }

-- | The users and groups that have the permissions for creating volumes from the snapshot.
--
-- /Note:/ Consider using 'createVolumePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsCreateVolumePermissions :: Lens.Lens' DescribeSnapshotAttributeResponse (Core.Maybe [Types.CreateVolumePermission])
dsarrsCreateVolumePermissions = Lens.field @"createVolumePermissions"
{-# DEPRECATED dsarrsCreateVolumePermissions "Use generic-lens or generic-optics with 'createVolumePermissions' instead." #-}

-- | The product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsProductCodes :: Lens.Lens' DescribeSnapshotAttributeResponse (Core.Maybe [Types.ProductCode])
dsarrsProductCodes = Lens.field @"productCodes"
{-# DEPRECATED dsarrsProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The ID of the EBS snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsSnapshotId :: Lens.Lens' DescribeSnapshotAttributeResponse (Core.Maybe Types.SnapshotId)
dsarrsSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED dsarrsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsResponseStatus :: Lens.Lens' DescribeSnapshotAttributeResponse Core.Int
dsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
