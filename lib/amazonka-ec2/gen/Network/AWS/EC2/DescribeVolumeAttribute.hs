{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVolumeAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified volume. You can specify only one attribute at a time.
--
-- For more information about EBS volumes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS Volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeVolumeAttribute
  ( -- * Creating a request
    DescribeVolumeAttribute (..),
    mkDescribeVolumeAttribute,

    -- ** Request lenses
    dvaAttribute,
    dvaVolumeId,
    dvaDryRun,

    -- * Destructuring the response
    DescribeVolumeAttributeResponse (..),
    mkDescribeVolumeAttributeResponse,

    -- ** Response lenses
    dvarrsAutoEnableIO,
    dvarrsProductCodes,
    dvarrsVolumeId,
    dvarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVolumeAttribute' smart constructor.
data DescribeVolumeAttribute = DescribeVolumeAttribute'
  { -- | The attribute of the volume. This parameter is required.
    attribute :: Types.VolumeAttributeName,
    -- | The ID of the volume.
    volumeId :: Types.VolumeId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumeAttribute' value with any optional fields omitted.
mkDescribeVolumeAttribute ::
  -- | 'attribute'
  Types.VolumeAttributeName ->
  -- | 'volumeId'
  Types.VolumeId ->
  DescribeVolumeAttribute
mkDescribeVolumeAttribute attribute volumeId =
  DescribeVolumeAttribute'
    { attribute,
      volumeId,
      dryRun = Core.Nothing
    }

-- | The attribute of the volume. This parameter is required.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvaAttribute :: Lens.Lens' DescribeVolumeAttribute Types.VolumeAttributeName
dvaAttribute = Lens.field @"attribute"
{-# DEPRECATED dvaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvaVolumeId :: Lens.Lens' DescribeVolumeAttribute Types.VolumeId
dvaVolumeId = Lens.field @"volumeId"
{-# DEPRECATED dvaVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvaDryRun :: Lens.Lens' DescribeVolumeAttribute (Core.Maybe Core.Bool)
dvaDryRun = Lens.field @"dryRun"
{-# DEPRECATED dvaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DescribeVolumeAttribute where
  type Rs DescribeVolumeAttribute = DescribeVolumeAttributeResponse
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
            ( Core.pure ("Action", "DescribeVolumeAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Attribute" attribute)
                Core.<> (Core.toQueryValue "VolumeId" volumeId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVolumeAttributeResponse'
            Core.<$> (x Core..@? "autoEnableIO")
            Core.<*> (x Core..@? "productCodes" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "volumeId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeVolumeAttributeResponse' smart constructor.
data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse'
  { -- | The state of @autoEnableIO@ attribute.
    autoEnableIO :: Core.Maybe Types.AttributeBooleanValue,
    -- | A list of product codes.
    productCodes :: Core.Maybe [Types.ProductCode],
    -- | The ID of the volume.
    volumeId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumeAttributeResponse' value with any optional fields omitted.
mkDescribeVolumeAttributeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeVolumeAttributeResponse
mkDescribeVolumeAttributeResponse responseStatus =
  DescribeVolumeAttributeResponse'
    { autoEnableIO = Core.Nothing,
      productCodes = Core.Nothing,
      volumeId = Core.Nothing,
      responseStatus
    }

-- | The state of @autoEnableIO@ attribute.
--
-- /Note:/ Consider using 'autoEnableIO' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarrsAutoEnableIO :: Lens.Lens' DescribeVolumeAttributeResponse (Core.Maybe Types.AttributeBooleanValue)
dvarrsAutoEnableIO = Lens.field @"autoEnableIO"
{-# DEPRECATED dvarrsAutoEnableIO "Use generic-lens or generic-optics with 'autoEnableIO' instead." #-}

-- | A list of product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarrsProductCodes :: Lens.Lens' DescribeVolumeAttributeResponse (Core.Maybe [Types.ProductCode])
dvarrsProductCodes = Lens.field @"productCodes"
{-# DEPRECATED dvarrsProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarrsVolumeId :: Lens.Lens' DescribeVolumeAttributeResponse (Core.Maybe Types.String)
dvarrsVolumeId = Lens.field @"volumeId"
{-# DEPRECATED dvarrsVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarrsResponseStatus :: Lens.Lens' DescribeVolumeAttributeResponse Core.Int
dvarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
