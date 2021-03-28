{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeVolumeAttribute (..)
    , mkDescribeVolumeAttribute
    -- ** Request lenses
    , dvaAttribute
    , dvaVolumeId
    , dvaDryRun

    -- * Destructuring the response
    , DescribeVolumeAttributeResponse (..)
    , mkDescribeVolumeAttributeResponse
    -- ** Response lenses
    , dvarrsAutoEnableIO
    , dvarrsProductCodes
    , dvarrsVolumeId
    , dvarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVolumeAttribute' smart constructor.
data DescribeVolumeAttribute = DescribeVolumeAttribute'
  { attribute :: Types.VolumeAttributeName
    -- ^ The attribute of the volume. This parameter is required.
  , volumeId :: Types.VolumeId
    -- ^ The ID of the volume.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumeAttribute' value with any optional fields omitted.
mkDescribeVolumeAttribute
    :: Types.VolumeAttributeName -- ^ 'attribute'
    -> Types.VolumeId -- ^ 'volumeId'
    -> DescribeVolumeAttribute
mkDescribeVolumeAttribute attribute volumeId
  = DescribeVolumeAttribute'{attribute, volumeId,
                             dryRun = Core.Nothing}

-- | The attribute of the volume. This parameter is required.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvaAttribute :: Lens.Lens' DescribeVolumeAttribute Types.VolumeAttributeName
dvaAttribute = Lens.field @"attribute"
{-# INLINEABLE dvaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvaVolumeId :: Lens.Lens' DescribeVolumeAttribute Types.VolumeId
dvaVolumeId = Lens.field @"volumeId"
{-# INLINEABLE dvaVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvaDryRun :: Lens.Lens' DescribeVolumeAttribute (Core.Maybe Core.Bool)
dvaDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DescribeVolumeAttribute where
        toQuery DescribeVolumeAttribute{..}
          = Core.toQueryPair "Action"
              ("DescribeVolumeAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Attribute" attribute
              Core.<> Core.toQueryPair "VolumeId" volumeId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DescribeVolumeAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVolumeAttribute where
        type Rs DescribeVolumeAttribute = DescribeVolumeAttributeResponse
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
                 DescribeVolumeAttributeResponse' Core.<$>
                   (x Core..@? "autoEnableIO") Core.<*>
                     x Core..@? "productCodes" Core..<@> Core.parseXMLList "item"
                     Core.<*> x Core..@? "volumeId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeVolumeAttributeResponse' smart constructor.
data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse'
  { autoEnableIO :: Core.Maybe Types.AttributeBooleanValue
    -- ^ The state of @autoEnableIO@ attribute.
  , productCodes :: Core.Maybe [Types.ProductCode]
    -- ^ A list of product codes.
  , volumeId :: Core.Maybe Core.Text
    -- ^ The ID of the volume.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumeAttributeResponse' value with any optional fields omitted.
mkDescribeVolumeAttributeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVolumeAttributeResponse
mkDescribeVolumeAttributeResponse responseStatus
  = DescribeVolumeAttributeResponse'{autoEnableIO = Core.Nothing,
                                     productCodes = Core.Nothing, volumeId = Core.Nothing,
                                     responseStatus}

-- | The state of @autoEnableIO@ attribute.
--
-- /Note:/ Consider using 'autoEnableIO' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarrsAutoEnableIO :: Lens.Lens' DescribeVolumeAttributeResponse (Core.Maybe Types.AttributeBooleanValue)
dvarrsAutoEnableIO = Lens.field @"autoEnableIO"
{-# INLINEABLE dvarrsAutoEnableIO #-}
{-# DEPRECATED autoEnableIO "Use generic-lens or generic-optics with 'autoEnableIO' instead"  #-}

-- | A list of product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarrsProductCodes :: Lens.Lens' DescribeVolumeAttributeResponse (Core.Maybe [Types.ProductCode])
dvarrsProductCodes = Lens.field @"productCodes"
{-# INLINEABLE dvarrsProductCodes #-}
{-# DEPRECATED productCodes "Use generic-lens or generic-optics with 'productCodes' instead"  #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarrsVolumeId :: Lens.Lens' DescribeVolumeAttributeResponse (Core.Maybe Core.Text)
dvarrsVolumeId = Lens.field @"volumeId"
{-# INLINEABLE dvarrsVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarrsResponseStatus :: Lens.Lens' DescribeVolumeAttributeResponse Core.Int
dvarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
