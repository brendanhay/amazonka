{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import volume task using metadata from the specified disk image.For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/importing-your-volumes-into-amazon-ebs.html Importing Disks to Amazon EBS> .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
module Network.AWS.EC2.ImportVolume
  ( -- * Creating a request
    ImportVolume (..),
    mkImportVolume,

    -- ** Request lenses
    ivAvailabilityZone,
    ivImage,
    ivVolume,
    ivDescription,
    ivDryRun,

    -- * Destructuring the response
    ImportVolumeResponse (..),
    mkImportVolumeResponse,

    -- ** Response lenses
    ivrrsConversionTask,
    ivrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportVolume' smart constructor.
data ImportVolume = ImportVolume'
  { -- | The Availability Zone for the resulting EBS volume.
    availabilityZone :: Types.String,
    -- | The disk image.
    image :: Types.DiskImageDetail,
    -- | The volume size.
    volume :: Types.VolumeDetail,
    -- | A description of the volume.
    description :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportVolume' value with any optional fields omitted.
mkImportVolume ::
  -- | 'availabilityZone'
  Types.String ->
  -- | 'image'
  Types.DiskImageDetail ->
  -- | 'volume'
  Types.VolumeDetail ->
  ImportVolume
mkImportVolume availabilityZone image volume =
  ImportVolume'
    { availabilityZone,
      image,
      volume,
      description = Core.Nothing,
      dryRun = Core.Nothing
    }

-- | The Availability Zone for the resulting EBS volume.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivAvailabilityZone :: Lens.Lens' ImportVolume Types.String
ivAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ivAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The disk image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivImage :: Lens.Lens' ImportVolume Types.DiskImageDetail
ivImage = Lens.field @"image"
{-# DEPRECATED ivImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The volume size.
--
-- /Note:/ Consider using 'volume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivVolume :: Lens.Lens' ImportVolume Types.VolumeDetail
ivVolume = Lens.field @"volume"
{-# DEPRECATED ivVolume "Use generic-lens or generic-optics with 'volume' instead." #-}

-- | A description of the volume.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivDescription :: Lens.Lens' ImportVolume (Core.Maybe Types.String)
ivDescription = Lens.field @"description"
{-# DEPRECATED ivDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivDryRun :: Lens.Lens' ImportVolume (Core.Maybe Core.Bool)
ivDryRun = Lens.field @"dryRun"
{-# DEPRECATED ivDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest ImportVolume where
  type Rs ImportVolume = ImportVolumeResponse
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
            ( Core.pure ("Action", "ImportVolume")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "AvailabilityZone" availabilityZone)
                Core.<> (Core.toQueryValue "Image" image)
                Core.<> (Core.toQueryValue "Volume" volume)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ImportVolumeResponse'
            Core.<$> (x Core..@? "conversionTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkImportVolumeResponse' smart constructor.
data ImportVolumeResponse = ImportVolumeResponse'
  { -- | Information about the conversion task.
    conversionTask :: Core.Maybe Types.ConversionTask,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportVolumeResponse' value with any optional fields omitted.
mkImportVolumeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ImportVolumeResponse
mkImportVolumeResponse responseStatus =
  ImportVolumeResponse'
    { conversionTask = Core.Nothing,
      responseStatus
    }

-- | Information about the conversion task.
--
-- /Note:/ Consider using 'conversionTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivrrsConversionTask :: Lens.Lens' ImportVolumeResponse (Core.Maybe Types.ConversionTask)
ivrrsConversionTask = Lens.field @"conversionTask"
{-# DEPRECATED ivrrsConversionTask "Use generic-lens or generic-optics with 'conversionTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivrrsResponseStatus :: Lens.Lens' ImportVolumeResponse Core.Int
ivrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ivrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
