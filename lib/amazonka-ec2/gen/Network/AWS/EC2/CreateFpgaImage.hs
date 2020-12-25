{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateFpgaImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon FPGA Image (AFI) from the specified design checkpoint (DCP).
--
-- The create operation is asynchronous. To verify that the AFI is ready for use, check the output logs.
-- An AFI contains the FPGA bitstream that is ready to download to an FPGA. You can securely deploy an AFI on multiple FPGA-accelerated instances. For more information, see the <https://github.com/aws/aws-fpga/ AWS FPGA Hardware Development Kit> .
module Network.AWS.EC2.CreateFpgaImage
  ( -- * Creating a request
    CreateFpgaImage (..),
    mkCreateFpgaImage,

    -- ** Request lenses
    cInputStorageLocation,
    cClientToken,
    cDescription,
    cDryRun,
    cLogsStorageLocation,
    cName,
    cTagSpecifications,

    -- * Destructuring the response
    CreateFpgaImageResponse (..),
    mkCreateFpgaImageResponse,

    -- ** Response lenses
    crsFpgaImageGlobalId,
    crsFpgaImageId,
    crsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFpgaImage' smart constructor.
data CreateFpgaImage = CreateFpgaImage'
  { -- | The location of the encrypted design checkpoint in Amazon S3. The input must be a tarball.
    inputStorageLocation :: Types.StorageLocation,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Core.Maybe Types.ClientToken,
    -- | A description for the AFI.
    description :: Core.Maybe Types.Description,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The location in Amazon S3 for the output logs.
    logsStorageLocation :: Core.Maybe Types.StorageLocation,
    -- | A name for the AFI.
    name :: Core.Maybe Types.Name,
    -- | The tags to apply to the FPGA image during creation.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFpgaImage' value with any optional fields omitted.
mkCreateFpgaImage ::
  -- | 'inputStorageLocation'
  Types.StorageLocation ->
  CreateFpgaImage
mkCreateFpgaImage inputStorageLocation =
  CreateFpgaImage'
    { inputStorageLocation,
      clientToken = Core.Nothing,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      logsStorageLocation = Core.Nothing,
      name = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The location of the encrypted design checkpoint in Amazon S3. The input must be a tarball.
--
-- /Note:/ Consider using 'inputStorageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInputStorageLocation :: Lens.Lens' CreateFpgaImage Types.StorageLocation
cInputStorageLocation = Lens.field @"inputStorageLocation"
{-# DEPRECATED cInputStorageLocation "Use generic-lens or generic-optics with 'inputStorageLocation' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClientToken :: Lens.Lens' CreateFpgaImage (Core.Maybe Types.ClientToken)
cClientToken = Lens.field @"clientToken"
{-# DEPRECATED cClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | A description for the AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CreateFpgaImage (Core.Maybe Types.Description)
cDescription = Lens.field @"description"
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDryRun :: Lens.Lens' CreateFpgaImage (Core.Maybe Core.Bool)
cDryRun = Lens.field @"dryRun"
{-# DEPRECATED cDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The location in Amazon S3 for the output logs.
--
-- /Note:/ Consider using 'logsStorageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLogsStorageLocation :: Lens.Lens' CreateFpgaImage (Core.Maybe Types.StorageLocation)
cLogsStorageLocation = Lens.field @"logsStorageLocation"
{-# DEPRECATED cLogsStorageLocation "Use generic-lens or generic-optics with 'logsStorageLocation' instead." #-}

-- | A name for the AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CreateFpgaImage (Core.Maybe Types.Name)
cName = Lens.field @"name"
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The tags to apply to the FPGA image during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTagSpecifications :: Lens.Lens' CreateFpgaImage (Core.Maybe [Types.TagSpecification])
cTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED cTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateFpgaImage where
  type Rs CreateFpgaImage = CreateFpgaImageResponse
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
            ( Core.pure ("Action", "CreateFpgaImage")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InputStorageLocation" inputStorageLocation)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> ( Core.toQueryValue "LogsStorageLocation"
                            Core.<$> logsStorageLocation
                        )
                Core.<> (Core.toQueryValue "Name" Core.<$> name)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFpgaImageResponse'
            Core.<$> (x Core..@? "fpgaImageGlobalId")
            Core.<*> (x Core..@? "fpgaImageId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFpgaImageResponse' smart constructor.
data CreateFpgaImageResponse = CreateFpgaImageResponse'
  { -- | The global FPGA image identifier (AGFI ID).
    fpgaImageGlobalId :: Core.Maybe Types.FpgaImageGlobalId,
    -- | The FPGA image identifier (AFI ID).
    fpgaImageId :: Core.Maybe Types.FpgaImageId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFpgaImageResponse' value with any optional fields omitted.
mkCreateFpgaImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateFpgaImageResponse
mkCreateFpgaImageResponse responseStatus =
  CreateFpgaImageResponse'
    { fpgaImageGlobalId = Core.Nothing,
      fpgaImageId = Core.Nothing,
      responseStatus
    }

-- | The global FPGA image identifier (AGFI ID).
--
-- /Note:/ Consider using 'fpgaImageGlobalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsFpgaImageGlobalId :: Lens.Lens' CreateFpgaImageResponse (Core.Maybe Types.FpgaImageGlobalId)
crsFpgaImageGlobalId = Lens.field @"fpgaImageGlobalId"
{-# DEPRECATED crsFpgaImageGlobalId "Use generic-lens or generic-optics with 'fpgaImageGlobalId' instead." #-}

-- | The FPGA image identifier (AFI ID).
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsFpgaImageId :: Lens.Lens' CreateFpgaImageResponse (Core.Maybe Types.FpgaImageId)
crsFpgaImageId = Lens.field @"fpgaImageId"
{-# DEPRECATED crsFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateFpgaImageResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
