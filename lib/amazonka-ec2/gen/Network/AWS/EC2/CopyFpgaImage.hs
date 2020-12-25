{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CopyFpgaImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified Amazon FPGA Image (AFI) to the current Region.
module Network.AWS.EC2.CopyFpgaImage
  ( -- * Creating a request
    CopyFpgaImage (..),
    mkCopyFpgaImage,

    -- ** Request lenses
    cfiSourceFpgaImageId,
    cfiSourceRegion,
    cfiClientToken,
    cfiDescription,
    cfiDryRun,
    cfiName,

    -- * Destructuring the response
    CopyFpgaImageResponse (..),
    mkCopyFpgaImageResponse,

    -- ** Response lenses
    cfirrsFpgaImageId,
    cfirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCopyFpgaImage' smart constructor.
data CopyFpgaImage = CopyFpgaImage'
  { -- | The ID of the source AFI.
    sourceFpgaImageId :: Types.String,
    -- | The Region that contains the source AFI.
    sourceRegion :: Types.String,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | The description for the new AFI.
    description :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The name for the new AFI. The default is the name of the source AFI.
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyFpgaImage' value with any optional fields omitted.
mkCopyFpgaImage ::
  -- | 'sourceFpgaImageId'
  Types.String ->
  -- | 'sourceRegion'
  Types.String ->
  CopyFpgaImage
mkCopyFpgaImage sourceFpgaImageId sourceRegion =
  CopyFpgaImage'
    { sourceFpgaImageId,
      sourceRegion,
      clientToken = Core.Nothing,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      name = Core.Nothing
    }

-- | The ID of the source AFI.
--
-- /Note:/ Consider using 'sourceFpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiSourceFpgaImageId :: Lens.Lens' CopyFpgaImage Types.String
cfiSourceFpgaImageId = Lens.field @"sourceFpgaImageId"
{-# DEPRECATED cfiSourceFpgaImageId "Use generic-lens or generic-optics with 'sourceFpgaImageId' instead." #-}

-- | The Region that contains the source AFI.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiSourceRegion :: Lens.Lens' CopyFpgaImage Types.String
cfiSourceRegion = Lens.field @"sourceRegion"
{-# DEPRECATED cfiSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiClientToken :: Lens.Lens' CopyFpgaImage (Core.Maybe Types.String)
cfiClientToken = Lens.field @"clientToken"
{-# DEPRECATED cfiClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The description for the new AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiDescription :: Lens.Lens' CopyFpgaImage (Core.Maybe Types.String)
cfiDescription = Lens.field @"description"
{-# DEPRECATED cfiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiDryRun :: Lens.Lens' CopyFpgaImage (Core.Maybe Core.Bool)
cfiDryRun = Lens.field @"dryRun"
{-# DEPRECATED cfiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The name for the new AFI. The default is the name of the source AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiName :: Lens.Lens' CopyFpgaImage (Core.Maybe Types.String)
cfiName = Lens.field @"name"
{-# DEPRECATED cfiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.AWSRequest CopyFpgaImage where
  type Rs CopyFpgaImage = CopyFpgaImageResponse
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
            ( Core.pure ("Action", "CopyFpgaImage")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "SourceFpgaImageId" sourceFpgaImageId)
                Core.<> (Core.toQueryValue "SourceRegion" sourceRegion)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Name" Core.<$> name)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CopyFpgaImageResponse'
            Core.<$> (x Core..@? "fpgaImageId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopyFpgaImageResponse' smart constructor.
data CopyFpgaImageResponse = CopyFpgaImageResponse'
  { -- | The ID of the new AFI.
    fpgaImageId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyFpgaImageResponse' value with any optional fields omitted.
mkCopyFpgaImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopyFpgaImageResponse
mkCopyFpgaImageResponse responseStatus =
  CopyFpgaImageResponse'
    { fpgaImageId = Core.Nothing,
      responseStatus
    }

-- | The ID of the new AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfirrsFpgaImageId :: Lens.Lens' CopyFpgaImageResponse (Core.Maybe Types.String)
cfirrsFpgaImageId = Lens.field @"fpgaImageId"
{-# DEPRECATED cfirrsFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfirrsResponseStatus :: Lens.Lens' CopyFpgaImageResponse Core.Int
cfirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
