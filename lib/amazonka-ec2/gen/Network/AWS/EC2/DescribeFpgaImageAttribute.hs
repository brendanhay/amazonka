{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFpgaImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified Amazon FPGA Image (AFI).
module Network.AWS.EC2.DescribeFpgaImageAttribute
  ( -- * Creating a request
    DescribeFpgaImageAttribute (..),
    mkDescribeFpgaImageAttribute,

    -- ** Request lenses
    dfiaFpgaImageId,
    dfiaAttribute,
    dfiaDryRun,

    -- * Destructuring the response
    DescribeFpgaImageAttributeResponse (..),
    mkDescribeFpgaImageAttributeResponse,

    -- ** Response lenses
    dfiarrsFpgaImageAttribute,
    dfiarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeFpgaImageAttribute' smart constructor.
data DescribeFpgaImageAttribute = DescribeFpgaImageAttribute'
  { -- | The ID of the AFI.
    fpgaImageId :: Types.FpgaImageId,
    -- | The AFI attribute.
    attribute :: Types.FpgaImageAttributeName,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFpgaImageAttribute' value with any optional fields omitted.
mkDescribeFpgaImageAttribute ::
  -- | 'fpgaImageId'
  Types.FpgaImageId ->
  -- | 'attribute'
  Types.FpgaImageAttributeName ->
  DescribeFpgaImageAttribute
mkDescribeFpgaImageAttribute fpgaImageId attribute =
  DescribeFpgaImageAttribute'
    { fpgaImageId,
      attribute,
      dryRun = Core.Nothing
    }

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiaFpgaImageId :: Lens.Lens' DescribeFpgaImageAttribute Types.FpgaImageId
dfiaFpgaImageId = Lens.field @"fpgaImageId"
{-# DEPRECATED dfiaFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | The AFI attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiaAttribute :: Lens.Lens' DescribeFpgaImageAttribute Types.FpgaImageAttributeName
dfiaAttribute = Lens.field @"attribute"
{-# DEPRECATED dfiaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiaDryRun :: Lens.Lens' DescribeFpgaImageAttribute (Core.Maybe Core.Bool)
dfiaDryRun = Lens.field @"dryRun"
{-# DEPRECATED dfiaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DescribeFpgaImageAttribute where
  type
    Rs DescribeFpgaImageAttribute =
      DescribeFpgaImageAttributeResponse
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
            ( Core.pure ("Action", "DescribeFpgaImageAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "FpgaImageId" fpgaImageId)
                Core.<> (Core.toQueryValue "Attribute" attribute)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFpgaImageAttributeResponse'
            Core.<$> (x Core..@? "fpgaImageAttribute")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeFpgaImageAttributeResponse' smart constructor.
data DescribeFpgaImageAttributeResponse = DescribeFpgaImageAttributeResponse'
  { -- | Information about the attribute.
    fpgaImageAttribute :: Core.Maybe Types.FpgaImageAttribute,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFpgaImageAttributeResponse' value with any optional fields omitted.
mkDescribeFpgaImageAttributeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeFpgaImageAttributeResponse
mkDescribeFpgaImageAttributeResponse responseStatus =
  DescribeFpgaImageAttributeResponse'
    { fpgaImageAttribute =
        Core.Nothing,
      responseStatus
    }

-- | Information about the attribute.
--
-- /Note:/ Consider using 'fpgaImageAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiarrsFpgaImageAttribute :: Lens.Lens' DescribeFpgaImageAttributeResponse (Core.Maybe Types.FpgaImageAttribute)
dfiarrsFpgaImageAttribute = Lens.field @"fpgaImageAttribute"
{-# DEPRECATED dfiarrsFpgaImageAttribute "Use generic-lens or generic-optics with 'fpgaImageAttribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiarrsResponseStatus :: Lens.Lens' DescribeFpgaImageAttributeResponse Core.Int
dfiarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfiarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
