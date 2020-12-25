{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeCopyProductStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified copy product operation.
module Network.AWS.ServiceCatalog.DescribeCopyProductStatus
  ( -- * Creating a request
    DescribeCopyProductStatus (..),
    mkDescribeCopyProductStatus,

    -- ** Request lenses
    dcpsCopyProductToken,
    dcpsAcceptLanguage,

    -- * Destructuring the response
    DescribeCopyProductStatusResponse (..),
    mkDescribeCopyProductStatusResponse,

    -- ** Response lenses
    dcpsrrsCopyProductStatus,
    dcpsrrsStatusDetail,
    dcpsrrsTargetProductId,
    dcpsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeCopyProductStatus' smart constructor.
data DescribeCopyProductStatus = DescribeCopyProductStatus'
  { -- | The token for the copy product operation. This token is returned by 'CopyProduct' .
    copyProductToken :: Types.CopyProductToken,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Core.Maybe Types.AcceptLanguage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCopyProductStatus' value with any optional fields omitted.
mkDescribeCopyProductStatus ::
  -- | 'copyProductToken'
  Types.CopyProductToken ->
  DescribeCopyProductStatus
mkDescribeCopyProductStatus copyProductToken =
  DescribeCopyProductStatus'
    { copyProductToken,
      acceptLanguage = Core.Nothing
    }

-- | The token for the copy product operation. This token is returned by 'CopyProduct' .
--
-- /Note:/ Consider using 'copyProductToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsCopyProductToken :: Lens.Lens' DescribeCopyProductStatus Types.CopyProductToken
dcpsCopyProductToken = Lens.field @"copyProductToken"
{-# DEPRECATED dcpsCopyProductToken "Use generic-lens or generic-optics with 'copyProductToken' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsAcceptLanguage :: Lens.Lens' DescribeCopyProductStatus (Core.Maybe Types.AcceptLanguage)
dcpsAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dcpsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON DescribeCopyProductStatus where
  toJSON DescribeCopyProductStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CopyProductToken" Core..= copyProductToken),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest DescribeCopyProductStatus where
  type
    Rs DescribeCopyProductStatus =
      DescribeCopyProductStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DescribeCopyProductStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCopyProductStatusResponse'
            Core.<$> (x Core..:? "CopyProductStatus")
            Core.<*> (x Core..:? "StatusDetail")
            Core.<*> (x Core..:? "TargetProductId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeCopyProductStatusResponse' smart constructor.
data DescribeCopyProductStatusResponse = DescribeCopyProductStatusResponse'
  { -- | The status of the copy product operation.
    copyProductStatus :: Core.Maybe Types.CopyProductStatus,
    -- | The status message.
    statusDetail :: Core.Maybe Types.StatusDetail,
    -- | The identifier of the copied product.
    targetProductId :: Core.Maybe Types.Id,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCopyProductStatusResponse' value with any optional fields omitted.
mkDescribeCopyProductStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCopyProductStatusResponse
mkDescribeCopyProductStatusResponse responseStatus =
  DescribeCopyProductStatusResponse'
    { copyProductStatus =
        Core.Nothing,
      statusDetail = Core.Nothing,
      targetProductId = Core.Nothing,
      responseStatus
    }

-- | The status of the copy product operation.
--
-- /Note:/ Consider using 'copyProductStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsCopyProductStatus :: Lens.Lens' DescribeCopyProductStatusResponse (Core.Maybe Types.CopyProductStatus)
dcpsrrsCopyProductStatus = Lens.field @"copyProductStatus"
{-# DEPRECATED dcpsrrsCopyProductStatus "Use generic-lens or generic-optics with 'copyProductStatus' instead." #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsStatusDetail :: Lens.Lens' DescribeCopyProductStatusResponse (Core.Maybe Types.StatusDetail)
dcpsrrsStatusDetail = Lens.field @"statusDetail"
{-# DEPRECATED dcpsrrsStatusDetail "Use generic-lens or generic-optics with 'statusDetail' instead." #-}

-- | The identifier of the copied product.
--
-- /Note:/ Consider using 'targetProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsTargetProductId :: Lens.Lens' DescribeCopyProductStatusResponse (Core.Maybe Types.Id)
dcpsrrsTargetProductId = Lens.field @"targetProductId"
{-# DEPRECATED dcpsrrsTargetProductId "Use generic-lens or generic-optics with 'targetProductId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsResponseStatus :: Lens.Lens' DescribeCopyProductStatusResponse Core.Int
dcpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
