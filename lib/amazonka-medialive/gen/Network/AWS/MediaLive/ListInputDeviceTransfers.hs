{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListInputDeviceTransfers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List input devices that are currently being transferred. List input devices that you are transferring from your AWS account or input devices that another AWS account is transferring to you.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputDeviceTransfers
  ( -- * Creating a request
    ListInputDeviceTransfers (..),
    mkListInputDeviceTransfers,

    -- ** Request lenses
    lidtTransferType,
    lidtMaxResults,
    lidtNextToken,

    -- * Destructuring the response
    ListInputDeviceTransfersResponse (..),
    mkListInputDeviceTransfersResponse,

    -- ** Response lenses
    lidtrrsInputDeviceTransfers,
    lidtrrsNextToken,
    lidtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListInputDeviceTransfersRequest
--
-- /See:/ 'mkListInputDeviceTransfers' smart constructor.
data ListInputDeviceTransfers = ListInputDeviceTransfers'
  { transferType :: Core.Text,
    maxResults :: Core.Maybe Core.Natural,
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInputDeviceTransfers' value with any optional fields omitted.
mkListInputDeviceTransfers ::
  -- | 'transferType'
  Core.Text ->
  ListInputDeviceTransfers
mkListInputDeviceTransfers transferType =
  ListInputDeviceTransfers'
    { transferType,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'transferType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtTransferType :: Lens.Lens' ListInputDeviceTransfers Core.Text
lidtTransferType = Lens.field @"transferType"
{-# DEPRECATED lidtTransferType "Use generic-lens or generic-optics with 'transferType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtMaxResults :: Lens.Lens' ListInputDeviceTransfers (Core.Maybe Core.Natural)
lidtMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lidtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtNextToken :: Lens.Lens' ListInputDeviceTransfers (Core.Maybe Core.Text)
lidtNextToken = Lens.field @"nextToken"
{-# DEPRECATED lidtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListInputDeviceTransfers where
  type Rs ListInputDeviceTransfers = ListInputDeviceTransfersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/prod/inputDeviceTransfers",
        Core._rqQuery =
          Core.toQueryValue "transferType" transferType
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInputDeviceTransfersResponse'
            Core.<$> (x Core..:? "inputDeviceTransfers")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListInputDeviceTransfers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"inputDeviceTransfers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Placeholder documentation for ListInputDeviceTransfersResponse
--
-- /See:/ 'mkListInputDeviceTransfersResponse' smart constructor.
data ListInputDeviceTransfersResponse = ListInputDeviceTransfersResponse'
  { -- | The list of devices that you are transferring or are being transferred to you.
    inputDeviceTransfers :: Core.Maybe [Types.TransferringInputDeviceSummary],
    -- | A token to get additional list results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInputDeviceTransfersResponse' value with any optional fields omitted.
mkListInputDeviceTransfersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListInputDeviceTransfersResponse
mkListInputDeviceTransfersResponse responseStatus =
  ListInputDeviceTransfersResponse'
    { inputDeviceTransfers =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of devices that you are transferring or are being transferred to you.
--
-- /Note:/ Consider using 'inputDeviceTransfers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtrrsInputDeviceTransfers :: Lens.Lens' ListInputDeviceTransfersResponse (Core.Maybe [Types.TransferringInputDeviceSummary])
lidtrrsInputDeviceTransfers = Lens.field @"inputDeviceTransfers"
{-# DEPRECATED lidtrrsInputDeviceTransfers "Use generic-lens or generic-optics with 'inputDeviceTransfers' instead." #-}

-- | A token to get additional list results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtrrsNextToken :: Lens.Lens' ListInputDeviceTransfersResponse (Core.Maybe Core.Text)
lidtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lidtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidtrrsResponseStatus :: Lens.Lens' ListInputDeviceTransfersResponse Core.Int
lidtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lidtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
