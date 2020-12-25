{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListReceiptFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IP address filters associated with your AWS account in the current AWS Region.
--
-- For information about managing IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.ListReceiptFilters
  ( -- * Creating a request
    ListReceiptFilters (..),
    mkListReceiptFilters,

    -- * Destructuring the response
    ListReceiptFiltersResponse (..),
    mkListReceiptFiltersResponse,

    -- ** Response lenses
    lrfrrsFilters,
    lrfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to list the IP address filters that exist under your AWS account. You use IP address filters when you receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkListReceiptFilters' smart constructor.
data ListReceiptFilters = ListReceiptFilters'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReceiptFilters' value with any optional fields omitted.
mkListReceiptFilters ::
  ListReceiptFilters
mkListReceiptFilters = ListReceiptFilters'

instance Core.AWSRequest ListReceiptFilters where
  type Rs ListReceiptFilters = ListReceiptFiltersResponse
  request x@_ =
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
            ( Core.pure ("Action", "ListReceiptFilters")
                Core.<> (Core.pure ("Version", "2010-12-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListReceiptFiltersResult"
      ( \s h x ->
          ListReceiptFiltersResponse'
            Core.<$> (x Core..@? "Filters" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A list of IP address filters that exist under your AWS account.
--
-- /See:/ 'mkListReceiptFiltersResponse' smart constructor.
data ListReceiptFiltersResponse = ListReceiptFiltersResponse'
  { -- | A list of IP address filter data structures, which each consist of a name, an IP address range, and whether to allow or block mail from it.
    filters :: Core.Maybe [Types.ReceiptFilter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReceiptFiltersResponse' value with any optional fields omitted.
mkListReceiptFiltersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListReceiptFiltersResponse
mkListReceiptFiltersResponse responseStatus =
  ListReceiptFiltersResponse'
    { filters = Core.Nothing,
      responseStatus
    }

-- | A list of IP address filter data structures, which each consist of a name, an IP address range, and whether to allow or block mail from it.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrrsFilters :: Lens.Lens' ListReceiptFiltersResponse (Core.Maybe [Types.ReceiptFilter])
lrfrrsFilters = Lens.field @"filters"
{-# DEPRECATED lrfrrsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrrsResponseStatus :: Lens.Lens' ListReceiptFiltersResponse Core.Int
lrfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
