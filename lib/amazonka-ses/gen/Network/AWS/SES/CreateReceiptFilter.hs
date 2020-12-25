{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateReceiptFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IP address filter.
--
-- For information about setting up IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateReceiptFilter
  ( -- * Creating a request
    CreateReceiptFilter (..),
    mkCreateReceiptFilter,

    -- ** Request lenses
    crfFilter,

    -- * Destructuring the response
    CreateReceiptFilterResponse (..),
    mkCreateReceiptFilterResponse,

    -- ** Response lenses
    crfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to create a new IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateReceiptFilter' smart constructor.
newtype CreateReceiptFilter = CreateReceiptFilter'
  { -- | A data structure that describes the IP address filter to create, which consists of a name, an IP address range, and whether to allow or block mail from it.
    filter :: Types.ReceiptFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReceiptFilter' value with any optional fields omitted.
mkCreateReceiptFilter ::
  -- | 'filter'
  Types.ReceiptFilter ->
  CreateReceiptFilter
mkCreateReceiptFilter filter = CreateReceiptFilter' {filter}

-- | A data structure that describes the IP address filter to create, which consists of a name, an IP address range, and whether to allow or block mail from it.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crfFilter :: Lens.Lens' CreateReceiptFilter Types.ReceiptFilter
crfFilter = Lens.field @"filter"
{-# DEPRECATED crfFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Core.AWSRequest CreateReceiptFilter where
  type Rs CreateReceiptFilter = CreateReceiptFilterResponse
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
            ( Core.pure ("Action", "CreateReceiptFilter")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Filter" filter)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateReceiptFilterResult"
      ( \s h x ->
          CreateReceiptFilterResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateReceiptFilterResponse' smart constructor.
newtype CreateReceiptFilterResponse = CreateReceiptFilterResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReceiptFilterResponse' value with any optional fields omitted.
mkCreateReceiptFilterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateReceiptFilterResponse
mkCreateReceiptFilterResponse responseStatus =
  CreateReceiptFilterResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crfrrsResponseStatus :: Lens.Lens' CreateReceiptFilterResponse Core.Int
crfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
