{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteReceiptFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IP address filter.
--
-- For information about managing IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteReceiptFilter
  ( -- * Creating a request
    DeleteReceiptFilter (..),
    mkDeleteReceiptFilter,

    -- ** Request lenses
    drfFilterName,

    -- * Destructuring the response
    DeleteReceiptFilterResponse (..),
    mkDeleteReceiptFilterResponse,

    -- ** Response lenses
    drfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete an IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteReceiptFilter' smart constructor.
newtype DeleteReceiptFilter = DeleteReceiptFilter'
  { -- | The name of the IP address filter to delete.
    filterName :: Types.ReceiptFilterName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReceiptFilter' value with any optional fields omitted.
mkDeleteReceiptFilter ::
  -- | 'filterName'
  Types.ReceiptFilterName ->
  DeleteReceiptFilter
mkDeleteReceiptFilter filterName = DeleteReceiptFilter' {filterName}

-- | The name of the IP address filter to delete.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfFilterName :: Lens.Lens' DeleteReceiptFilter Types.ReceiptFilterName
drfFilterName = Lens.field @"filterName"
{-# DEPRECATED drfFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

instance Core.AWSRequest DeleteReceiptFilter where
  type Rs DeleteReceiptFilter = DeleteReceiptFilterResponse
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
            ( Core.pure ("Action", "DeleteReceiptFilter")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "FilterName" filterName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteReceiptFilterResult"
      ( \s h x ->
          DeleteReceiptFilterResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteReceiptFilterResponse' smart constructor.
newtype DeleteReceiptFilterResponse = DeleteReceiptFilterResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReceiptFilterResponse' value with any optional fields omitted.
mkDeleteReceiptFilterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteReceiptFilterResponse
mkDeleteReceiptFilterResponse responseStatus =
  DeleteReceiptFilterResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrrsResponseStatus :: Lens.Lens' DeleteReceiptFilterResponse Core.Int
drfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
