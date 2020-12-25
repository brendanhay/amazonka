{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a specified crawler from the AWS Glue Data Catalog, unless the crawler state is @RUNNING@ .
module Network.AWS.Glue.DeleteCrawler
  ( -- * Creating a request
    DeleteCrawler (..),
    mkDeleteCrawler,

    -- ** Request lenses
    dcName,

    -- * Destructuring the response
    DeleteCrawlerResponse (..),
    mkDeleteCrawlerResponse,

    -- ** Response lenses
    dcrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCrawler' smart constructor.
newtype DeleteCrawler = DeleteCrawler'
  { -- | The name of the crawler to remove.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCrawler' value with any optional fields omitted.
mkDeleteCrawler ::
  -- | 'name'
  Types.Name ->
  DeleteCrawler
mkDeleteCrawler name = DeleteCrawler' {name}

-- | The name of the crawler to remove.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcName :: Lens.Lens' DeleteCrawler Types.Name
dcName = Lens.field @"name"
{-# DEPRECATED dcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteCrawler where
  toJSON DeleteCrawler {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteCrawler where
  type Rs DeleteCrawler = DeleteCrawlerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeleteCrawler")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCrawlerResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteCrawlerResponse' smart constructor.
newtype DeleteCrawlerResponse = DeleteCrawlerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCrawlerResponse' value with any optional fields omitted.
mkDeleteCrawlerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteCrawlerResponse
mkDeleteCrawlerResponse responseStatus =
  DeleteCrawlerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsResponseStatus :: Lens.Lens' DeleteCrawlerResponse Core.Int
dcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
