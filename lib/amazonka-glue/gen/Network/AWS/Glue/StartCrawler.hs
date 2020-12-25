{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a crawl using the specified crawler, regardless of what is scheduled. If the crawler is already running, returns a <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-exceptions.html#aws-glue-api-exceptions-CrawlerRunningException CrawlerRunningException> .
module Network.AWS.Glue.StartCrawler
  ( -- * Creating a request
    StartCrawler (..),
    mkStartCrawler,

    -- ** Request lenses
    scfName,

    -- * Destructuring the response
    StartCrawlerResponse (..),
    mkStartCrawlerResponse,

    -- ** Response lenses
    scrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartCrawler' smart constructor.
newtype StartCrawler = StartCrawler'
  { -- | Name of the crawler to start.
    name :: Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartCrawler' value with any optional fields omitted.
mkStartCrawler ::
  -- | 'name'
  Types.NameString ->
  StartCrawler
mkStartCrawler name = StartCrawler' {name}

-- | Name of the crawler to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scfName :: Lens.Lens' StartCrawler Types.NameString
scfName = Lens.field @"name"
{-# DEPRECATED scfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON StartCrawler where
  toJSON StartCrawler {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StartCrawler where
  type Rs StartCrawler = StartCrawlerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.StartCrawler")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartCrawlerResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartCrawlerResponse' smart constructor.
newtype StartCrawlerResponse = StartCrawlerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartCrawlerResponse' value with any optional fields omitted.
mkStartCrawlerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartCrawlerResponse
mkStartCrawlerResponse responseStatus =
  StartCrawlerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsResponseStatus :: Lens.Lens' StartCrawlerResponse Core.Int
scrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
