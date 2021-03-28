{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available database blueprints in Amazon Lightsail. A blueprint describes the major engine version of a database.
--
-- You can use a blueprint ID to create a new database that runs a specific database engine.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
    (
    -- * Creating a request
      GetRelationalDatabaseBlueprints (..)
    , mkGetRelationalDatabaseBlueprints
    -- ** Request lenses
    , grdbPageToken

    -- * Destructuring the response
    , GetRelationalDatabaseBlueprintsResponse (..)
    , mkGetRelationalDatabaseBlueprintsResponse
    -- ** Response lenses
    , grdbrrsBlueprints
    , grdbrrsNextPageToken
    , grdbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabaseBlueprints' smart constructor.
newtype GetRelationalDatabaseBlueprints = GetRelationalDatabaseBlueprints'
  { pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseBlueprints' value with any optional fields omitted.
mkGetRelationalDatabaseBlueprints
    :: GetRelationalDatabaseBlueprints
mkGetRelationalDatabaseBlueprints
  = GetRelationalDatabaseBlueprints'{pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbPageToken :: Lens.Lens' GetRelationalDatabaseBlueprints (Core.Maybe Core.Text)
grdbPageToken = Lens.field @"pageToken"
{-# INLINEABLE grdbPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetRelationalDatabaseBlueprints where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRelationalDatabaseBlueprints where
        toHeaders GetRelationalDatabaseBlueprints{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.GetRelationalDatabaseBlueprints")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRelationalDatabaseBlueprints where
        toJSON GetRelationalDatabaseBlueprints{..}
          = Core.object
              (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetRelationalDatabaseBlueprints where
        type Rs GetRelationalDatabaseBlueprints =
             GetRelationalDatabaseBlueprintsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseBlueprintsResponse' Core.<$>
                   (x Core..:? "blueprints") Core.<*> x Core..:? "nextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetRelationalDatabaseBlueprints where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"blueprints" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetRelationalDatabaseBlueprintsResponse' smart constructor.
data GetRelationalDatabaseBlueprintsResponse = GetRelationalDatabaseBlueprintsResponse'
  { blueprints :: Core.Maybe [Types.RelationalDatabaseBlueprint]
    -- ^ An object describing the result of your get relational database blueprints request.
  , nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseBlueprints@ request and specify the next page token using the @pageToken@ parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseBlueprintsResponse' value with any optional fields omitted.
mkGetRelationalDatabaseBlueprintsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRelationalDatabaseBlueprintsResponse
mkGetRelationalDatabaseBlueprintsResponse responseStatus
  = GetRelationalDatabaseBlueprintsResponse'{blueprints =
                                               Core.Nothing,
                                             nextPageToken = Core.Nothing, responseStatus}

-- | An object describing the result of your get relational database blueprints request.
--
-- /Note:/ Consider using 'blueprints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrrsBlueprints :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse (Core.Maybe [Types.RelationalDatabaseBlueprint])
grdbrrsBlueprints = Lens.field @"blueprints"
{-# INLINEABLE grdbrrsBlueprints #-}
{-# DEPRECATED blueprints "Use generic-lens or generic-optics with 'blueprints' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseBlueprints@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrrsNextPageToken :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse (Core.Maybe Core.Text)
grdbrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE grdbrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrrsResponseStatus :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse Core.Int
grdbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grdbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
