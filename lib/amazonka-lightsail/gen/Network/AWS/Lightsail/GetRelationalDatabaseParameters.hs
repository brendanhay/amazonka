{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the runtime parameters offered by the underlying database software, or engine, for a specific database in Amazon Lightsail.
--
-- In addition to the parameter names and values, this operation returns other information about each parameter. This information includes whether changes require a reboot, whether the parameter is modifiable, the allowed values, and the data types.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseParameters
    (
    -- * Creating a request
      GetRelationalDatabaseParameters (..)
    , mkGetRelationalDatabaseParameters
    -- ** Request lenses
    , grdpRelationalDatabaseName
    , grdpPageToken

    -- * Destructuring the response
    , GetRelationalDatabaseParametersResponse (..)
    , mkGetRelationalDatabaseParametersResponse
    -- ** Response lenses
    , grdprrsNextPageToken
    , grdprrsParameters
    , grdprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabaseParameters' smart constructor.
data GetRelationalDatabaseParameters = GetRelationalDatabaseParameters'
  { relationalDatabaseName :: Types.ResourceName
    -- ^ The name of your database for which to get parameters.
  , pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseParameters@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseParameters' value with any optional fields omitted.
mkGetRelationalDatabaseParameters
    :: Types.ResourceName -- ^ 'relationalDatabaseName'
    -> GetRelationalDatabaseParameters
mkGetRelationalDatabaseParameters relationalDatabaseName
  = GetRelationalDatabaseParameters'{relationalDatabaseName,
                                     pageToken = Core.Nothing}

-- | The name of your database for which to get parameters.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdpRelationalDatabaseName :: Lens.Lens' GetRelationalDatabaseParameters Types.ResourceName
grdpRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# INLINEABLE grdpRelationalDatabaseName #-}
{-# DEPRECATED relationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseParameters@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdpPageToken :: Lens.Lens' GetRelationalDatabaseParameters (Core.Maybe Core.Text)
grdpPageToken = Lens.field @"pageToken"
{-# INLINEABLE grdpPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetRelationalDatabaseParameters where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRelationalDatabaseParameters where
        toHeaders GetRelationalDatabaseParameters{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.GetRelationalDatabaseParameters")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRelationalDatabaseParameters where
        toJSON GetRelationalDatabaseParameters{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("relationalDatabaseName" Core..= relationalDatabaseName),
                  ("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetRelationalDatabaseParameters where
        type Rs GetRelationalDatabaseParameters =
             GetRelationalDatabaseParametersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseParametersResponse' Core.<$>
                   (x Core..:? "nextPageToken") Core.<*> x Core..:? "parameters"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetRelationalDatabaseParameters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetRelationalDatabaseParametersResponse' smart constructor.
data GetRelationalDatabaseParametersResponse = GetRelationalDatabaseParametersResponse'
  { nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseParameters@ request and specify the next page token using the @pageToken@ parameter.
  , parameters :: Core.Maybe [Types.RelationalDatabaseParameter]
    -- ^ An object describing the result of your get relational database parameters request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseParametersResponse' value with any optional fields omitted.
mkGetRelationalDatabaseParametersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRelationalDatabaseParametersResponse
mkGetRelationalDatabaseParametersResponse responseStatus
  = GetRelationalDatabaseParametersResponse'{nextPageToken =
                                               Core.Nothing,
                                             parameters = Core.Nothing, responseStatus}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseParameters@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdprrsNextPageToken :: Lens.Lens' GetRelationalDatabaseParametersResponse (Core.Maybe Core.Text)
grdprrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE grdprrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | An object describing the result of your get relational database parameters request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdprrsParameters :: Lens.Lens' GetRelationalDatabaseParametersResponse (Core.Maybe [Types.RelationalDatabaseParameter])
grdprrsParameters = Lens.field @"parameters"
{-# INLINEABLE grdprrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdprrsResponseStatus :: Lens.Lens' GetRelationalDatabaseParametersResponse Core.Int
grdprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grdprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
