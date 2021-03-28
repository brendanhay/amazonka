{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetParametersByPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about one or more parameters in a specific hierarchy. 
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetParametersByPath
    (
    -- * Creating a request
      GetParametersByPath (..)
    , mkGetParametersByPath
    -- ** Request lenses
    , gpbpPath
    , gpbpMaxResults
    , gpbpNextToken
    , gpbpParameterFilters
    , gpbpRecursive
    , gpbpWithDecryption

    -- * Destructuring the response
    , GetParametersByPathResponse (..)
    , mkGetParametersByPathResponse
    -- ** Response lenses
    , gpbprrsNextToken
    , gpbprrsParameters
    , gpbprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetParametersByPath' smart constructor.
data GetParametersByPath = GetParametersByPath'
  { path :: Types.PSParameterName
    -- ^ The hierarchy for the parameter. Hierarchies start with a forward slash (/) and end with the parameter name. A parameter name hierarchy can have a maximum of 15 levels. Here is an example of a hierarchy: @/Finance/Prod/IAD/WinServ2016/license33@ 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to start the list. Use this token to get the next set of results. 
  , parameterFilters :: Core.Maybe [Types.ParameterStringFilter]
    -- ^ Filters to limit the request results.
  , recursive :: Core.Maybe Core.Bool
    -- ^ Retrieve all parameters within a hierarchy.
--
-- /Important:/ If a user has access to a path, then the user can access all levels of that path. For example, if a user has permission to access path @/a@ , then the user can also access @/a/b@ . Even if a user has explicitly been denied access in IAM for parameter @/a/b@ , they can still call the GetParametersByPath API action recursively for @/a@ and view @/a/b@ .
  , withDecryption :: Core.Maybe Core.Bool
    -- ^ Retrieve all parameters in a hierarchy with their value decrypted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetParametersByPath' value with any optional fields omitted.
mkGetParametersByPath
    :: Types.PSParameterName -- ^ 'path'
    -> GetParametersByPath
mkGetParametersByPath path
  = GetParametersByPath'{path, maxResults = Core.Nothing,
                         nextToken = Core.Nothing, parameterFilters = Core.Nothing,
                         recursive = Core.Nothing, withDecryption = Core.Nothing}

-- | The hierarchy for the parameter. Hierarchies start with a forward slash (/) and end with the parameter name. A parameter name hierarchy can have a maximum of 15 levels. Here is an example of a hierarchy: @/Finance/Prod/IAD/WinServ2016/license33@ 
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpPath :: Lens.Lens' GetParametersByPath Types.PSParameterName
gpbpPath = Lens.field @"path"
{-# INLINEABLE gpbpPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpMaxResults :: Lens.Lens' GetParametersByPath (Core.Maybe Core.Natural)
gpbpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gpbpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to start the list. Use this token to get the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpNextToken :: Lens.Lens' GetParametersByPath (Core.Maybe Types.NextToken)
gpbpNextToken = Lens.field @"nextToken"
{-# INLINEABLE gpbpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Filters to limit the request results.
--
-- /Note:/ Consider using 'parameterFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpParameterFilters :: Lens.Lens' GetParametersByPath (Core.Maybe [Types.ParameterStringFilter])
gpbpParameterFilters = Lens.field @"parameterFilters"
{-# INLINEABLE gpbpParameterFilters #-}
{-# DEPRECATED parameterFilters "Use generic-lens or generic-optics with 'parameterFilters' instead"  #-}

-- | Retrieve all parameters within a hierarchy.
--
-- /Important:/ If a user has access to a path, then the user can access all levels of that path. For example, if a user has permission to access path @/a@ , then the user can also access @/a/b@ . Even if a user has explicitly been denied access in IAM for parameter @/a/b@ , they can still call the GetParametersByPath API action recursively for @/a@ and view @/a/b@ .
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpRecursive :: Lens.Lens' GetParametersByPath (Core.Maybe Core.Bool)
gpbpRecursive = Lens.field @"recursive"
{-# INLINEABLE gpbpRecursive #-}
{-# DEPRECATED recursive "Use generic-lens or generic-optics with 'recursive' instead"  #-}

-- | Retrieve all parameters in a hierarchy with their value decrypted.
--
-- /Note:/ Consider using 'withDecryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpWithDecryption :: Lens.Lens' GetParametersByPath (Core.Maybe Core.Bool)
gpbpWithDecryption = Lens.field @"withDecryption"
{-# INLINEABLE gpbpWithDecryption #-}
{-# DEPRECATED withDecryption "Use generic-lens or generic-optics with 'withDecryption' instead"  #-}

instance Core.ToQuery GetParametersByPath where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetParametersByPath where
        toHeaders GetParametersByPath{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.GetParametersByPath")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetParametersByPath where
        toJSON GetParametersByPath{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Path" Core..= path),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ParameterFilters" Core..=) Core.<$> parameterFilters,
                  ("Recursive" Core..=) Core.<$> recursive,
                  ("WithDecryption" Core..=) Core.<$> withDecryption])

instance Core.AWSRequest GetParametersByPath where
        type Rs GetParametersByPath = GetParametersByPathResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetParametersByPathResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Parameters" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetParametersByPath where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetParametersByPathResponse' smart constructor.
data GetParametersByPathResponse = GetParametersByPathResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. Use this token to get the next set of results.
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ A list of parameters found in the specified hierarchy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetParametersByPathResponse' value with any optional fields omitted.
mkGetParametersByPathResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetParametersByPathResponse
mkGetParametersByPathResponse responseStatus
  = GetParametersByPathResponse'{nextToken = Core.Nothing,
                                 parameters = Core.Nothing, responseStatus}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbprrsNextToken :: Lens.Lens' GetParametersByPathResponse (Core.Maybe Types.NextToken)
gpbprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gpbprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of parameters found in the specified hierarchy.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbprrsParameters :: Lens.Lens' GetParametersByPathResponse (Core.Maybe [Types.Parameter])
gpbprrsParameters = Lens.field @"parameters"
{-# INLINEABLE gpbprrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbprrsResponseStatus :: Lens.Lens' GetParametersByPathResponse Core.Int
gpbprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpbprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
