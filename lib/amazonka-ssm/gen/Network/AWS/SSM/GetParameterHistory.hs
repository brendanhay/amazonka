{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetParameterHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the history of all changes to a parameter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetParameterHistory
    (
    -- * Creating a request
      GetParameterHistory (..)
    , mkGetParameterHistory
    -- ** Request lenses
    , gphName
    , gphMaxResults
    , gphNextToken
    , gphWithDecryption

    -- * Destructuring the response
    , GetParameterHistoryResponse (..)
    , mkGetParameterHistoryResponse
    -- ** Response lenses
    , gphrrsNextToken
    , gphrrsParameters
    , gphrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetParameterHistory' smart constructor.
data GetParameterHistory = GetParameterHistory'
  { name :: Types.PSParameterName
    -- ^ The name of the parameter for which you want to review history.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  , withDecryption :: Core.Maybe Core.Bool
    -- ^ Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetParameterHistory' value with any optional fields omitted.
mkGetParameterHistory
    :: Types.PSParameterName -- ^ 'name'
    -> GetParameterHistory
mkGetParameterHistory name
  = GetParameterHistory'{name, maxResults = Core.Nothing,
                         nextToken = Core.Nothing, withDecryption = Core.Nothing}

-- | The name of the parameter for which you want to review history.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphName :: Lens.Lens' GetParameterHistory Types.PSParameterName
gphName = Lens.field @"name"
{-# INLINEABLE gphName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphMaxResults :: Lens.Lens' GetParameterHistory (Core.Maybe Core.Natural)
gphMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gphMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphNextToken :: Lens.Lens' GetParameterHistory (Core.Maybe Types.NextToken)
gphNextToken = Lens.field @"nextToken"
{-# INLINEABLE gphNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
--
-- /Note:/ Consider using 'withDecryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphWithDecryption :: Lens.Lens' GetParameterHistory (Core.Maybe Core.Bool)
gphWithDecryption = Lens.field @"withDecryption"
{-# INLINEABLE gphWithDecryption #-}
{-# DEPRECATED withDecryption "Use generic-lens or generic-optics with 'withDecryption' instead"  #-}

instance Core.ToQuery GetParameterHistory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetParameterHistory where
        toHeaders GetParameterHistory{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.GetParameterHistory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetParameterHistory where
        toJSON GetParameterHistory{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("WithDecryption" Core..=) Core.<$> withDecryption])

instance Core.AWSRequest GetParameterHistory where
        type Rs GetParameterHistory = GetParameterHistoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetParameterHistoryResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Parameters" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetParameterHistory where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetParameterHistoryResponse' smart constructor.
data GetParameterHistoryResponse = GetParameterHistoryResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , parameters :: Core.Maybe [Types.ParameterHistory]
    -- ^ A list of parameters returned by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetParameterHistoryResponse' value with any optional fields omitted.
mkGetParameterHistoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetParameterHistoryResponse
mkGetParameterHistoryResponse responseStatus
  = GetParameterHistoryResponse'{nextToken = Core.Nothing,
                                 parameters = Core.Nothing, responseStatus}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphrrsNextToken :: Lens.Lens' GetParameterHistoryResponse (Core.Maybe Types.NextToken)
gphrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gphrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of parameters returned by the request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphrrsParameters :: Lens.Lens' GetParameterHistoryResponse (Core.Maybe [Types.ParameterHistory])
gphrrsParameters = Lens.field @"parameters"
{-# INLINEABLE gphrrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphrrsResponseStatus :: Lens.Lens' GetParameterHistoryResponse Core.Int
gphrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gphrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
