{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about artifacts.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListArtifacts
    (
    -- * Creating a request
      ListArtifacts (..)
    , mkListArtifacts
    -- ** Request lenses
    , laArn
    , laType
    , laNextToken

    -- * Destructuring the response
    , ListArtifactsResponse (..)
    , mkListArtifactsResponse
    -- ** Response lenses
    , larrsArtifacts
    , larrsNextToken
    , larrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list artifacts operation.
--
-- /See:/ 'mkListArtifacts' smart constructor.
data ListArtifacts = ListArtifacts'
  { arn :: Types.AmazonResourceName
    -- ^ The run, job, suite, or test ARN.
  , type' :: Types.ArtifactCategory
    -- ^ The artifacts' type.
--
-- Allowed values include:
--
--     * FILE
--
--
--     * LOG
--
--
--     * SCREENSHOT
--
--
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListArtifacts' value with any optional fields omitted.
mkListArtifacts
    :: Types.AmazonResourceName -- ^ 'arn'
    -> Types.ArtifactCategory -- ^ 'type\''
    -> ListArtifacts
mkListArtifacts arn type'
  = ListArtifacts'{arn, type', nextToken = Core.Nothing}

-- | The run, job, suite, or test ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laArn :: Lens.Lens' ListArtifacts Types.AmazonResourceName
laArn = Lens.field @"arn"
{-# INLINEABLE laArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The artifacts' type.
--
-- Allowed values include:
--
--     * FILE
--
--
--     * LOG
--
--
--     * SCREENSHOT
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laType :: Lens.Lens' ListArtifacts Types.ArtifactCategory
laType = Lens.field @"type'"
{-# INLINEABLE laType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListArtifacts (Core.Maybe Types.PaginationToken)
laNextToken = Lens.field @"nextToken"
{-# INLINEABLE laNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListArtifacts where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListArtifacts where
        toHeaders ListArtifacts{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.ListArtifacts")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListArtifacts where
        toJSON ListArtifacts{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("arn" Core..= arn), Core.Just ("type" Core..= type'),
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListArtifacts where
        type Rs ListArtifacts = ListArtifactsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListArtifactsResponse' Core.<$>
                   (x Core..:? "artifacts") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListArtifacts where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"artifacts" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the result of a list artifacts operation.
--
-- /See:/ 'mkListArtifactsResponse' smart constructor.
data ListArtifactsResponse = ListArtifactsResponse'
  { artifacts :: Core.Maybe [Types.Artifact]
    -- ^ Information about the artifacts.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListArtifactsResponse' value with any optional fields omitted.
mkListArtifactsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListArtifactsResponse
mkListArtifactsResponse responseStatus
  = ListArtifactsResponse'{artifacts = Core.Nothing,
                           nextToken = Core.Nothing, responseStatus}

-- | Information about the artifacts.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsArtifacts :: Lens.Lens' ListArtifactsResponse (Core.Maybe [Types.Artifact])
larrsArtifacts = Lens.field @"artifacts"
{-# INLINEABLE larrsArtifacts #-}
{-# DEPRECATED artifacts "Use generic-lens or generic-optics with 'artifacts' instead"  #-}

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListArtifactsResponse (Core.Maybe Types.PaginationToken)
larrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE larrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListArtifactsResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
