{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetIntentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions of an intent.
--
-- The @GetIntentVersions@ operation returns an @IntentMetadata@ object for each version of an intent. For example, if an intent has three numbered versions, the @GetIntentVersions@ operation returns four @IntentMetadata@ objects in the response, one for each numbered version and one for the @> LATEST@ version. 
-- The @GetIntentVersions@ operation always returns at least one version, the @> LATEST@ version.
-- This operation requires permissions for the @lex:GetIntentVersions@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetIntentVersions
    (
    -- * Creating a request
      GetIntentVersions (..)
    , mkGetIntentVersions
    -- ** Request lenses
    , givName
    , givMaxResults
    , givNextToken

    -- * Destructuring the response
    , GetIntentVersionsResponse (..)
    , mkGetIntentVersionsResponse
    -- ** Response lenses
    , givrrsIntents
    , givrrsNextToken
    , givrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetIntentVersions' smart constructor.
data GetIntentVersions = GetIntentVersions'
  { name :: Types.IntentName
    -- ^ The name of the intent for which versions should be returned.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of intent versions to return in the response. The default is 10.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIntentVersions' value with any optional fields omitted.
mkGetIntentVersions
    :: Types.IntentName -- ^ 'name'
    -> GetIntentVersions
mkGetIntentVersions name
  = GetIntentVersions'{name, maxResults = Core.Nothing,
                       nextToken = Core.Nothing}

-- | The name of the intent for which versions should be returned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givName :: Lens.Lens' GetIntentVersions Types.IntentName
givName = Lens.field @"name"
{-# INLINEABLE givName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The maximum number of intent versions to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givMaxResults :: Lens.Lens' GetIntentVersions (Core.Maybe Core.Natural)
givMaxResults = Lens.field @"maxResults"
{-# INLINEABLE givMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givNextToken :: Lens.Lens' GetIntentVersions (Core.Maybe Types.NextToken)
givNextToken = Lens.field @"nextToken"
{-# INLINEABLE givNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetIntentVersions where
        toQuery GetIntentVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders GetIntentVersions where
        toHeaders GetIntentVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetIntentVersions where
        type Rs GetIntentVersions = GetIntentVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/intents/" Core.<> Core.toText name Core.<> "/versions/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetIntentVersionsResponse' Core.<$>
                   (x Core..:? "intents") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetIntentVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"intents" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetIntentVersionsResponse' smart constructor.
data GetIntentVersionsResponse = GetIntentVersionsResponse'
  { intents :: Core.Maybe [Types.IntentMetadata]
    -- ^ An array of @IntentMetadata@ objects, one for each numbered version of the intent plus one for the @> LATEST@ version.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetIntentVersionsResponse' value with any optional fields omitted.
mkGetIntentVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetIntentVersionsResponse
mkGetIntentVersionsResponse responseStatus
  = GetIntentVersionsResponse'{intents = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | An array of @IntentMetadata@ objects, one for each numbered version of the intent plus one for the @> LATEST@ version.
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givrrsIntents :: Lens.Lens' GetIntentVersionsResponse (Core.Maybe [Types.IntentMetadata])
givrrsIntents = Lens.field @"intents"
{-# INLINEABLE givrrsIntents #-}
{-# DEPRECATED intents "Use generic-lens or generic-optics with 'intents' instead"  #-}

-- | A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givrrsNextToken :: Lens.Lens' GetIntentVersionsResponse (Core.Maybe Types.NextToken)
givrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE givrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givrrsResponseStatus :: Lens.Lens' GetIntentVersionsResponse Core.Int
givrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE givrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
