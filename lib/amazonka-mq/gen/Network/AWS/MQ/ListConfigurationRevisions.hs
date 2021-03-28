{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.ListConfigurationRevisions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all revisions for the specified configuration.
module Network.AWS.MQ.ListConfigurationRevisions
    (
    -- * Creating a request
      ListConfigurationRevisions (..)
    , mkListConfigurationRevisions
    -- ** Request lenses
    , lcrConfigurationId
    , lcrMaxResults
    , lcrNextToken

    -- * Destructuring the response
    , ListConfigurationRevisionsResponse (..)
    , mkListConfigurationRevisionsResponse
    -- ** Response lenses
    , lcrrrsConfigurationId
    , lcrrrsMaxResults
    , lcrrrsNextToken
    , lcrrrsRevisions
    , lcrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListConfigurationRevisions' smart constructor.
data ListConfigurationRevisions = ListConfigurationRevisions'
  { configurationId :: Core.Text
    -- ^ The unique ID that Amazon MQ generates for the configuration.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConfigurationRevisions' value with any optional fields omitted.
mkListConfigurationRevisions
    :: Core.Text -- ^ 'configurationId'
    -> ListConfigurationRevisions
mkListConfigurationRevisions configurationId
  = ListConfigurationRevisions'{configurationId,
                                maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrConfigurationId :: Lens.Lens' ListConfigurationRevisions Core.Text
lcrConfigurationId = Lens.field @"configurationId"
{-# INLINEABLE lcrConfigurationId #-}
{-# DEPRECATED configurationId "Use generic-lens or generic-optics with 'configurationId' instead"  #-}

-- | The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrMaxResults :: Lens.Lens' ListConfigurationRevisions (Core.Maybe Core.Natural)
lcrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrNextToken :: Lens.Lens' ListConfigurationRevisions (Core.Maybe Core.Text)
lcrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListConfigurationRevisions where
        toQuery ListConfigurationRevisions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListConfigurationRevisions where
        toHeaders ListConfigurationRevisions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListConfigurationRevisions where
        type Rs ListConfigurationRevisions =
             ListConfigurationRevisionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/configurations/" Core.<> Core.toText configurationId Core.<>
                             "/revisions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListConfigurationRevisionsResponse' Core.<$>
                   (x Core..:? "configurationId") Core.<*> x Core..:? "maxResults"
                     Core.<*> x Core..:? "nextToken"
                     Core.<*> x Core..:? "revisions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListConfigurationRevisionsResponse' smart constructor.
data ListConfigurationRevisionsResponse = ListConfigurationRevisionsResponse'
  { configurationId :: Core.Maybe Core.Text
    -- ^ The unique ID that Amazon MQ generates for the configuration.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of configuration revisions that can be returned per page (20 by default). This value must be an integer from 5 to 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
  , revisions :: Core.Maybe [Types.ConfigurationRevision]
    -- ^ The list of all revisions for the specified configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListConfigurationRevisionsResponse' value with any optional fields omitted.
mkListConfigurationRevisionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListConfigurationRevisionsResponse
mkListConfigurationRevisionsResponse responseStatus
  = ListConfigurationRevisionsResponse'{configurationId =
                                          Core.Nothing,
                                        maxResults = Core.Nothing, nextToken = Core.Nothing,
                                        revisions = Core.Nothing, responseStatus}

-- | The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrrsConfigurationId :: Lens.Lens' ListConfigurationRevisionsResponse (Core.Maybe Core.Text)
lcrrrsConfigurationId = Lens.field @"configurationId"
{-# INLINEABLE lcrrrsConfigurationId #-}
{-# DEPRECATED configurationId "Use generic-lens or generic-optics with 'configurationId' instead"  #-}

-- | The maximum number of configuration revisions that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrrsMaxResults :: Lens.Lens' ListConfigurationRevisionsResponse (Core.Maybe Core.Int)
lcrrrsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcrrrsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrrsNextToken :: Lens.Lens' ListConfigurationRevisionsResponse (Core.Maybe Core.Text)
lcrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of all revisions for the specified configuration.
--
-- /Note:/ Consider using 'revisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrrsRevisions :: Lens.Lens' ListConfigurationRevisionsResponse (Core.Maybe [Types.ConfigurationRevision])
lcrrrsRevisions = Lens.field @"revisions"
{-# INLINEABLE lcrrrsRevisions #-}
{-# DEPRECATED revisions "Use generic-lens or generic-optics with 'revisions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrrsResponseStatus :: Lens.Lens' ListConfigurationRevisionsResponse Core.Int
lcrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
