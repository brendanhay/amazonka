{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetApiKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current 'ApiKeys' resource.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetApiKeys
    (
    -- * Creating a request
      GetApiKeys (..)
    , mkGetApiKeys
    -- ** Request lenses
    , gakCustomerId
    , gakIncludeValues
    , gakLimit
    , gakNameQuery
    , gakPosition

    -- * Destructuring the response
    , GetApiKeysResponse (..)
    , mkGetApiKeysResponse
    -- ** Response lenses
    , gakrrsItems
    , gakrrsPosition
    , gakrrsWarnings
    , gakrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to get information about the current 'ApiKeys' resource.
--
-- /See:/ 'mkGetApiKeys' smart constructor.
data GetApiKeys = GetApiKeys'
  { customerId :: Core.Maybe Core.Text
    -- ^ The identifier of a customer in AWS Marketplace or an external system, such as a developer portal.
  , includeValues :: Core.Maybe Core.Bool
    -- ^ A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains key values.
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , nameQuery :: Core.Maybe Core.Text
    -- ^ The name of queried API keys.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApiKeys' value with any optional fields omitted.
mkGetApiKeys
    :: GetApiKeys
mkGetApiKeys
  = GetApiKeys'{customerId = Core.Nothing,
                includeValues = Core.Nothing, limit = Core.Nothing,
                nameQuery = Core.Nothing, position = Core.Nothing}

-- | The identifier of a customer in AWS Marketplace or an external system, such as a developer portal.
--
-- /Note:/ Consider using 'customerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakCustomerId :: Lens.Lens' GetApiKeys (Core.Maybe Core.Text)
gakCustomerId = Lens.field @"customerId"
{-# INLINEABLE gakCustomerId #-}
{-# DEPRECATED customerId "Use generic-lens or generic-optics with 'customerId' instead"  #-}

-- | A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains key values.
--
-- /Note:/ Consider using 'includeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakIncludeValues :: Lens.Lens' GetApiKeys (Core.Maybe Core.Bool)
gakIncludeValues = Lens.field @"includeValues"
{-# INLINEABLE gakIncludeValues #-}
{-# DEPRECATED includeValues "Use generic-lens or generic-optics with 'includeValues' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakLimit :: Lens.Lens' GetApiKeys (Core.Maybe Core.Int)
gakLimit = Lens.field @"limit"
{-# INLINEABLE gakLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The name of queried API keys.
--
-- /Note:/ Consider using 'nameQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakNameQuery :: Lens.Lens' GetApiKeys (Core.Maybe Core.Text)
gakNameQuery = Lens.field @"nameQuery"
{-# INLINEABLE gakNameQuery #-}
{-# DEPRECATED nameQuery "Use generic-lens or generic-optics with 'nameQuery' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakPosition :: Lens.Lens' GetApiKeys (Core.Maybe Core.Text)
gakPosition = Lens.field @"position"
{-# INLINEABLE gakPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetApiKeys where
        toQuery GetApiKeys{..}
          = Core.maybe Core.mempty (Core.toQueryPair "customerId") customerId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "includeValues")
                includeValues
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "limit") limit
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "name") nameQuery
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetApiKeys where
        toHeaders GetApiKeys{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetApiKeys where
        type Rs GetApiKeys = GetApiKeysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/apikeys",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetApiKeysResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     x Core..:? "warnings"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetApiKeys where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | Represents a collection of API keys as represented by an 'ApiKeys' resource.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html Use API Keys> 
--
-- /See:/ 'mkGetApiKeysResponse' smart constructor.
data GetApiKeysResponse = GetApiKeysResponse'
  { items :: Core.Maybe [Types.ApiKey]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , warnings :: Core.Maybe [Core.Text]
    -- ^ A list of warning messages logged during the import of API keys when the @failOnWarnings@ option is set to true.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetApiKeysResponse' value with any optional fields omitted.
mkGetApiKeysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetApiKeysResponse
mkGetApiKeysResponse responseStatus
  = GetApiKeysResponse'{items = Core.Nothing,
                        position = Core.Nothing, warnings = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakrrsItems :: Lens.Lens' GetApiKeysResponse (Core.Maybe [Types.ApiKey])
gakrrsItems = Lens.field @"items"
{-# INLINEABLE gakrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakrrsPosition :: Lens.Lens' GetApiKeysResponse (Core.Maybe Core.Text)
gakrrsPosition = Lens.field @"position"
{-# INLINEABLE gakrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | A list of warning messages logged during the import of API keys when the @failOnWarnings@ option is set to true.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakrrsWarnings :: Lens.Lens' GetApiKeysResponse (Core.Maybe [Core.Text])
gakrrsWarnings = Lens.field @"warnings"
{-# INLINEABLE gakrrsWarnings #-}
{-# DEPRECATED warnings "Use generic-lens or generic-optics with 'warnings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakrrsResponseStatus :: Lens.Lens' GetApiKeysResponse Core.Int
gakrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gakrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
