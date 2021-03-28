{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeBrokerEngineTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe available engine types and versions.
module Network.AWS.MQ.DescribeBrokerEngineTypes
    (
    -- * Creating a request
      DescribeBrokerEngineTypes (..)
    , mkDescribeBrokerEngineTypes
    -- ** Request lenses
    , dbetEngineType
    , dbetMaxResults
    , dbetNextToken

    -- * Destructuring the response
    , DescribeBrokerEngineTypesResponse (..)
    , mkDescribeBrokerEngineTypesResponse
    -- ** Response lenses
    , dbetrrsBrokerEngineTypes
    , dbetrrsMaxResults
    , dbetrrsNextToken
    , dbetrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBrokerEngineTypes' smart constructor.
data DescribeBrokerEngineTypes = DescribeBrokerEngineTypes'
  { engineType :: Core.Maybe Core.Text
    -- ^ Filter response by engine type.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of engine types that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBrokerEngineTypes' value with any optional fields omitted.
mkDescribeBrokerEngineTypes
    :: DescribeBrokerEngineTypes
mkDescribeBrokerEngineTypes
  = DescribeBrokerEngineTypes'{engineType = Core.Nothing,
                               maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Filter response by engine type.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetEngineType :: Lens.Lens' DescribeBrokerEngineTypes (Core.Maybe Core.Text)
dbetEngineType = Lens.field @"engineType"
{-# INLINEABLE dbetEngineType #-}
{-# DEPRECATED engineType "Use generic-lens or generic-optics with 'engineType' instead"  #-}

-- | The maximum number of engine types that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetMaxResults :: Lens.Lens' DescribeBrokerEngineTypes (Core.Maybe Core.Natural)
dbetMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dbetMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetNextToken :: Lens.Lens' DescribeBrokerEngineTypes (Core.Maybe Core.Text)
dbetNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbetNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeBrokerEngineTypes where
        toQuery DescribeBrokerEngineTypes{..}
          = Core.maybe Core.mempty (Core.toQueryPair "engineType") engineType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders DescribeBrokerEngineTypes where
        toHeaders DescribeBrokerEngineTypes{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeBrokerEngineTypes where
        type Rs DescribeBrokerEngineTypes =
             DescribeBrokerEngineTypesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/v1/broker-engine-types",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBrokerEngineTypesResponse' Core.<$>
                   (x Core..:? "brokerEngineTypes") Core.<*> x Core..:? "maxResults"
                     Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeBrokerEngineTypesResponse' smart constructor.
data DescribeBrokerEngineTypesResponse = DescribeBrokerEngineTypesResponse'
  { brokerEngineTypes :: Core.Maybe [Types.BrokerEngineType]
    -- ^ List of available engine types and versions.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Required. The maximum number of engine types that can be returned per page (20 by default). This value must be an integer from 5 to 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBrokerEngineTypesResponse' value with any optional fields omitted.
mkDescribeBrokerEngineTypesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeBrokerEngineTypesResponse
mkDescribeBrokerEngineTypesResponse responseStatus
  = DescribeBrokerEngineTypesResponse'{brokerEngineTypes =
                                         Core.Nothing,
                                       maxResults = Core.Nothing, nextToken = Core.Nothing,
                                       responseStatus}

-- | List of available engine types and versions.
--
-- /Note:/ Consider using 'brokerEngineTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetrrsBrokerEngineTypes :: Lens.Lens' DescribeBrokerEngineTypesResponse (Core.Maybe [Types.BrokerEngineType])
dbetrrsBrokerEngineTypes = Lens.field @"brokerEngineTypes"
{-# INLINEABLE dbetrrsBrokerEngineTypes #-}
{-# DEPRECATED brokerEngineTypes "Use generic-lens or generic-optics with 'brokerEngineTypes' instead"  #-}

-- | Required. The maximum number of engine types that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetrrsMaxResults :: Lens.Lens' DescribeBrokerEngineTypesResponse (Core.Maybe Core.Natural)
dbetrrsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dbetrrsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetrrsNextToken :: Lens.Lens' DescribeBrokerEngineTypesResponse (Core.Maybe Core.Text)
dbetrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbetrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetrrsResponseStatus :: Lens.Lens' DescribeBrokerEngineTypesResponse Core.Int
dbetrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbetrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
