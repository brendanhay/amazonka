{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeBrokerInstanceOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe available broker instance options.
module Network.AWS.MQ.DescribeBrokerInstanceOptions
    (
    -- * Creating a request
      DescribeBrokerInstanceOptions (..)
    , mkDescribeBrokerInstanceOptions
    -- ** Request lenses
    , dbioEngineType
    , dbioHostInstanceType
    , dbioMaxResults
    , dbioNextToken
    , dbioStorageType

    -- * Destructuring the response
    , DescribeBrokerInstanceOptionsResponse (..)
    , mkDescribeBrokerInstanceOptionsResponse
    -- ** Response lenses
    , dbiorrsBrokerInstanceOptions
    , dbiorrsMaxResults
    , dbiorrsNextToken
    , dbiorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBrokerInstanceOptions' smart constructor.
data DescribeBrokerInstanceOptions = DescribeBrokerInstanceOptions'
  { engineType :: Core.Maybe Core.Text
    -- ^ Filter response by engine type.
  , hostInstanceType :: Core.Maybe Core.Text
    -- ^ Filter response by host instance type.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of instance options that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
  , storageType :: Core.Maybe Core.Text
    -- ^ Filter response by storage type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBrokerInstanceOptions' value with any optional fields omitted.
mkDescribeBrokerInstanceOptions
    :: DescribeBrokerInstanceOptions
mkDescribeBrokerInstanceOptions
  = DescribeBrokerInstanceOptions'{engineType = Core.Nothing,
                                   hostInstanceType = Core.Nothing, maxResults = Core.Nothing,
                                   nextToken = Core.Nothing, storageType = Core.Nothing}

-- | Filter response by engine type.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioEngineType :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
dbioEngineType = Lens.field @"engineType"
{-# INLINEABLE dbioEngineType #-}
{-# DEPRECATED engineType "Use generic-lens or generic-optics with 'engineType' instead"  #-}

-- | Filter response by host instance type.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioHostInstanceType :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
dbioHostInstanceType = Lens.field @"hostInstanceType"
{-# INLINEABLE dbioHostInstanceType #-}
{-# DEPRECATED hostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead"  #-}

-- | The maximum number of instance options that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioMaxResults :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Natural)
dbioMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dbioMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioNextToken :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
dbioNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbioNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Filter response by storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioStorageType :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
dbioStorageType = Lens.field @"storageType"
{-# INLINEABLE dbioStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

instance Core.ToQuery DescribeBrokerInstanceOptions where
        toQuery DescribeBrokerInstanceOptions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "engineType") engineType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "hostInstanceType")
                hostInstanceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "storageType") storageType

instance Core.ToHeaders DescribeBrokerInstanceOptions where
        toHeaders DescribeBrokerInstanceOptions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeBrokerInstanceOptions where
        type Rs DescribeBrokerInstanceOptions =
             DescribeBrokerInstanceOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/v1/broker-instance-options",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBrokerInstanceOptionsResponse' Core.<$>
                   (x Core..:? "brokerInstanceOptions") Core.<*>
                     x Core..:? "maxResults"
                     Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeBrokerInstanceOptionsResponse' smart constructor.
data DescribeBrokerInstanceOptionsResponse = DescribeBrokerInstanceOptionsResponse'
  { brokerInstanceOptions :: Core.Maybe [Types.BrokerInstanceOption]
    -- ^ List of available broker instance options.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Required. The maximum number of instance options that can be returned per page (20 by default). This value must be an integer from 5 to 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBrokerInstanceOptionsResponse' value with any optional fields omitted.
mkDescribeBrokerInstanceOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeBrokerInstanceOptionsResponse
mkDescribeBrokerInstanceOptionsResponse responseStatus
  = DescribeBrokerInstanceOptionsResponse'{brokerInstanceOptions =
                                             Core.Nothing,
                                           maxResults = Core.Nothing, nextToken = Core.Nothing,
                                           responseStatus}

-- | List of available broker instance options.
--
-- /Note:/ Consider using 'brokerInstanceOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorrsBrokerInstanceOptions :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Core.Maybe [Types.BrokerInstanceOption])
dbiorrsBrokerInstanceOptions = Lens.field @"brokerInstanceOptions"
{-# INLINEABLE dbiorrsBrokerInstanceOptions #-}
{-# DEPRECATED brokerInstanceOptions "Use generic-lens or generic-optics with 'brokerInstanceOptions' instead"  #-}

-- | Required. The maximum number of instance options that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorrsMaxResults :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Core.Maybe Core.Natural)
dbiorrsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dbiorrsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorrsNextToken :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Core.Maybe Core.Text)
dbiorrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbiorrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorrsResponseStatus :: Lens.Lens' DescribeBrokerInstanceOptionsResponse Core.Int
dbiorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbiorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
