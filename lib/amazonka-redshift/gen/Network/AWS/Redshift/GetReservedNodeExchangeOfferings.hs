{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.GetReservedNodeExchangeOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of DC2 ReservedNodeOfferings that matches the payment type, term, and usage price of the given DC1 reserved node.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.GetReservedNodeExchangeOfferings
    (
    -- * Creating a request
      GetReservedNodeExchangeOfferings (..)
    , mkGetReservedNodeExchangeOfferings
    -- ** Request lenses
    , grneoReservedNodeId
    , grneoMarker
    , grneoMaxRecords

    -- * Destructuring the response
    , GetReservedNodeExchangeOfferingsResponse (..)
    , mkGetReservedNodeExchangeOfferingsResponse
    -- ** Response lenses
    , grneorrsMarker
    , grneorrsReservedNodeOfferings
    , grneorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkGetReservedNodeExchangeOfferings' smart constructor.
data GetReservedNodeExchangeOfferings = GetReservedNodeExchangeOfferings'
  { reservedNodeId :: Core.Text
    -- ^ A string representing the node identifier for the DC1 Reserved Node to be exchanged.
  , marker :: Core.Maybe Core.Text
    -- ^ A value that indicates the starting point for the next set of ReservedNodeOfferings.
  , maxRecords :: Core.Maybe Core.Int
    -- ^ An integer setting the maximum number of ReservedNodeOfferings to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReservedNodeExchangeOfferings' value with any optional fields omitted.
mkGetReservedNodeExchangeOfferings
    :: Core.Text -- ^ 'reservedNodeId'
    -> GetReservedNodeExchangeOfferings
mkGetReservedNodeExchangeOfferings reservedNodeId
  = GetReservedNodeExchangeOfferings'{reservedNodeId,
                                      marker = Core.Nothing, maxRecords = Core.Nothing}

-- | A string representing the node identifier for the DC1 Reserved Node to be exchanged.
--
-- /Note:/ Consider using 'reservedNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneoReservedNodeId :: Lens.Lens' GetReservedNodeExchangeOfferings Core.Text
grneoReservedNodeId = Lens.field @"reservedNodeId"
{-# INLINEABLE grneoReservedNodeId #-}
{-# DEPRECATED reservedNodeId "Use generic-lens or generic-optics with 'reservedNodeId' instead"  #-}

-- | A value that indicates the starting point for the next set of ReservedNodeOfferings.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneoMarker :: Lens.Lens' GetReservedNodeExchangeOfferings (Core.Maybe Core.Text)
grneoMarker = Lens.field @"marker"
{-# INLINEABLE grneoMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | An integer setting the maximum number of ReservedNodeOfferings to retrieve.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneoMaxRecords :: Lens.Lens' GetReservedNodeExchangeOfferings (Core.Maybe Core.Int)
grneoMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE grneoMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery GetReservedNodeExchangeOfferings where
        toQuery GetReservedNodeExchangeOfferings{..}
          = Core.toQueryPair "Action"
              ("GetReservedNodeExchangeOfferings" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ReservedNodeId" reservedNodeId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders GetReservedNodeExchangeOfferings where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetReservedNodeExchangeOfferings where
        type Rs GetReservedNodeExchangeOfferings =
             GetReservedNodeExchangeOfferingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "GetReservedNodeExchangeOfferingsResult"
              (\ s h x ->
                 GetReservedNodeExchangeOfferingsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "ReservedNodeOfferings" Core..<@>
                       Core.parseXMLList "ReservedNodeOffering"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetReservedNodeExchangeOfferings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"reservedNodeOfferings" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkGetReservedNodeExchangeOfferingsResponse' smart constructor.
data GetReservedNodeExchangeOfferingsResponse = GetReservedNodeExchangeOfferingsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point for returning a set of response records. When the results of a @GetReservedNodeExchangeOfferings@ request exceed the value specified in MaxRecords, Amazon Redshift returns a value in the marker field of the response. You can retrieve the next set of response records by providing the returned marker value in the marker parameter and retrying the request. 
  , reservedNodeOfferings :: Core.Maybe [Types.ReservedNodeOffering]
    -- ^ Returns an array of 'ReservedNodeOffering' objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReservedNodeExchangeOfferingsResponse' value with any optional fields omitted.
mkGetReservedNodeExchangeOfferingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetReservedNodeExchangeOfferingsResponse
mkGetReservedNodeExchangeOfferingsResponse responseStatus
  = GetReservedNodeExchangeOfferingsResponse'{marker = Core.Nothing,
                                              reservedNodeOfferings = Core.Nothing, responseStatus}

-- | An optional parameter that specifies the starting point for returning a set of response records. When the results of a @GetReservedNodeExchangeOfferings@ request exceed the value specified in MaxRecords, Amazon Redshift returns a value in the marker field of the response. You can retrieve the next set of response records by providing the returned marker value in the marker parameter and retrying the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneorrsMarker :: Lens.Lens' GetReservedNodeExchangeOfferingsResponse (Core.Maybe Core.Text)
grneorrsMarker = Lens.field @"marker"
{-# INLINEABLE grneorrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Returns an array of 'ReservedNodeOffering' objects.
--
-- /Note:/ Consider using 'reservedNodeOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneorrsReservedNodeOfferings :: Lens.Lens' GetReservedNodeExchangeOfferingsResponse (Core.Maybe [Types.ReservedNodeOffering])
grneorrsReservedNodeOfferings = Lens.field @"reservedNodeOfferings"
{-# INLINEABLE grneorrsReservedNodeOfferings #-}
{-# DEPRECATED reservedNodeOfferings "Use generic-lens or generic-optics with 'reservedNodeOfferings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneorrsResponseStatus :: Lens.Lens' GetReservedNodeExchangeOfferingsResponse Core.Int
grneorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grneorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
