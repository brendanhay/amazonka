{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the usage data of a usage plan in a specified time interval.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetUsage
    (
    -- * Creating a request
      GetUsage (..)
    , mkGetUsage
    -- ** Request lenses
    , guUsagePlanId
    , guStartDate
    , guEndDate
    , guKeyId
    , guLimit
    , guPosition

     -- * Destructuring the response
    , Types.Usage (..)
    , Types.mkUsage
    -- ** Response lenses
    , Types.uEndDate
    , Types.uItems
    , Types.uPosition
    , Types.uStartDate
    , Types.uUsagePlanId
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get the usage data of a usage plan in a specified time interval.
--
-- /See:/ 'mkGetUsage' smart constructor.
data GetUsage = GetUsage'
  { usagePlanId :: Core.Text
    -- ^ [Required] The Id of the usage plan associated with the usage data.
  , startDate :: Core.Text
    -- ^ [Required] The starting date (e.g., 2016-01-01) of the usage data.
  , endDate :: Core.Text
    -- ^ [Required] The ending date (e.g., 2016-12-31) of the usage data.
  , keyId :: Core.Maybe Core.Text
    -- ^ The Id of the API key associated with the resultant usage data.
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsage' value with any optional fields omitted.
mkGetUsage
    :: Core.Text -- ^ 'usagePlanId'
    -> Core.Text -- ^ 'startDate'
    -> Core.Text -- ^ 'endDate'
    -> GetUsage
mkGetUsage usagePlanId startDate endDate
  = GetUsage'{usagePlanId, startDate, endDate, keyId = Core.Nothing,
              limit = Core.Nothing, position = Core.Nothing}

-- | [Required] The Id of the usage plan associated with the usage data.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guUsagePlanId :: Lens.Lens' GetUsage Core.Text
guUsagePlanId = Lens.field @"usagePlanId"
{-# INLINEABLE guUsagePlanId #-}
{-# DEPRECATED usagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead"  #-}

-- | [Required] The starting date (e.g., 2016-01-01) of the usage data.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guStartDate :: Lens.Lens' GetUsage Core.Text
guStartDate = Lens.field @"startDate"
{-# INLINEABLE guStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

-- | [Required] The ending date (e.g., 2016-12-31) of the usage data.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guEndDate :: Lens.Lens' GetUsage Core.Text
guEndDate = Lens.field @"endDate"
{-# INLINEABLE guEndDate #-}
{-# DEPRECATED endDate "Use generic-lens or generic-optics with 'endDate' instead"  #-}

-- | The Id of the API key associated with the resultant usage data.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guKeyId :: Lens.Lens' GetUsage (Core.Maybe Core.Text)
guKeyId = Lens.field @"keyId"
{-# INLINEABLE guKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guLimit :: Lens.Lens' GetUsage (Core.Maybe Core.Int)
guLimit = Lens.field @"limit"
{-# INLINEABLE guLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guPosition :: Lens.Lens' GetUsage (Core.Maybe Core.Text)
guPosition = Lens.field @"position"
{-# INLINEABLE guPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetUsage where
        toQuery GetUsage{..}
          = Core.toQueryPair "startDate" startDate Core.<>
              Core.toQueryPair "endDate" endDate
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "keyId") keyId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "limit") limit
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetUsage where
        toHeaders GetUsage{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetUsage where
        type Rs GetUsage = Types.Usage
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/usageplans/" Core.<> Core.toText usagePlanId Core.<> "/usage",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetUsage where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")
