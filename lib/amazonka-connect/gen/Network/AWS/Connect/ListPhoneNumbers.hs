{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListPhoneNumbers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the phone numbers for the specified Amazon Connect instance. 
--
-- For more information about phone numbers, see <https://docs.aws.amazon.com/connect/latest/adminguide/contact-center-phone-number.html Set Up Phone Numbers for Your Contact Center> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListPhoneNumbers
    (
    -- * Creating a request
      ListPhoneNumbers (..)
    , mkListPhoneNumbers
    -- ** Request lenses
    , lpnInstanceId
    , lpnMaxResults
    , lpnNextToken
    , lpnPhoneNumberCountryCodes
    , lpnPhoneNumberTypes

    -- * Destructuring the response
    , ListPhoneNumbersResponse (..)
    , mkListPhoneNumbersResponse
    -- ** Response lenses
    , lpnrrsNextToken
    , lpnrrsPhoneNumberSummaryList
    , lpnrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPhoneNumbers' smart constructor.
data ListPhoneNumbers = ListPhoneNumbers'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximimum number of results to return per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
  , phoneNumberCountryCodes :: Core.Maybe [Types.PhoneNumberCountryCode]
    -- ^ The ISO country code.
  , phoneNumberTypes :: Core.Maybe [Types.PhoneNumberType]
    -- ^ The type of phone number.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPhoneNumbers' value with any optional fields omitted.
mkListPhoneNumbers
    :: Types.InstanceId -- ^ 'instanceId'
    -> ListPhoneNumbers
mkListPhoneNumbers instanceId
  = ListPhoneNumbers'{instanceId, maxResults = Core.Nothing,
                      nextToken = Core.Nothing, phoneNumberCountryCodes = Core.Nothing,
                      phoneNumberTypes = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnInstanceId :: Lens.Lens' ListPhoneNumbers Types.InstanceId
lpnInstanceId = Lens.field @"instanceId"
{-# INLINEABLE lpnInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnMaxResults :: Lens.Lens' ListPhoneNumbers (Core.Maybe Core.Natural)
lpnMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpnMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnNextToken :: Lens.Lens' ListPhoneNumbers (Core.Maybe Types.NextToken)
lpnNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpnNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ISO country code.
--
-- /Note:/ Consider using 'phoneNumberCountryCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnPhoneNumberCountryCodes :: Lens.Lens' ListPhoneNumbers (Core.Maybe [Types.PhoneNumberCountryCode])
lpnPhoneNumberCountryCodes = Lens.field @"phoneNumberCountryCodes"
{-# INLINEABLE lpnPhoneNumberCountryCodes #-}
{-# DEPRECATED phoneNumberCountryCodes "Use generic-lens or generic-optics with 'phoneNumberCountryCodes' instead"  #-}

-- | The type of phone number.
--
-- /Note:/ Consider using 'phoneNumberTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnPhoneNumberTypes :: Lens.Lens' ListPhoneNumbers (Core.Maybe [Types.PhoneNumberType])
lpnPhoneNumberTypes = Lens.field @"phoneNumberTypes"
{-# INLINEABLE lpnPhoneNumberTypes #-}
{-# DEPRECATED phoneNumberTypes "Use generic-lens or generic-optics with 'phoneNumberTypes' instead"  #-}

instance Core.ToQuery ListPhoneNumbers where
        toQuery ListPhoneNumbers{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.toQueryPair "phoneNumberCountryCodes"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   phoneNumberCountryCodes)
              Core.<>
              Core.toQueryPair "phoneNumberTypes"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   phoneNumberTypes)

instance Core.ToHeaders ListPhoneNumbers where
        toHeaders ListPhoneNumbers{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListPhoneNumbers where
        type Rs ListPhoneNumbers = ListPhoneNumbersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/phone-numbers-summary/" Core.<> Core.toText instanceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPhoneNumbersResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "PhoneNumberSummaryList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPhoneNumbers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"phoneNumberSummaryList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListPhoneNumbersResponse' smart constructor.
data ListPhoneNumbersResponse = ListPhoneNumbersResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If there are additional results, this is the token for the next set of results.
  , phoneNumberSummaryList :: Core.Maybe [Types.PhoneNumberSummary]
    -- ^ Information about the phone numbers.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPhoneNumbersResponse' value with any optional fields omitted.
mkListPhoneNumbersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPhoneNumbersResponse
mkListPhoneNumbersResponse responseStatus
  = ListPhoneNumbersResponse'{nextToken = Core.Nothing,
                              phoneNumberSummaryList = Core.Nothing, responseStatus}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnrrsNextToken :: Lens.Lens' ListPhoneNumbersResponse (Core.Maybe Types.NextToken)
lpnrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpnrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the phone numbers.
--
-- /Note:/ Consider using 'phoneNumberSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnrrsPhoneNumberSummaryList :: Lens.Lens' ListPhoneNumbersResponse (Core.Maybe [Types.PhoneNumberSummary])
lpnrrsPhoneNumberSummaryList = Lens.field @"phoneNumberSummaryList"
{-# INLINEABLE lpnrrsPhoneNumberSummaryList #-}
{-# DEPRECATED phoneNumberSummaryList "Use generic-lens or generic-optics with 'phoneNumberSummaryList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnrrsResponseStatus :: Lens.Lens' ListPhoneNumbersResponse Core.Int
lpnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
