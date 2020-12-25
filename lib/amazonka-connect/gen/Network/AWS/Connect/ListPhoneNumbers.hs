{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListPhoneNumbers (..),
    mkListPhoneNumbers,

    -- ** Request lenses
    lpnInstanceId,
    lpnMaxResults,
    lpnNextToken,
    lpnPhoneNumberCountryCodes,
    lpnPhoneNumberTypes,

    -- * Destructuring the response
    ListPhoneNumbersResponse (..),
    mkListPhoneNumbersResponse,

    -- ** Response lenses
    lpnrrsNextToken,
    lpnrrsPhoneNumberSummaryList,
    lpnrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPhoneNumbers' smart constructor.
data ListPhoneNumbers = ListPhoneNumbers'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The ISO country code.
    phoneNumberCountryCodes :: Core.Maybe [Types.PhoneNumberCountryCode],
    -- | The type of phone number.
    phoneNumberTypes :: Core.Maybe [Types.PhoneNumberType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPhoneNumbers' value with any optional fields omitted.
mkListPhoneNumbers ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListPhoneNumbers
mkListPhoneNumbers instanceId =
  ListPhoneNumbers'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      phoneNumberCountryCodes = Core.Nothing,
      phoneNumberTypes = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnInstanceId :: Lens.Lens' ListPhoneNumbers Types.InstanceId
lpnInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lpnInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnMaxResults :: Lens.Lens' ListPhoneNumbers (Core.Maybe Core.Natural)
lpnMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lpnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnNextToken :: Lens.Lens' ListPhoneNumbers (Core.Maybe Types.NextToken)
lpnNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ISO country code.
--
-- /Note:/ Consider using 'phoneNumberCountryCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnPhoneNumberCountryCodes :: Lens.Lens' ListPhoneNumbers (Core.Maybe [Types.PhoneNumberCountryCode])
lpnPhoneNumberCountryCodes = Lens.field @"phoneNumberCountryCodes"
{-# DEPRECATED lpnPhoneNumberCountryCodes "Use generic-lens or generic-optics with 'phoneNumberCountryCodes' instead." #-}

-- | The type of phone number.
--
-- /Note:/ Consider using 'phoneNumberTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnPhoneNumberTypes :: Lens.Lens' ListPhoneNumbers (Core.Maybe [Types.PhoneNumberType])
lpnPhoneNumberTypes = Lens.field @"phoneNumberTypes"
{-# DEPRECATED lpnPhoneNumberTypes "Use generic-lens or generic-optics with 'phoneNumberTypes' instead." #-}

instance Core.AWSRequest ListPhoneNumbers where
  type Rs ListPhoneNumbers = ListPhoneNumbersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/phone-numbers-summary/" Core.<> (Core.toText instanceId)),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> ( Core.toQueryValue
                        "phoneNumberCountryCodes"
                        (Core.toQueryList "member" Core.<$> phoneNumberCountryCodes)
                    )
            Core.<> ( Core.toQueryValue
                        "phoneNumberTypes"
                        (Core.toQueryList "member" Core.<$> phoneNumberTypes)
                    ),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPhoneNumbersResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "PhoneNumberSummaryList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPhoneNumbers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"phoneNumberSummaryList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListPhoneNumbersResponse' smart constructor.
data ListPhoneNumbersResponse = ListPhoneNumbersResponse'
  { -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the phone numbers.
    phoneNumberSummaryList :: Core.Maybe [Types.PhoneNumberSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPhoneNumbersResponse' value with any optional fields omitted.
mkListPhoneNumbersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPhoneNumbersResponse
mkListPhoneNumbersResponse responseStatus =
  ListPhoneNumbersResponse'
    { nextToken = Core.Nothing,
      phoneNumberSummaryList = Core.Nothing,
      responseStatus
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnrrsNextToken :: Lens.Lens' ListPhoneNumbersResponse (Core.Maybe Types.NextToken)
lpnrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpnrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the phone numbers.
--
-- /Note:/ Consider using 'phoneNumberSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnrrsPhoneNumberSummaryList :: Lens.Lens' ListPhoneNumbersResponse (Core.Maybe [Types.PhoneNumberSummary])
lpnrrsPhoneNumberSummaryList = Lens.field @"phoneNumberSummaryList"
{-# DEPRECATED lpnrrsPhoneNumberSummaryList "Use generic-lens or generic-optics with 'phoneNumberSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnrrsResponseStatus :: Lens.Lens' ListPhoneNumbersResponse Core.Int
lpnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
