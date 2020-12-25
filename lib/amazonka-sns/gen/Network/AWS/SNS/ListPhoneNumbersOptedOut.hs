{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListPhoneNumbersOptedOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of phone numbers that are opted out, meaning you cannot send SMS messages to them.
--
-- The results for @ListPhoneNumbersOptedOut@ are paginated, and each page returns up to 100 phone numbers. If additional phone numbers are available after the first page of results, then a @NextToken@ string will be returned. To receive the next page, you call @ListPhoneNumbersOptedOut@ again using the @NextToken@ string received from the previous call. When there are no more records to return, @NextToken@ will be null.
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListPhoneNumbersOptedOut
  ( -- * Creating a request
    ListPhoneNumbersOptedOut (..),
    mkListPhoneNumbersOptedOut,

    -- ** Request lenses
    lpnooNextToken,

    -- * Destructuring the response
    ListPhoneNumbersOptedOutResponse (..),
    mkListPhoneNumbersOptedOutResponse,

    -- ** Response lenses
    lpnoorrsNextToken,
    lpnoorrsPhoneNumbers,
    lpnoorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | The input for the @ListPhoneNumbersOptedOut@ action.
--
-- /See:/ 'mkListPhoneNumbersOptedOut' smart constructor.
newtype ListPhoneNumbersOptedOut = ListPhoneNumbersOptedOut'
  { -- | A @NextToken@ string is used when you call the @ListPhoneNumbersOptedOut@ action to retrieve additional records that are available after the first page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListPhoneNumbersOptedOut' value with any optional fields omitted.
mkListPhoneNumbersOptedOut ::
  ListPhoneNumbersOptedOut
mkListPhoneNumbersOptedOut =
  ListPhoneNumbersOptedOut' {nextToken = Core.Nothing}

-- | A @NextToken@ string is used when you call the @ListPhoneNumbersOptedOut@ action to retrieve additional records that are available after the first page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnooNextToken :: Lens.Lens' ListPhoneNumbersOptedOut (Core.Maybe Types.String)
lpnooNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpnooNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListPhoneNumbersOptedOut where
  type Rs ListPhoneNumbersOptedOut = ListPhoneNumbersOptedOutResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListPhoneNumbersOptedOut")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListPhoneNumbersOptedOutResult"
      ( \s h x ->
          ListPhoneNumbersOptedOutResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "phoneNumbers" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPhoneNumbersOptedOut where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"phoneNumbers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | The response from the @ListPhoneNumbersOptedOut@ action.
--
-- /See:/ 'mkListPhoneNumbersOptedOutResponse' smart constructor.
data ListPhoneNumbersOptedOutResponse = ListPhoneNumbersOptedOutResponse'
  { -- | A @NextToken@ string is returned when you call the @ListPhoneNumbersOptedOut@ action if additional records are available after the first page of results.
    nextToken :: Core.Maybe Types.String,
    -- | A list of phone numbers that are opted out of receiving SMS messages. The list is paginated, and each page can contain up to 100 phone numbers.
    phoneNumbers :: Core.Maybe [Types.PhoneNumber],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPhoneNumbersOptedOutResponse' value with any optional fields omitted.
mkListPhoneNumbersOptedOutResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPhoneNumbersOptedOutResponse
mkListPhoneNumbersOptedOutResponse responseStatus =
  ListPhoneNumbersOptedOutResponse'
    { nextToken = Core.Nothing,
      phoneNumbers = Core.Nothing,
      responseStatus
    }

-- | A @NextToken@ string is returned when you call the @ListPhoneNumbersOptedOut@ action if additional records are available after the first page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnoorrsNextToken :: Lens.Lens' ListPhoneNumbersOptedOutResponse (Core.Maybe Types.String)
lpnoorrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpnoorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of phone numbers that are opted out of receiving SMS messages. The list is paginated, and each page can contain up to 100 phone numbers.
--
-- /Note:/ Consider using 'phoneNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnoorrsPhoneNumbers :: Lens.Lens' ListPhoneNumbersOptedOutResponse (Core.Maybe [Types.PhoneNumber])
lpnoorrsPhoneNumbers = Lens.field @"phoneNumbers"
{-# DEPRECATED lpnoorrsPhoneNumbers "Use generic-lens or generic-optics with 'phoneNumbers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnoorrsResponseStatus :: Lens.Lens' ListPhoneNumbersOptedOutResponse Core.Int
lpnoorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpnoorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
