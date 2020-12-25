{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBuiltinSlotTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of built-in slot types that meet the specified criteria.
--
-- For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
-- This operation requires permission for the @lex:GetBuiltInSlotTypes@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBuiltinSlotTypes
  ( -- * Creating a request
    GetBuiltinSlotTypes (..),
    mkGetBuiltinSlotTypes,

    -- ** Request lenses
    gbstLocale,
    gbstMaxResults,
    gbstNextToken,
    gbstSignatureContains,

    -- * Destructuring the response
    GetBuiltinSlotTypesResponse (..),
    mkGetBuiltinSlotTypesResponse,

    -- ** Response lenses
    gbstrrsNextToken,
    gbstrrsSlotTypes,
    gbstrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBuiltinSlotTypes' smart constructor.
data GetBuiltinSlotTypes = GetBuiltinSlotTypes'
  { -- | A list of locales that the slot type supports.
    locale :: Core.Maybe Types.Locale,
    -- | The maximum number of slot types to return in the response. The default is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of slot types, specify the pagination token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Substring to match in built-in slot type signatures. A slot type will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
    signatureContains :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBuiltinSlotTypes' value with any optional fields omitted.
mkGetBuiltinSlotTypes ::
  GetBuiltinSlotTypes
mkGetBuiltinSlotTypes =
  GetBuiltinSlotTypes'
    { locale = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      signatureContains = Core.Nothing
    }

-- | A list of locales that the slot type supports.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstLocale :: Lens.Lens' GetBuiltinSlotTypes (Core.Maybe Types.Locale)
gbstLocale = Lens.field @"locale"
{-# DEPRECATED gbstLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The maximum number of slot types to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstMaxResults :: Lens.Lens' GetBuiltinSlotTypes (Core.Maybe Core.Natural)
gbstMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gbstMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of slot types, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstNextToken :: Lens.Lens' GetBuiltinSlotTypes (Core.Maybe Types.NextToken)
gbstNextToken = Lens.field @"nextToken"
{-# DEPRECATED gbstNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Substring to match in built-in slot type signatures. A slot type will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'signatureContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstSignatureContains :: Lens.Lens' GetBuiltinSlotTypes (Core.Maybe Types.String)
gbstSignatureContains = Lens.field @"signatureContains"
{-# DEPRECATED gbstSignatureContains "Use generic-lens or generic-optics with 'signatureContains' instead." #-}

instance Core.AWSRequest GetBuiltinSlotTypes where
  type Rs GetBuiltinSlotTypes = GetBuiltinSlotTypesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/builtins/slottypes/",
        Core._rqQuery =
          Core.toQueryValue "locale" Core.<$> locale
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "signatureContains" Core.<$> signatureContains),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBuiltinSlotTypesResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "slotTypes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetBuiltinSlotTypes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"slotTypes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetBuiltinSlotTypesResponse' smart constructor.
data GetBuiltinSlotTypesResponse = GetBuiltinSlotTypesResponse'
  { -- | If the response is truncated, the response includes a pagination token that you can use in your next request to fetch the next page of slot types.
    nextToken :: Core.Maybe Types.NextToken,
    -- | An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot type returned.
    slotTypes :: Core.Maybe [Types.BuiltinSlotTypeMetadata],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBuiltinSlotTypesResponse' value with any optional fields omitted.
mkGetBuiltinSlotTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBuiltinSlotTypesResponse
mkGetBuiltinSlotTypesResponse responseStatus =
  GetBuiltinSlotTypesResponse'
    { nextToken = Core.Nothing,
      slotTypes = Core.Nothing,
      responseStatus
    }

-- | If the response is truncated, the response includes a pagination token that you can use in your next request to fetch the next page of slot types.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstrrsNextToken :: Lens.Lens' GetBuiltinSlotTypesResponse (Core.Maybe Types.NextToken)
gbstrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gbstrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @BuiltInSlotTypeMetadata@ objects, one entry for each slot type returned.
--
-- /Note:/ Consider using 'slotTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstrrsSlotTypes :: Lens.Lens' GetBuiltinSlotTypesResponse (Core.Maybe [Types.BuiltinSlotTypeMetadata])
gbstrrsSlotTypes = Lens.field @"slotTypes"
{-# DEPRECATED gbstrrsSlotTypes "Use generic-lens or generic-optics with 'slotTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbstrrsResponseStatus :: Lens.Lens' GetBuiltinSlotTypesResponse Core.Int
gbstrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbstrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
