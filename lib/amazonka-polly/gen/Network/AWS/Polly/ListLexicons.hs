{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.ListLexicons
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of pronunciation lexicons stored in an AWS Region. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
--
-- This operation returns paginated results.
module Network.AWS.Polly.ListLexicons
  ( -- * Creating a request
    ListLexicons (..),
    mkListLexicons,

    -- ** Request lenses
    llNextToken,

    -- * Destructuring the response
    ListLexiconsResponse (..),
    mkListLexiconsResponse,

    -- ** Response lenses
    llrrsLexicons,
    llrrsNextToken,
    llrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Polly.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLexicons' smart constructor.
newtype ListLexicons = ListLexicons'
  { -- | An opaque pagination token returned from previous @ListLexicons@ operation. If present, indicates where to continue the list of lexicons.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListLexicons' value with any optional fields omitted.
mkListLexicons ::
  ListLexicons
mkListLexicons = ListLexicons' {nextToken = Core.Nothing}

-- | An opaque pagination token returned from previous @ListLexicons@ operation. If present, indicates where to continue the list of lexicons.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llNextToken :: Lens.Lens' ListLexicons (Core.Maybe Types.NextToken)
llNextToken = Lens.field @"nextToken"
{-# DEPRECATED llNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListLexicons where
  type Rs ListLexicons = ListLexiconsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/v1/lexicons",
        Core._rqQuery = Core.toQueryValue "NextToken" Core.<$> nextToken,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLexiconsResponse'
            Core.<$> (x Core..:? "Lexicons")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLexicons where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"lexicons" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListLexiconsResponse' smart constructor.
data ListLexiconsResponse = ListLexiconsResponse'
  { -- | A list of lexicon names and attributes.
    lexicons :: Core.Maybe [Types.LexiconDescription],
    -- | The pagination token to use in the next request to continue the listing of lexicons. @NextToken@ is returned only if the response is truncated.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListLexiconsResponse' value with any optional fields omitted.
mkListLexiconsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLexiconsResponse
mkListLexiconsResponse responseStatus =
  ListLexiconsResponse'
    { lexicons = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of lexicon names and attributes.
--
-- /Note:/ Consider using 'lexicons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrrsLexicons :: Lens.Lens' ListLexiconsResponse (Core.Maybe [Types.LexiconDescription])
llrrsLexicons = Lens.field @"lexicons"
{-# DEPRECATED llrrsLexicons "Use generic-lens or generic-optics with 'lexicons' instead." #-}

-- | The pagination token to use in the next request to continue the listing of lexicons. @NextToken@ is returned only if the response is truncated.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrrsNextToken :: Lens.Lens' ListLexiconsResponse (Core.Maybe Types.NextToken)
llrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED llrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrrsResponseStatus :: Lens.Lens' ListLexiconsResponse Core.Int
llrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED llrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
