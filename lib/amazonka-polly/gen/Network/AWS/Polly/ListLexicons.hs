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
    llrsLexicons,
    llrsNextToken,
    llrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListLexicons' smart constructor.
newtype ListLexicons = ListLexicons'
  { -- | An opaque pagination token returned from previous @ListLexicons@ operation. If present, indicates where to continue the list of lexicons.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLexicons' with the minimum fields required to make a request.
--
-- * 'nextToken' - An opaque pagination token returned from previous @ListLexicons@ operation. If present, indicates where to continue the list of lexicons.
mkListLexicons ::
  ListLexicons
mkListLexicons = ListLexicons' {nextToken = Lude.Nothing}

-- | An opaque pagination token returned from previous @ListLexicons@ operation. If present, indicates where to continue the list of lexicons.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llNextToken :: Lens.Lens' ListLexicons (Lude.Maybe Lude.Text)
llNextToken = Lens.lens (nextToken :: ListLexicons -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLexicons)
{-# DEPRECATED llNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListLexicons where
  page rq rs
    | Page.stop (rs Lens.^. llrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. llrsLexicons) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& llNextToken Lens..~ rs Lens.^. llrsNextToken

instance Lude.AWSRequest ListLexicons where
  type Rs ListLexicons = ListLexiconsResponse
  request = Req.get pollyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLexiconsResponse'
            Lude.<$> (x Lude..?> "Lexicons" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLexicons where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListLexicons where
  toPath = Lude.const "/v1/lexicons"

instance Lude.ToQuery ListLexicons where
  toQuery ListLexicons' {..} =
    Lude.mconcat ["NextToken" Lude.=: nextToken]

-- | /See:/ 'mkListLexiconsResponse' smart constructor.
data ListLexiconsResponse = ListLexiconsResponse'
  { -- | A list of lexicon names and attributes.
    lexicons :: Lude.Maybe [LexiconDescription],
    -- | The pagination token to use in the next request to continue the listing of lexicons. @NextToken@ is returned only if the response is truncated.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLexiconsResponse' with the minimum fields required to make a request.
--
-- * 'lexicons' - A list of lexicon names and attributes.
-- * 'nextToken' - The pagination token to use in the next request to continue the listing of lexicons. @NextToken@ is returned only if the response is truncated.
-- * 'responseStatus' - The response status code.
mkListLexiconsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLexiconsResponse
mkListLexiconsResponse pResponseStatus_ =
  ListLexiconsResponse'
    { lexicons = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of lexicon names and attributes.
--
-- /Note:/ Consider using 'lexicons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrsLexicons :: Lens.Lens' ListLexiconsResponse (Lude.Maybe [LexiconDescription])
llrsLexicons = Lens.lens (lexicons :: ListLexiconsResponse -> Lude.Maybe [LexiconDescription]) (\s a -> s {lexicons = a} :: ListLexiconsResponse)
{-# DEPRECATED llrsLexicons "Use generic-lens or generic-optics with 'lexicons' instead." #-}

-- | The pagination token to use in the next request to continue the listing of lexicons. @NextToken@ is returned only if the response is truncated.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrsNextToken :: Lens.Lens' ListLexiconsResponse (Lude.Maybe Lude.Text)
llrsNextToken = Lens.lens (nextToken :: ListLexiconsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLexiconsResponse)
{-# DEPRECATED llrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrsResponseStatus :: Lens.Lens' ListLexiconsResponse Lude.Int
llrsResponseStatus = Lens.lens (responseStatus :: ListLexiconsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLexiconsResponse)
{-# DEPRECATED llrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
