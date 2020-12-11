{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListInputs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces list of inputs that have been created
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputs
  ( -- * Creating a request
    ListInputs (..),
    mkListInputs,

    -- ** Request lenses
    liNextToken,
    liMaxResults,

    -- * Destructuring the response
    ListInputsResponse (..),
    mkListInputsResponse,

    -- ** Response lenses
    lirsInputs,
    lirsNextToken,
    lirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for ListInputsRequest
--
-- /See:/ 'mkListInputs' smart constructor.
data ListInputs = ListInputs'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInputs' with the minimum fields required to make a request.
--
-- * 'maxResults' - Undocumented field.
-- * 'nextToken' - Undocumented field.
mkListInputs ::
  ListInputs
mkListInputs =
  ListInputs' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListInputs (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListInputs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInputs)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListInputs (Lude.Maybe Lude.Natural)
liMaxResults = Lens.lens (maxResults :: ListInputs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListInputs)
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListInputs where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsInputs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListInputs where
  type Rs ListInputs = ListInputsResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInputsResponse'
            Lude.<$> (x Lude..?> "inputs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInputs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListInputs where
  toPath = Lude.const "/prod/inputs"

instance Lude.ToQuery ListInputs where
  toQuery ListInputs' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Placeholder documentation for ListInputsResponse
--
-- /See:/ 'mkListInputsResponse' smart constructor.
data ListInputsResponse = ListInputsResponse'
  { inputs ::
      Lude.Maybe [Input],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInputsResponse' with the minimum fields required to make a request.
--
-- * 'inputs' - Undocumented field.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkListInputsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInputsResponse
mkListInputsResponse pResponseStatus_ =
  ListInputsResponse'
    { inputs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsInputs :: Lens.Lens' ListInputsResponse (Lude.Maybe [Input])
lirsInputs = Lens.lens (inputs :: ListInputsResponse -> Lude.Maybe [Input]) (\s a -> s {inputs = a} :: ListInputsResponse)
{-# DEPRECATED lirsInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListInputsResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListInputsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInputsResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListInputsResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListInputsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInputsResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
