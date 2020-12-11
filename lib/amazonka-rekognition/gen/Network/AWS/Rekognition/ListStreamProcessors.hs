{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.ListStreamProcessors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of stream processors that you have created with 'CreateStreamProcessor' .
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListStreamProcessors
  ( -- * Creating a request
    ListStreamProcessors (..),
    mkListStreamProcessors,

    -- ** Request lenses
    lspNextToken,
    lspMaxResults,

    -- * Destructuring the response
    ListStreamProcessorsResponse (..),
    mkListStreamProcessorsResponse,

    -- ** Response lenses
    lsprsStreamProcessors,
    lsprsNextToken,
    lsprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListStreamProcessors' smart constructor.
data ListStreamProcessors = ListStreamProcessors'
  { nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListStreamProcessors' with the minimum fields required to make a request.
--
-- * 'maxResults' - Maximum number of stream processors you want Amazon Rekognition Video to return in the response. The default is 1000.
-- * 'nextToken' - If the previous response was incomplete (because there are more stream processors to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of stream processors.
mkListStreamProcessors ::
  ListStreamProcessors
mkListStreamProcessors =
  ListStreamProcessors'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If the previous response was incomplete (because there are more stream processors to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of stream processors.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspNextToken :: Lens.Lens' ListStreamProcessors (Lude.Maybe Lude.Text)
lspNextToken = Lens.lens (nextToken :: ListStreamProcessors -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStreamProcessors)
{-# DEPRECATED lspNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of stream processors you want Amazon Rekognition Video to return in the response. The default is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspMaxResults :: Lens.Lens' ListStreamProcessors (Lude.Maybe Lude.Natural)
lspMaxResults = Lens.lens (maxResults :: ListStreamProcessors -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListStreamProcessors)
{-# DEPRECATED lspMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListStreamProcessors where
  page rq rs
    | Page.stop (rs Lens.^. lsprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsprsStreamProcessors) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lspNextToken Lens..~ rs Lens.^. lsprsNextToken

instance Lude.AWSRequest ListStreamProcessors where
  type Rs ListStreamProcessors = ListStreamProcessorsResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListStreamProcessorsResponse'
            Lude.<$> (x Lude..?> "StreamProcessors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStreamProcessors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.ListStreamProcessors" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListStreamProcessors where
  toJSON ListStreamProcessors' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListStreamProcessors where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStreamProcessors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListStreamProcessorsResponse' smart constructor.
data ListStreamProcessorsResponse = ListStreamProcessorsResponse'
  { streamProcessors ::
      Lude.Maybe [StreamProcessor],
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

-- | Creates a value of 'ListStreamProcessorsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of stream processors.
-- * 'responseStatus' - The response status code.
-- * 'streamProcessors' - List of stream processors that you have created.
mkListStreamProcessorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStreamProcessorsResponse
mkListStreamProcessorsResponse pResponseStatus_ =
  ListStreamProcessorsResponse'
    { streamProcessors = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of stream processors that you have created.
--
-- /Note:/ Consider using 'streamProcessors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsStreamProcessors :: Lens.Lens' ListStreamProcessorsResponse (Lude.Maybe [StreamProcessor])
lsprsStreamProcessors = Lens.lens (streamProcessors :: ListStreamProcessorsResponse -> Lude.Maybe [StreamProcessor]) (\s a -> s {streamProcessors = a} :: ListStreamProcessorsResponse)
{-# DEPRECATED lsprsStreamProcessors "Use generic-lens or generic-optics with 'streamProcessors' instead." #-}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of stream processors.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsNextToken :: Lens.Lens' ListStreamProcessorsResponse (Lude.Maybe Lude.Text)
lsprsNextToken = Lens.lens (nextToken :: ListStreamProcessorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStreamProcessorsResponse)
{-# DEPRECATED lsprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsResponseStatus :: Lens.Lens' ListStreamProcessorsResponse Lude.Int
lsprsResponseStatus = Lens.lens (responseStatus :: ListStreamProcessorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStreamProcessorsResponse)
{-# DEPRECATED lsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
