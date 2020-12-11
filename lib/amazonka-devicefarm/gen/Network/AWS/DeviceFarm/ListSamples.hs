{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListSamples
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about samples, given an AWS Device Farm job ARN.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListSamples
  ( -- * Creating a request
    ListSamples (..),
    mkListSamples,

    -- ** Request lenses
    lsNextToken,
    lsArn,

    -- * Destructuring the response
    ListSamplesResponse (..),
    mkListSamplesResponse,

    -- ** Response lenses
    lssrsNextToken,
    lssrsSamples,
    lssrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the list samples operation.
--
-- /See:/ 'mkListSamples' smart constructor.
data ListSamples = ListSamples'
  { nextToken :: Lude.Maybe Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSamples' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the job used to list samples.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListSamples ::
  -- | 'arn'
  Lude.Text ->
  ListSamples
mkListSamples pArn_ =
  ListSamples' {nextToken = Lude.Nothing, arn = pArn_}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListSamples (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListSamples -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSamples)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the job used to list samples.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsArn :: Lens.Lens' ListSamples Lude.Text
lsArn = Lens.lens (arn :: ListSamples -> Lude.Text) (\s a -> s {arn = a} :: ListSamples)
{-# DEPRECATED lsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Page.AWSPager ListSamples where
  page rq rs
    | Page.stop (rs Lens.^. lssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lssrsSamples) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lssrsNextToken

instance Lude.AWSRequest ListSamples where
  type Rs ListSamples = ListSamplesResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSamplesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "samples" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSamples where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListSamples" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSamples where
  toJSON ListSamples' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath ListSamples where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSamples where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a list samples request.
--
-- /See:/ 'mkListSamplesResponse' smart constructor.
data ListSamplesResponse = ListSamplesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    samples :: Lude.Maybe [Sample],
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

-- | Creates a value of 'ListSamplesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
-- * 'samples' - Information about the samples.
mkListSamplesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSamplesResponse
mkListSamplesResponse pResponseStatus_ =
  ListSamplesResponse'
    { nextToken = Lude.Nothing,
      samples = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssrsNextToken :: Lens.Lens' ListSamplesResponse (Lude.Maybe Lude.Text)
lssrsNextToken = Lens.lens (nextToken :: ListSamplesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSamplesResponse)
{-# DEPRECATED lssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the samples.
--
-- /Note:/ Consider using 'samples' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssrsSamples :: Lens.Lens' ListSamplesResponse (Lude.Maybe [Sample])
lssrsSamples = Lens.lens (samples :: ListSamplesResponse -> Lude.Maybe [Sample]) (\s a -> s {samples = a} :: ListSamplesResponse)
{-# DEPRECATED lssrsSamples "Use generic-lens or generic-optics with 'samples' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssrsResponseStatus :: Lens.Lens' ListSamplesResponse Lude.Int
lssrsResponseStatus = Lens.lens (responseStatus :: ListSamplesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSamplesResponse)
{-# DEPRECATED lssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
