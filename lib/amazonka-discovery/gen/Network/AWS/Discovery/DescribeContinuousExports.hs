{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeContinuousExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists exports as specified by ID. All continuous exports associated with your user account can be listed if you call @DescribeContinuousExports@ as is without passing any parameters.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeContinuousExports
  ( -- * Creating a request
    DescribeContinuousExports (..),
    mkDescribeContinuousExports,

    -- ** Request lenses
    dceNextToken,
    dceExportIds,
    dceMaxResults,

    -- * Destructuring the response
    DescribeContinuousExportsResponse (..),
    mkDescribeContinuousExportsResponse,

    -- ** Response lenses
    dcersNextToken,
    dcersDescriptions,
    dcersResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeContinuousExports' smart constructor.
data DescribeContinuousExports = DescribeContinuousExports'
  { nextToken ::
      Lude.Maybe Lude.Text,
    exportIds :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'DescribeContinuousExports' with the minimum fields required to make a request.
--
-- * 'exportIds' - The unique IDs assigned to the exports.
-- * 'maxResults' - A number between 1 and 100 specifying the maximum number of continuous export descriptions returned.
-- * 'nextToken' - The token from the previous call to @DescribeExportTasks@ .
mkDescribeContinuousExports ::
  DescribeContinuousExports
mkDescribeContinuousExports =
  DescribeContinuousExports'
    { nextToken = Lude.Nothing,
      exportIds = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token from the previous call to @DescribeExportTasks@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceNextToken :: Lens.Lens' DescribeContinuousExports (Lude.Maybe Lude.Text)
dceNextToken = Lens.lens (nextToken :: DescribeContinuousExports -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeContinuousExports)
{-# DEPRECATED dceNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique IDs assigned to the exports.
--
-- /Note:/ Consider using 'exportIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceExportIds :: Lens.Lens' DescribeContinuousExports (Lude.Maybe [Lude.Text])
dceExportIds = Lens.lens (exportIds :: DescribeContinuousExports -> Lude.Maybe [Lude.Text]) (\s a -> s {exportIds = a} :: DescribeContinuousExports)
{-# DEPRECATED dceExportIds "Use generic-lens or generic-optics with 'exportIds' instead." #-}

-- | A number between 1 and 100 specifying the maximum number of continuous export descriptions returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceMaxResults :: Lens.Lens' DescribeContinuousExports (Lude.Maybe Lude.Natural)
dceMaxResults = Lens.lens (maxResults :: DescribeContinuousExports -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeContinuousExports)
{-# DEPRECATED dceMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeContinuousExports where
  page rq rs
    | Page.stop (rs Lens.^. dcersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcersDescriptions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dceNextToken Lens..~ rs Lens.^. dcersNextToken

instance Lude.AWSRequest DescribeContinuousExports where
  type
    Rs DescribeContinuousExports =
      DescribeContinuousExportsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeContinuousExportsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "descriptions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeContinuousExports where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.DescribeContinuousExports" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeContinuousExports where
  toJSON DescribeContinuousExports' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("exportIds" Lude..=) Lude.<$> exportIds,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeContinuousExports where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeContinuousExports where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeContinuousExportsResponse' smart constructor.
data DescribeContinuousExportsResponse = DescribeContinuousExportsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    descriptions ::
      Lude.Maybe
        [ContinuousExportDescription],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeContinuousExportsResponse' with the minimum fields required to make a request.
--
-- * 'descriptions' - A list of continuous export descriptions.
-- * 'nextToken' - The token from the previous call to @DescribeExportTasks@ .
-- * 'responseStatus' - The response status code.
mkDescribeContinuousExportsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeContinuousExportsResponse
mkDescribeContinuousExportsResponse pResponseStatus_ =
  DescribeContinuousExportsResponse'
    { nextToken = Lude.Nothing,
      descriptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token from the previous call to @DescribeExportTasks@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcersNextToken :: Lens.Lens' DescribeContinuousExportsResponse (Lude.Maybe Lude.Text)
dcersNextToken = Lens.lens (nextToken :: DescribeContinuousExportsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeContinuousExportsResponse)
{-# DEPRECATED dcersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of continuous export descriptions.
--
-- /Note:/ Consider using 'descriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcersDescriptions :: Lens.Lens' DescribeContinuousExportsResponse (Lude.Maybe [ContinuousExportDescription])
dcersDescriptions = Lens.lens (descriptions :: DescribeContinuousExportsResponse -> Lude.Maybe [ContinuousExportDescription]) (\s a -> s {descriptions = a} :: DescribeContinuousExportsResponse)
{-# DEPRECATED dcersDescriptions "Use generic-lens or generic-optics with 'descriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcersResponseStatus :: Lens.Lens' DescribeContinuousExportsResponse Lude.Int
dcersResponseStatus = Lens.lens (responseStatus :: DescribeContinuousExportsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeContinuousExportsResponse)
{-# DEPRECATED dcersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
