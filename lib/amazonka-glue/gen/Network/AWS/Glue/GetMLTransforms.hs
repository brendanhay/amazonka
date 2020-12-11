{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetMLTransforms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a sortable, filterable list of existing AWS Glue machine learning transforms. Machine learning transforms are a special type of transform that use machine learning to learn the details of the transformation to be performed by learning from examples provided by humans. These transformations are then saved by AWS Glue, and you can retrieve their metadata by calling @GetMLTransforms@ .
module Network.AWS.Glue.GetMLTransforms
  ( -- * Creating a request
    GetMLTransforms (..),
    mkGetMLTransforms,

    -- ** Request lenses
    gmltNextToken,
    gmltSort,
    gmltFilter,
    gmltMaxResults,

    -- * Destructuring the response
    GetMLTransformsResponse (..),
    mkGetMLTransformsResponse,

    -- ** Response lenses
    gmltsrsNextToken,
    gmltsrsResponseStatus,
    gmltsrsTransforms,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMLTransforms' smart constructor.
data GetMLTransforms = GetMLTransforms'
  { nextToken ::
      Lude.Maybe Lude.Text,
    sort :: Lude.Maybe TransformSortCriteria,
    filter :: Lude.Maybe TransformFilterCriteria,
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

-- | Creates a value of 'GetMLTransforms' with the minimum fields required to make a request.
--
-- * 'filter' - The filter transformation criteria.
-- * 'maxResults' - The maximum number of results to return.
-- * 'nextToken' - A paginated token to offset the results.
-- * 'sort' - The sorting criteria.
mkGetMLTransforms ::
  GetMLTransforms
mkGetMLTransforms =
  GetMLTransforms'
    { nextToken = Lude.Nothing,
      sort = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A paginated token to offset the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltNextToken :: Lens.Lens' GetMLTransforms (Lude.Maybe Lude.Text)
gmltNextToken = Lens.lens (nextToken :: GetMLTransforms -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMLTransforms)
{-# DEPRECATED gmltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sorting criteria.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltSort :: Lens.Lens' GetMLTransforms (Lude.Maybe TransformSortCriteria)
gmltSort = Lens.lens (sort :: GetMLTransforms -> Lude.Maybe TransformSortCriteria) (\s a -> s {sort = a} :: GetMLTransforms)
{-# DEPRECATED gmltSort "Use generic-lens or generic-optics with 'sort' instead." #-}

-- | The filter transformation criteria.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltFilter :: Lens.Lens' GetMLTransforms (Lude.Maybe TransformFilterCriteria)
gmltFilter = Lens.lens (filter :: GetMLTransforms -> Lude.Maybe TransformFilterCriteria) (\s a -> s {filter = a} :: GetMLTransforms)
{-# DEPRECATED gmltFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltMaxResults :: Lens.Lens' GetMLTransforms (Lude.Maybe Lude.Natural)
gmltMaxResults = Lens.lens (maxResults :: GetMLTransforms -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetMLTransforms)
{-# DEPRECATED gmltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest GetMLTransforms where
  type Rs GetMLTransforms = GetMLTransformsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMLTransformsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Transforms" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders GetMLTransforms where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetMLTransforms" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMLTransforms where
  toJSON GetMLTransforms' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Sort" Lude..=) Lude.<$> sort,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetMLTransforms where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMLTransforms where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMLTransformsResponse' smart constructor.
data GetMLTransformsResponse = GetMLTransformsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    transforms :: [MLTransform]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMLTransformsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A pagination token, if more results are available.
-- * 'responseStatus' - The response status code.
-- * 'transforms' - A list of machine learning transforms.
mkGetMLTransformsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMLTransformsResponse
mkGetMLTransformsResponse pResponseStatus_ =
  GetMLTransformsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      transforms = Lude.mempty
    }

-- | A pagination token, if more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltsrsNextToken :: Lens.Lens' GetMLTransformsResponse (Lude.Maybe Lude.Text)
gmltsrsNextToken = Lens.lens (nextToken :: GetMLTransformsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMLTransformsResponse)
{-# DEPRECATED gmltsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltsrsResponseStatus :: Lens.Lens' GetMLTransformsResponse Lude.Int
gmltsrsResponseStatus = Lens.lens (responseStatus :: GetMLTransformsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMLTransformsResponse)
{-# DEPRECATED gmltsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of machine learning transforms.
--
-- /Note:/ Consider using 'transforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltsrsTransforms :: Lens.Lens' GetMLTransformsResponse [MLTransform]
gmltsrsTransforms = Lens.lens (transforms :: GetMLTransformsResponse -> [MLTransform]) (\s a -> s {transforms = a} :: GetMLTransformsResponse)
{-# DEPRECATED gmltsrsTransforms "Use generic-lens or generic-optics with 'transforms' instead." #-}
