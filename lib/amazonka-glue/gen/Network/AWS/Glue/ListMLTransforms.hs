{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListMLTransforms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a sortable, filterable list of existing AWS Glue machine learning transforms in this AWS account, or the resources with the specified tag. This operation takes the optional @Tags@ field, which you can use as a filter of the responses so that tagged resources can be retrieved as a group. If you choose to use tag filtering, only resources with the tags are retrieved.
module Network.AWS.Glue.ListMLTransforms
  ( -- * Creating a request
    ListMLTransforms (..),
    mkListMLTransforms,

    -- ** Request lenses
    lmltNextToken,
    lmltSort,
    lmltFilter,
    lmltMaxResults,
    lmltTags,

    -- * Destructuring the response
    ListMLTransformsResponse (..),
    mkListMLTransformsResponse,

    -- ** Response lenses
    lmltrsNextToken,
    lmltrsResponseStatus,
    lmltrsTransformIds,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListMLTransforms' smart constructor.
data ListMLTransforms = ListMLTransforms'
  { nextToken ::
      Lude.Maybe Lude.Text,
    sort :: Lude.Maybe TransformSortCriteria,
    filter :: Lude.Maybe TransformFilterCriteria,
    maxResults :: Lude.Maybe Lude.Natural,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMLTransforms' with the minimum fields required to make a request.
--
-- * 'filter' - A @TransformFilterCriteria@ used to filter the machine learning transforms.
-- * 'maxResults' - The maximum size of a list to return.
-- * 'nextToken' - A continuation token, if this is a continuation request.
-- * 'sort' - A @TransformSortCriteria@ used to sort the machine learning transforms.
-- * 'tags' - Specifies to return only these tagged resources.
mkListMLTransforms ::
  ListMLTransforms
mkListMLTransforms =
  ListMLTransforms'
    { nextToken = Lude.Nothing,
      sort = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltNextToken :: Lens.Lens' ListMLTransforms (Lude.Maybe Lude.Text)
lmltNextToken = Lens.lens (nextToken :: ListMLTransforms -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMLTransforms)
{-# DEPRECATED lmltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A @TransformSortCriteria@ used to sort the machine learning transforms.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltSort :: Lens.Lens' ListMLTransforms (Lude.Maybe TransformSortCriteria)
lmltSort = Lens.lens (sort :: ListMLTransforms -> Lude.Maybe TransformSortCriteria) (\s a -> s {sort = a} :: ListMLTransforms)
{-# DEPRECATED lmltSort "Use generic-lens or generic-optics with 'sort' instead." #-}

-- | A @TransformFilterCriteria@ used to filter the machine learning transforms.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltFilter :: Lens.Lens' ListMLTransforms (Lude.Maybe TransformFilterCriteria)
lmltFilter = Lens.lens (filter :: ListMLTransforms -> Lude.Maybe TransformFilterCriteria) (\s a -> s {filter = a} :: ListMLTransforms)
{-# DEPRECATED lmltFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltMaxResults :: Lens.Lens' ListMLTransforms (Lude.Maybe Lude.Natural)
lmltMaxResults = Lens.lens (maxResults :: ListMLTransforms -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMLTransforms)
{-# DEPRECATED lmltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltTags :: Lens.Lens' ListMLTransforms (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
lmltTags = Lens.lens (tags :: ListMLTransforms -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListMLTransforms)
{-# DEPRECATED lmltTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest ListMLTransforms where
  type Rs ListMLTransforms = ListMLTransformsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMLTransformsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "TransformIds" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListMLTransforms where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.ListMLTransforms" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListMLTransforms where
  toJSON ListMLTransforms' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Sort" Lude..=) Lude.<$> sort,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath ListMLTransforms where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMLTransforms where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListMLTransformsResponse' smart constructor.
data ListMLTransformsResponse = ListMLTransformsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    transformIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMLTransformsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if the returned list does not contain the last metric available.
-- * 'responseStatus' - The response status code.
-- * 'transformIds' - The identifiers of all the machine learning transforms in the account, or the machine learning transforms with the specified tags.
mkListMLTransformsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMLTransformsResponse
mkListMLTransformsResponse pResponseStatus_ =
  ListMLTransformsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      transformIds = Lude.mempty
    }

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltrsNextToken :: Lens.Lens' ListMLTransformsResponse (Lude.Maybe Lude.Text)
lmltrsNextToken = Lens.lens (nextToken :: ListMLTransformsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMLTransformsResponse)
{-# DEPRECATED lmltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltrsResponseStatus :: Lens.Lens' ListMLTransformsResponse Lude.Int
lmltrsResponseStatus = Lens.lens (responseStatus :: ListMLTransformsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMLTransformsResponse)
{-# DEPRECATED lmltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The identifiers of all the machine learning transforms in the account, or the machine learning transforms with the specified tags.
--
-- /Note:/ Consider using 'transformIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltrsTransformIds :: Lens.Lens' ListMLTransformsResponse [Lude.Text]
lmltrsTransformIds = Lens.lens (transformIds :: ListMLTransformsResponse -> [Lude.Text]) (\s a -> s {transformIds = a} :: ListMLTransformsResponse)
{-# DEPRECATED lmltrsTransformIds "Use generic-lens or generic-optics with 'transformIds' instead." #-}
