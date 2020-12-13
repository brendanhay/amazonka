{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tags for the given Elasticsearch domain.
module Network.AWS.ElasticSearch.ListTags
  ( -- * Creating a request
    ListTags (..),
    mkListTags,

    -- ** Request lenses
    ltARN,

    -- * Destructuring the response
    ListTagsResponse (..),
    mkListTagsResponse,

    -- ** Response lenses
    ltrsTagList,
    ltrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'ListTags' @ operation. Specify the @ARN@ for the Elasticsearch domain to which the tags are attached that you want to view are attached.
--
-- /See:/ 'mkListTags' smart constructor.
newtype ListTags = ListTags'
  { -- | Specify the @ARN@ for the Elasticsearch domain to which the tags are attached that you want to view.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- * 'arn' - Specify the @ARN@ for the Elasticsearch domain to which the tags are attached that you want to view.
mkListTags ::
  -- | 'arn'
  Lude.Text ->
  ListTags
mkListTags pARN_ = ListTags' {arn = pARN_}

-- | Specify the @ARN@ for the Elasticsearch domain to which the tags are attached that you want to view.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltARN :: Lens.Lens' ListTags Lude.Text
ltARN = Lens.lens (arn :: ListTags -> Lude.Text) (\s a -> s {arn = a} :: ListTags)
{-# DEPRECATED ltARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest ListTags where
  type Rs ListTags = ListTagsResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Lude.<$> (x Lude..?> "TagList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTags where
  toPath = Lude.const "/2015-01-01/tags/"

instance Lude.ToQuery ListTags where
  toQuery ListTags' {..} = Lude.mconcat ["arn" Lude.=: arn]

-- | The result of a @ListTags@ operation. Contains tags for all requested Elasticsearch domains.
--
-- /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | List of @Tag@ for the requested Elasticsearch domain.
    tagList :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- * 'tagList' - List of @Tag@ for the requested Elasticsearch domain.
-- * 'responseStatus' - The response status code.
mkListTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsResponse
mkListTagsResponse pResponseStatus_ =
  ListTagsResponse'
    { tagList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of @Tag@ for the requested Elasticsearch domain.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTagList :: Lens.Lens' ListTagsResponse (Lude.Maybe [Tag])
ltrsTagList = Lens.lens (tagList :: ListTagsResponse -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: ListTagsResponse)
{-# DEPRECATED ltrsTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTagsResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
