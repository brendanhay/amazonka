{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.RemoveTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified set of tags from the specified Elasticsearch domain.
module Network.AWS.ElasticSearch.RemoveTags
  ( -- * Creating a request
    RemoveTags (..),
    mkRemoveTags,

    -- ** Request lenses
    rtARN,
    rtTagKeys,

    -- * Destructuring the response
    RemoveTagsResponse (..),
    mkRemoveTagsResponse,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'RemoveTags' @ operation. Specify the @ARN@ for the Elasticsearch domain from which you want to remove the specified @TagKey@ .
--
-- /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { arn :: Lude.Text,
    tagKeys :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTags' with the minimum fields required to make a request.
--
-- * 'arn' - Specifies the @ARN@ for the Elasticsearch domain from which you want to delete the specified tags.
-- * 'tagKeys' - Specifies the @TagKey@ list which you want to remove from the Elasticsearch domain.
mkRemoveTags ::
  -- | 'arn'
  Lude.Text ->
  RemoveTags
mkRemoveTags pARN_ =
  RemoveTags' {arn = pARN_, tagKeys = Lude.mempty}

-- | Specifies the @ARN@ for the Elasticsearch domain from which you want to delete the specified tags.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtARN :: Lens.Lens' RemoveTags Lude.Text
rtARN = Lens.lens (arn :: RemoveTags -> Lude.Text) (\s a -> s {arn = a} :: RemoveTags)
{-# DEPRECATED rtARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Specifies the @TagKey@ list which you want to remove from the Elasticsearch domain.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTagKeys :: Lens.Lens' RemoveTags [Lude.Text]
rtTagKeys = Lens.lens (tagKeys :: RemoveTags -> [Lude.Text]) (\s a -> s {tagKeys = a} :: RemoveTags)
{-# DEPRECATED rtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest RemoveTags where
  type Rs RemoveTags = RemoveTagsResponse
  request = Req.postJSON elasticSearchService
  response = Res.receiveNull RemoveTagsResponse'

instance Lude.ToHeaders RemoveTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RemoveTags where
  toJSON RemoveTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ARN" Lude..= arn),
            Lude.Just ("TagKeys" Lude..= tagKeys)
          ]
      )

instance Lude.ToPath RemoveTags where
  toPath = Lude.const "/2015-01-01/tags-removal"

instance Lude.ToQuery RemoveTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveTagsResponse' smart constructor.
data RemoveTagsResponse = RemoveTagsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsResponse' with the minimum fields required to make a request.
mkRemoveTagsResponse ::
  RemoveTagsResponse
mkRemoveTagsResponse = RemoveTagsResponse'
