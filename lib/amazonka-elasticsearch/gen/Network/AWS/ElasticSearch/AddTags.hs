{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches tags to an existing Elasticsearch domain. Tags are a set of case-sensitive key value pairs. An Elasticsearch domain may have up to 10 tags. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-awsresorcetagging Tagging Amazon Elasticsearch Service Domains for more information.>
module Network.AWS.ElasticSearch.AddTags
  ( -- * Creating a request
    AddTags (..),
    mkAddTags,

    -- ** Request lenses
    atARN,
    atTagList,

    -- * Destructuring the response
    AddTagsResponse (..),
    mkAddTagsResponse,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'AddTags' @ operation. Specify the tags that you want to attach to the Elasticsearch domain.
--
-- /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { -- | Specify the @ARN@ for which you want to add the tags.
    arn :: Lude.Text,
    -- | List of @Tag@ that need to be added for the Elasticsearch domain.
    tagList :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTags' with the minimum fields required to make a request.
--
-- * 'arn' - Specify the @ARN@ for which you want to add the tags.
-- * 'tagList' - List of @Tag@ that need to be added for the Elasticsearch domain.
mkAddTags ::
  -- | 'arn'
  Lude.Text ->
  AddTags
mkAddTags pARN_ = AddTags' {arn = pARN_, tagList = Lude.mempty}

-- | Specify the @ARN@ for which you want to add the tags.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atARN :: Lens.Lens' AddTags Lude.Text
atARN = Lens.lens (arn :: AddTags -> Lude.Text) (\s a -> s {arn = a} :: AddTags)
{-# DEPRECATED atARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | List of @Tag@ that need to be added for the Elasticsearch domain.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTagList :: Lens.Lens' AddTags [Tag]
atTagList = Lens.lens (tagList :: AddTags -> [Tag]) (\s a -> s {tagList = a} :: AddTags)
{-# DEPRECATED atTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

instance Lude.AWSRequest AddTags where
  type Rs AddTags = AddTagsResponse
  request = Req.postJSON elasticSearchService
  response = Res.receiveNull AddTagsResponse'

instance Lude.ToHeaders AddTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AddTags where
  toJSON AddTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ARN" Lude..= arn),
            Lude.Just ("TagList" Lude..= tagList)
          ]
      )

instance Lude.ToPath AddTags where
  toPath = Lude.const "/2015-01-01/tags"

instance Lude.ToQuery AddTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsResponse' with the minimum fields required to make a request.
mkAddTagsResponse ::
  AddTagsResponse
mkAddTagsResponse = AddTagsResponse'
