{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'RemoveTags' @ operation. Specify the @ARN@ for the Elasticsearch domain from which you want to remove the specified @TagKey@ .
--
-- /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | Specifies the @ARN@ for the Elasticsearch domain from which you want to delete the specified tags.
    arn :: Types.ARN,
    -- | Specifies the @TagKey@ list which you want to remove from the Elasticsearch domain.
    tagKeys :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTags' value with any optional fields omitted.
mkRemoveTags ::
  -- | 'arn'
  Types.ARN ->
  RemoveTags
mkRemoveTags arn = RemoveTags' {arn, tagKeys = Core.mempty}

-- | Specifies the @ARN@ for the Elasticsearch domain from which you want to delete the specified tags.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtARN :: Lens.Lens' RemoveTags Types.ARN
rtARN = Lens.field @"arn"
{-# DEPRECATED rtARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Specifies the @TagKey@ list which you want to remove from the Elasticsearch domain.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTagKeys :: Lens.Lens' RemoveTags [Types.String]
rtTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED rtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.FromJSON RemoveTags where
  toJSON RemoveTags {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ARN" Core..= arn),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.AWSRequest RemoveTags where
  type Rs RemoveTags = RemoveTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2015-01-01/tags-removal",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull RemoveTagsResponse'

-- | /See:/ 'mkRemoveTagsResponse' smart constructor.
data RemoveTagsResponse = RemoveTagsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsResponse' value with any optional fields omitted.
mkRemoveTagsResponse ::
  RemoveTagsResponse
mkRemoveTagsResponse = RemoveTagsResponse'
