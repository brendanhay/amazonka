{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes tags from a resource. You must provide the ARN of the resource from which you want to delete the tag or tags.
module Network.AWS.Redshift.DeleteTags
  ( -- * Creating a request
    DeleteTags (..),
    mkDeleteTags,

    -- ** Request lenses
    dtsResourceName,
    dtsTagKeys,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the output from the @DeleteTags@ action.
--
-- /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | The Amazon Resource Name (ARN) from which you want to remove the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
    resourceName :: Types.String,
    -- | The tag key that you want to delete.
    tagKeys :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTags' value with any optional fields omitted.
mkDeleteTags ::
  -- | 'resourceName'
  Types.String ->
  DeleteTags
mkDeleteTags resourceName =
  DeleteTags' {resourceName, tagKeys = Core.mempty}

-- | The Amazon Resource Name (ARN) from which you want to remove the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsResourceName :: Lens.Lens' DeleteTags Types.String
dtsResourceName = Lens.field @"resourceName"
{-# DEPRECATED dtsResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The tag key that you want to delete.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsTagKeys :: Lens.Lens' DeleteTags [Types.String]
dtsTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED dtsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteTags")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ResourceName" resourceName)
                Core.<> (Core.toQueryValue "TagKeys" (Core.toQueryList "TagKey" tagKeys))
            )
      }
  response = Response.receiveNull DeleteTagsResponse'

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagsResponse' value with any optional fields omitted.
mkDeleteTagsResponse ::
  DeleteTagsResponse
mkDeleteTagsResponse = DeleteTagsResponse'
