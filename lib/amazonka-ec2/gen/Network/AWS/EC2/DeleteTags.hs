{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of tags from the specified set of resources.
--
-- To list the current tags, use 'DescribeTags' . For more information about tags, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DeleteTags
    (
    -- * Creating a request
      DeleteTags (..)
    , mkDeleteTags
    -- ** Request lenses
    , dtsResources
    , dtsDryRun
    , dtsTags

    -- * Destructuring the response
    , DeleteTagsResponse (..)
    , mkDeleteTagsResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { resources :: [Types.TaggableResourceId]
    -- ^ The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to delete. Specify a tag key and an optional tag value to delete specific tags. If you specify a tag key without a tag value, we delete any tag with this key regardless of its value. If you specify a tag key with an empty string as the tag value, we delete the tag only if its value is an empty string.
--
-- If you omit this parameter, we delete all user-defined tags for the specified resources. We do not delete AWS-generated tags (tags that have the @aws:@ prefix).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTags' value with any optional fields omitted.
mkDeleteTags
    :: DeleteTags
mkDeleteTags
  = DeleteTags'{resources = Core.mempty, dryRun = Core.Nothing,
                tags = Core.Nothing}

-- | The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsResources :: Lens.Lens' DeleteTags [Types.TaggableResourceId]
dtsResources = Lens.field @"resources"
{-# INLINEABLE dtsResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsDryRun :: Lens.Lens' DeleteTags (Core.Maybe Core.Bool)
dtsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to delete. Specify a tag key and an optional tag value to delete specific tags. If you specify a tag key without a tag value, we delete any tag with this key regardless of its value. If you specify a tag key with an empty string as the tag value, we delete the tag only if its value is an empty string.
--
-- If you omit this parameter, we delete all user-defined tags for the specified resources. We do not delete AWS-generated tags (tags that have the @aws:@ prefix).
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsTags :: Lens.Lens' DeleteTags (Core.Maybe [Types.Tag])
dtsTags = Lens.field @"tags"
{-# INLINEABLE dtsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery DeleteTags where
        toQuery DeleteTags{..}
          = Core.toQueryPair "Action" ("DeleteTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "ResourceId" resources
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Tag") tags

instance Core.ToHeaders DeleteTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTags where
        type Rs DeleteTags = DeleteTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteTagsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagsResponse' value with any optional fields omitted.
mkDeleteTagsResponse
    :: DeleteTagsResponse
mkDeleteTagsResponse = DeleteTagsResponse'
