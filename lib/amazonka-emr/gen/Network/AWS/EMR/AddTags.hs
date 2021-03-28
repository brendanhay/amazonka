{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters> . 
module Network.AWS.EMR.AddTags
    (
    -- * Creating a request
      AddTags (..)
    , mkAddTags
    -- ** Request lenses
    , atResourceId
    , atTags

    -- * Destructuring the response
    , AddTagsResponse (..)
    , mkAddTagsResponse
    -- ** Response lenses
    , atrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input identifies a cluster and a list of tags to attach.
--
-- /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { resourceId :: Types.ResourceId
    -- ^ The Amazon EMR resource identifier to which tags will be added. This value must be a cluster identifier.
  , tags :: [Types.Tag]
    -- ^ A list of tags to associate with a cluster and propagate to EC2 instances. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTags' value with any optional fields omitted.
mkAddTags
    :: Types.ResourceId -- ^ 'resourceId'
    -> AddTags
mkAddTags resourceId = AddTags'{resourceId, tags = Core.mempty}

-- | The Amazon EMR resource identifier to which tags will be added. This value must be a cluster identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceId :: Lens.Lens' AddTags Types.ResourceId
atResourceId = Lens.field @"resourceId"
{-# INLINEABLE atResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | A list of tags to associate with a cluster and propagate to EC2 instances. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags [Types.Tag]
atTags = Lens.field @"tags"
{-# INLINEABLE atTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery AddTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddTags where
        toHeaders AddTags{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.AddTags") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddTags where
        toJSON AddTags{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("Tags" Core..= tags)])

instance Core.AWSRequest AddTags where
        type Rs AddTags = AddTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AddTagsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | This output indicates the result of adding tags to a resource.
--
-- /See:/ 'mkAddTagsResponse' smart constructor.
newtype AddTagsResponse = AddTagsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsResponse' value with any optional fields omitted.
mkAddTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddTagsResponse
mkAddTagsResponse responseStatus = AddTagsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrrsResponseStatus :: Lens.Lens' AddTagsResponse Core.Int
atrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
