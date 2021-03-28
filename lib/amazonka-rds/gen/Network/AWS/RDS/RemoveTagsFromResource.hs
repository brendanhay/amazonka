{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes metadata tags from an Amazon RDS resource.
--
-- For an overview on tagging an Amazon RDS resource, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide./ 
module Network.AWS.RDS.RemoveTagsFromResource
    (
    -- * Creating a request
      RemoveTagsFromResource (..)
    , mkRemoveTagsFromResource
    -- ** Request lenses
    , rtfrResourceName
    , rtfrTagKeys

    -- * Destructuring the response
    , RemoveTagsFromResourceResponse (..)
    , mkRemoveTagsFromResourceResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { resourceName :: Core.Text
    -- ^ The Amazon RDS resource that the tags are removed from. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide./ 
  , tagKeys :: [Core.Text]
    -- ^ The tag key (name) of the tag to be removed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResource' value with any optional fields omitted.
mkRemoveTagsFromResource
    :: Core.Text -- ^ 'resourceName'
    -> RemoveTagsFromResource
mkRemoveTagsFromResource resourceName
  = RemoveTagsFromResource'{resourceName, tagKeys = Core.mempty}

-- | The Amazon RDS resource that the tags are removed from. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceName :: Lens.Lens' RemoveTagsFromResource Core.Text
rtfrResourceName = Lens.field @"resourceName"
{-# INLINEABLE rtfrResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | The tag key (name) of the tag to be removed.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Core.Text]
rtfrTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE rtfrTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery RemoveTagsFromResource where
        toQuery RemoveTagsFromResource{..}
          = Core.toQueryPair "Action" ("RemoveTagsFromResource" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "ResourceName" resourceName
              Core.<>
              Core.toQueryPair "TagKeys" (Core.toQueryList "member" tagKeys)

instance Core.ToHeaders RemoveTagsFromResource where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
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
        parseResponse
          = Response.receiveNull RemoveTagsFromResourceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResourceResponse' value with any optional fields omitted.
mkRemoveTagsFromResourceResponse
    :: RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
