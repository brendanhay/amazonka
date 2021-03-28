{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the tags identified by the @TagKeys@ list from the named resource.
module Network.AWS.ElastiCache.RemoveTagsFromResource
    (
    -- * Creating a request
      RemoveTagsFromResource (..)
    , mkRemoveTagsFromResource
    -- ** Request lenses
    , rtfrResourceName
    , rtfrTagKeys

     -- * Destructuring the response
    , Types.TagListMessage (..)
    , Types.mkTagListMessage
    -- ** Response lenses
    , Types.tlmTagList
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @RemoveTagsFromResource@ operation.
--
-- /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { resourceName :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the resource from which you want the tags removed, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ .
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , tagKeys :: [Core.Text]
    -- ^ A list of @TagKeys@ identifying the tags you want removed from the named resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResource' value with any optional fields omitted.
mkRemoveTagsFromResource
    :: Core.Text -- ^ 'resourceName'
    -> RemoveTagsFromResource
mkRemoveTagsFromResource resourceName
  = RemoveTagsFromResource'{resourceName, tagKeys = Core.mempty}

-- | The Amazon Resource Name (ARN) of the resource from which you want the tags removed, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ .
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceName :: Lens.Lens' RemoveTagsFromResource Core.Text
rtfrResourceName = Lens.field @"resourceName"
{-# INLINEABLE rtfrResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | A list of @TagKeys@ identifying the tags you want removed from the named resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Core.Text]
rtfrTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE rtfrTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery RemoveTagsFromResource where
        toQuery RemoveTagsFromResource{..}
          = Core.toQueryPair "Action" ("RemoveTagsFromResource" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "ResourceName" resourceName
              Core.<>
              Core.toQueryPair "TagKeys" (Core.toQueryList "member" tagKeys)

instance Core.ToHeaders RemoveTagsFromResource where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource = Types.TagListMessage
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
          = Response.receiveXMLWrapper "RemoveTagsFromResourceResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
