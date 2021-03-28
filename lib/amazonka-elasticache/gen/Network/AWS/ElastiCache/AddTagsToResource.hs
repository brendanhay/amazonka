{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds up to 50 cost allocation tags to the named resource. A cost allocation tag is a key-value pair where the key and value are case-sensitive. You can use cost allocation tags to categorize and track your AWS costs.
--
-- When you apply tags to your ElastiCache resources, AWS generates a cost allocation report as a comma-separated value (CSV) file with your usage and costs aggregated by your tags. You can apply tags that represent business categories (such as cost centers, application names, or owners) to organize your costs across multiple services. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Tagging.html Using Cost Allocation Tags in Amazon ElastiCache> in the /ElastiCache User Guide/ .
module Network.AWS.ElastiCache.AddTagsToResource
    (
    -- * Creating a request
      AddTagsToResource (..)
    , mkAddTagsToResource
    -- ** Request lenses
    , attrResourceName
    , attrTags

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

-- | Represents the input of an AddTagsToResource operation.
--
-- /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { resourceName :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the resource to which the tags are to be added, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ . ElastiCache resources are /cluster/ and /snapshot/ .
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , tags :: [Types.Tag]
    -- ^ A list of cost allocation tags to be added to this resource. A tag is a key-value pair. A tag key must be accompanied by a tag value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResource' value with any optional fields omitted.
mkAddTagsToResource
    :: Core.Text -- ^ 'resourceName'
    -> AddTagsToResource
mkAddTagsToResource resourceName
  = AddTagsToResource'{resourceName, tags = Core.mempty}

-- | The Amazon Resource Name (ARN) of the resource to which the tags are to be added, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ . ElastiCache resources are /cluster/ and /snapshot/ .
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceName :: Lens.Lens' AddTagsToResource Core.Text
attrResourceName = Lens.field @"resourceName"
{-# INLINEABLE attrResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | A list of cost allocation tags to be added to this resource. A tag is a key-value pair. A tag key must be accompanied by a tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Types.Tag]
attrTags = Lens.field @"tags"
{-# INLINEABLE attrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery AddTagsToResource where
        toQuery AddTagsToResource{..}
          = Core.toQueryPair "Action" ("AddTagsToResource" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "ResourceName" resourceName
              Core.<> Core.toQueryPair "Tags" (Core.toQueryList "Tag" tags)

instance Core.ToHeaders AddTagsToResource where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = Types.TagListMessage
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
          = Response.receiveXMLWrapper "AddTagsToResourceResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
