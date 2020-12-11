{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AddTagsToResource (..),
    mkAddTagsToResource,

    -- ** Request lenses
    attrResourceName,
    attrTags,

    -- * Destructuring the response
    TagListMessage (..),
    mkTagListMessage,

    -- ** Response lenses
    tlmTagList,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an AddTagsToResource operation.
--
-- /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { resourceName ::
      Lude.Text,
    tags :: [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- * 'resourceName' - The Amazon Resource Name (ARN) of the resource to which the tags are to be added, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ . ElastiCache resources are /cluster/ and /snapshot/ .
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'tags' - A list of cost allocation tags to be added to this resource. A tag is a key-value pair. A tag key must be accompanied by a tag value.
mkAddTagsToResource ::
  -- | 'resourceName'
  Lude.Text ->
  AddTagsToResource
mkAddTagsToResource pResourceName_ =
  AddTagsToResource'
    { resourceName = pResourceName_,
      tags = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource to which the tags are to be added, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ . ElastiCache resources are /cluster/ and /snapshot/ .
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceName :: Lens.Lens' AddTagsToResource Lude.Text
attrResourceName = Lens.lens (resourceName :: AddTagsToResource -> Lude.Text) (\s a -> s {resourceName = a} :: AddTagsToResource)
{-# DEPRECATED attrResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | A list of cost allocation tags to be added to this resource. A tag is a key-value pair. A tag key must be accompanied by a tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Tag]
attrTags = Lens.lens (tags :: AddTagsToResource -> [Tag]) (\s a -> s {tags = a} :: AddTagsToResource)
{-# DEPRECATED attrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = TagListMessage
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "AddTagsToResourceResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders AddTagsToResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddTagsToResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTagsToResource where
  toQuery AddTagsToResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AddTagsToResource" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "ResourceName" Lude.=: resourceName,
        "Tags" Lude.=: Lude.toQueryList "Tag" tags
      ]
