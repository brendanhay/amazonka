{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all cost allocation tags currently on the named resource. A @cost allocation tag@ is a key-value pair where the key is case-sensitive and the value is optional. You can use cost allocation tags to categorize and track your AWS costs.
--
-- If the cluster is not in the /available/ state, @ListTagsForResource@ returns an error.
-- You can have a maximum of 50 cost allocation tags on an ElastiCache resource. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Tagging.html Monitoring Costs with Tags> .
module Network.AWS.ElastiCache.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrResourceName,

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

-- | The input parameters for the @ListTagsForResource@ operation.
--
-- /See:/ 'mkListTagsForResource' smart constructor.
newtype ListTagsForResource = ListTagsForResource'
  { resourceName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'resourceName' - The Amazon Resource Name (ARN) of the resource for which you want the list of tags, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ .
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkListTagsForResource ::
  -- | 'resourceName'
  Lude.Text ->
  ListTagsForResource
mkListTagsForResource pResourceName_ =
  ListTagsForResource' {resourceName = pResourceName_}

-- | The Amazon Resource Name (ARN) of the resource for which you want the list of tags, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ .
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceName :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResourceName = Lens.lens (resourceName :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceName = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = TagListMessage
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ListTagsForResourceResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ListTagsForResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTagsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListTagsForResource" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "ResourceName" Lude.=: resourceName
      ]
