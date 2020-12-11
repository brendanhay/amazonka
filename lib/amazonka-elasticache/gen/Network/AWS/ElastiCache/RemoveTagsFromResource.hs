{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RemoveTagsFromResource (..),
    mkRemoveTagsFromResource,

    -- ** Request lenses
    rtfrResourceName,
    rtfrTagKeys,

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

-- | Represents the input of a @RemoveTagsFromResource@ operation.
--
-- /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { resourceName ::
      Lude.Text,
    tagKeys :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- * 'resourceName' - The Amazon Resource Name (ARN) of the resource from which you want the tags removed, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ .
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'tagKeys' - A list of @TagKeys@ identifying the tags you want removed from the named resource.
mkRemoveTagsFromResource ::
  -- | 'resourceName'
  Lude.Text ->
  RemoveTagsFromResource
mkRemoveTagsFromResource pResourceName_ =
  RemoveTagsFromResource'
    { resourceName = pResourceName_,
      tagKeys = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource from which you want the tags removed, for example @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@ .
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceName :: Lens.Lens' RemoveTagsFromResource Lude.Text
rtfrResourceName = Lens.lens (resourceName :: RemoveTagsFromResource -> Lude.Text) (\s a -> s {resourceName = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | A list of @TagKeys@ identifying the tags you want removed from the named resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Lude.Text]
rtfrTagKeys = Lens.lens (tagKeys :: RemoveTagsFromResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest RemoveTagsFromResource where
  type Rs RemoveTagsFromResource = TagListMessage
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "RemoveTagsFromResourceResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders RemoveTagsFromResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveTagsFromResource where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTagsFromResource where
  toQuery RemoveTagsFromResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RemoveTagsFromResource" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "ResourceName" Lude.=: resourceName,
        "TagKeys" Lude.=: Lude.toQueryList "member" tagKeys
      ]
