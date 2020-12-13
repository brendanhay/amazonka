{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds metadata tags to an Amazon RDS resource. These tags can also be used with cost allocation reporting to track cost associated with Amazon RDS resources, or used in a Condition statement in an IAM policy for Amazon RDS.
--
-- For an overview on tagging Amazon RDS resources, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources> .
module Network.AWS.RDS.AddTagsToResource
  ( -- * Creating a request
    AddTagsToResource (..),
    mkAddTagsToResource,

    -- ** Request lenses
    attrResourceName,
    attrTags,

    -- * Destructuring the response
    AddTagsToResourceResponse (..),
    mkAddTagsToResourceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | The Amazon RDS resource that the tags are added to. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
    resourceName :: Lude.Text,
    -- | The tags to be assigned to the Amazon RDS resource.
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- * 'resourceName' - The Amazon RDS resource that the tags are added to. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
-- * 'tags' - The tags to be assigned to the Amazon RDS resource.
mkAddTagsToResource ::
  -- | 'resourceName'
  Lude.Text ->
  AddTagsToResource
mkAddTagsToResource pResourceName_ =
  AddTagsToResource'
    { resourceName = pResourceName_,
      tags = Lude.mempty
    }

-- | The Amazon RDS resource that the tags are added to. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceName :: Lens.Lens' AddTagsToResource Lude.Text
attrResourceName = Lens.lens (resourceName :: AddTagsToResource -> Lude.Text) (\s a -> s {resourceName = a} :: AddTagsToResource)
{-# DEPRECATED attrResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The tags to be assigned to the Amazon RDS resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Tag]
attrTags = Lens.lens (tags :: AddTagsToResource -> [Tag]) (\s a -> s {tags = a} :: AddTagsToResource)
{-# DEPRECATED attrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = AddTagsToResourceResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull AddTagsToResourceResponse'

instance Lude.ToHeaders AddTagsToResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddTagsToResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTagsToResource where
  toQuery AddTagsToResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AddTagsToResource" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ResourceName" Lude.=: resourceName,
        "Tags" Lude.=: Lude.toQueryList "Tag" tags
      ]

-- | /See:/ 'mkAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
mkAddTagsToResourceResponse ::
  AddTagsToResourceResponse
mkAddTagsToResourceResponse = AddTagsToResourceResponse'
