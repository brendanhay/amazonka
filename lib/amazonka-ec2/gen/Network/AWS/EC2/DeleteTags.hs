{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteTags (..),
    mkDeleteTags,

    -- ** Request lenses
    dtsDryRun,
    dtsTags,
    dtsResources,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { dryRun :: Lude.Maybe Lude.Bool,
    tags :: Lude.Maybe [Tag],
    resources :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'resources' - The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
-- * 'tags' - The tags to delete. Specify a tag key and an optional tag value to delete specific tags. If you specify a tag key without a tag value, we delete any tag with this key regardless of its value. If you specify a tag key with an empty string as the tag value, we delete the tag only if its value is an empty string.
--
-- If you omit this parameter, we delete all user-defined tags for the specified resources. We do not delete AWS-generated tags (tags that have the @aws:@ prefix).
mkDeleteTags ::
  DeleteTags
mkDeleteTags =
  DeleteTags'
    { dryRun = Lude.Nothing,
      tags = Lude.Nothing,
      resources = Lude.mempty
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsDryRun :: Lens.Lens' DeleteTags (Lude.Maybe Lude.Bool)
dtsDryRun = Lens.lens (dryRun :: DeleteTags -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTags)
{-# DEPRECATED dtsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to delete. Specify a tag key and an optional tag value to delete specific tags. If you specify a tag key without a tag value, we delete any tag with this key regardless of its value. If you specify a tag key with an empty string as the tag value, we delete the tag only if its value is an empty string.
--
-- If you omit this parameter, we delete all user-defined tags for the specified resources. We do not delete AWS-generated tags (tags that have the @aws:@ prefix).
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsTags :: Lens.Lens' DeleteTags (Lude.Maybe [Tag])
dtsTags = Lens.lens (tags :: DeleteTags -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DeleteTags)
{-# DEPRECATED dtsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsResources :: Lens.Lens' DeleteTags [Lude.Text]
dtsResources = Lens.lens (resources :: DeleteTags -> [Lude.Text]) (\s a -> s {resources = a} :: DeleteTags)
{-# DEPRECATED dtsResources "Use generic-lens or generic-optics with 'resources' instead." #-}

instance Lude.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteTagsResponse'

instance Lude.ToHeaders DeleteTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTags where
  toQuery DeleteTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteTags" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        Lude.toQueryList "ResourceId" resources
      ]

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
mkDeleteTagsResponse ::
  DeleteTagsResponse
mkDeleteTagsResponse = DeleteTagsResponse'
