{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes tags from a resource. You must provide the ARN of the resource from which you want to delete the tag or tags.
module Network.AWS.Redshift.DeleteTags
  ( -- * Creating a request
    DeleteTags (..),
    mkDeleteTags,

    -- ** Request lenses
    dResourceName,
    dTagKeys,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the output from the @DeleteTags@ action.
--
-- /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | The Amazon Resource Name (ARN) from which you want to remove the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
    resourceName :: Lude.Text,
    -- | The tag key that you want to delete.
    tagKeys :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- * 'resourceName' - The Amazon Resource Name (ARN) from which you want to remove the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
-- * 'tagKeys' - The tag key that you want to delete.
mkDeleteTags ::
  -- | 'resourceName'
  Lude.Text ->
  DeleteTags
mkDeleteTags pResourceName_ =
  DeleteTags' {resourceName = pResourceName_, tagKeys = Lude.mempty}

-- | The Amazon Resource Name (ARN) from which you want to remove the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceName :: Lens.Lens' DeleteTags Lude.Text
dResourceName = Lens.lens (resourceName :: DeleteTags -> Lude.Text) (\s a -> s {resourceName = a} :: DeleteTags)
{-# DEPRECATED dResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The tag key that you want to delete.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTagKeys :: Lens.Lens' DeleteTags [Lude.Text]
dTagKeys = Lens.lens (tagKeys :: DeleteTags -> [Lude.Text]) (\s a -> s {tagKeys = a} :: DeleteTags)
{-# DEPRECATED dTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteTagsResponse'

instance Lude.ToHeaders DeleteTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTags where
  toQuery DeleteTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteTags" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ResourceName" Lude.=: resourceName,
        "TagKeys" Lude.=: Lude.toQueryList "TagKey" tagKeys
      ]

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
mkDeleteTagsResponse ::
  DeleteTagsResponse
mkDeleteTagsResponse = DeleteTagsResponse'
