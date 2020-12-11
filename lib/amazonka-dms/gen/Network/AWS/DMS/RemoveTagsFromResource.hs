{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes metadata tags from an AWS DMS resource, including replication instance, endpoint, security group, and migration task. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_Tag.html @Tag@ > data type description.
module Network.AWS.DMS.RemoveTagsFromResource
  ( -- * Creating a request
    RemoveTagsFromResource (..),
    mkRemoveTagsFromResource,

    -- ** Request lenses
    rtfrResourceARN,
    rtfrTagKeys,

    -- * Destructuring the response
    RemoveTagsFromResourceResponse (..),
    mkRemoveTagsFromResourceResponse,

    -- ** Response lenses
    rtfrrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Removes one or more tags from an AWS DMS resource.
--
-- /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { resourceARN ::
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
-- * 'resourceARN' - An AWS DMS resource from which you want to remove tag(s). The value for this parameter is an Amazon Resource Name (ARN).
-- * 'tagKeys' - The tag key (name) of the tag to be removed.
mkRemoveTagsFromResource ::
  -- | 'resourceARN'
  Lude.Text ->
  RemoveTagsFromResource
mkRemoveTagsFromResource pResourceARN_ =
  RemoveTagsFromResource'
    { resourceARN = pResourceARN_,
      tagKeys = Lude.mempty
    }

-- | An AWS DMS resource from which you want to remove tag(s). The value for this parameter is an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceARN :: Lens.Lens' RemoveTagsFromResource Lude.Text
rtfrResourceARN = Lens.lens (resourceARN :: RemoveTagsFromResource -> Lude.Text) (\s a -> s {resourceARN = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The tag key (name) of the tag to be removed.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Lude.Text]
rtfrTagKeys = Lens.lens (tagKeys :: RemoveTagsFromResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest RemoveTagsFromResource where
  type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.RemoveTagsFromResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("TagKeys" Lude..= tagKeys)
          ]
      )

instance Lude.ToPath RemoveTagsFromResource where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTagsFromResource where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveTagsFromResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse pResponseStatus_ =
  RemoveTagsFromResourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrsResponseStatus :: Lens.Lens' RemoveTagsFromResourceResponse Lude.Int
rtfrrsResponseStatus = Lens.lens (responseStatus :: RemoveTagsFromResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveTagsFromResourceResponse)
{-# DEPRECATED rtfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
