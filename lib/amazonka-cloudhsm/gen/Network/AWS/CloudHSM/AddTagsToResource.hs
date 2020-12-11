{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Adds or overwrites one or more tags for the specified AWS CloudHSM resource.
-- Each tag consists of a key and a value. Tag keys must be unique to each resource.
module Network.AWS.CloudHSM.AddTagsToResource
  ( -- * Creating a request
    AddTagsToResource (..),
    mkAddTagsToResource,

    -- ** Request lenses
    attrResourceARN,
    attrTagList,

    -- * Destructuring the response
    AddTagsToResourceResponse (..),
    mkAddTagsToResourceResponse,

    -- ** Response lenses
    attrrsResponseStatus,
    attrrsStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { resourceARN ::
      Lude.Text,
    tagList :: [Tag]
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
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the AWS CloudHSM resource to tag.
-- * 'tagList' - One or more tags.
mkAddTagsToResource ::
  -- | 'resourceARN'
  Lude.Text ->
  AddTagsToResource
mkAddTagsToResource pResourceARN_ =
  AddTagsToResource'
    { resourceARN = pResourceARN_,
      tagList = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource to tag.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceARN :: Lens.Lens' AddTagsToResource Lude.Text
attrResourceARN = Lens.lens (resourceARN :: AddTagsToResource -> Lude.Text) (\s a -> s {resourceARN = a} :: AddTagsToResource)
{-# DEPRECATED attrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTagList :: Lens.Lens' AddTagsToResource [Tag]
attrTagList = Lens.lens (tagList :: AddTagsToResource -> [Tag]) (\s a -> s {tagList = a} :: AddTagsToResource)
{-# DEPRECATED attrTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

instance Lude.AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = AddTagsToResourceResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddTagsToResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "Status")
      )

instance Lude.ToHeaders AddTagsToResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.AddTagsToResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("TagList" Lude..= tagList)
          ]
      )

instance Lude.ToPath AddTagsToResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTagsToResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { responseStatus ::
      Lude.Int,
    status :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the operation.
mkAddTagsToResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'status'
  Lude.Text ->
  AddTagsToResourceResponse
mkAddTagsToResourceResponse pResponseStatus_ pStatus_ =
  AddTagsToResourceResponse'
    { responseStatus = pResponseStatus_,
      status = pStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrsResponseStatus :: Lens.Lens' AddTagsToResourceResponse Lude.Int
attrrsResponseStatus = Lens.lens (responseStatus :: AddTagsToResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddTagsToResourceResponse)
{-# DEPRECATED attrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The status of the operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrsStatus :: Lens.Lens' AddTagsToResourceResponse Lude.Text
attrrsStatus = Lens.lens (status :: AddTagsToResourceResponse -> Lude.Text) (\s a -> s {status = a} :: AddTagsToResourceResponse)
{-# DEPRECATED attrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}
