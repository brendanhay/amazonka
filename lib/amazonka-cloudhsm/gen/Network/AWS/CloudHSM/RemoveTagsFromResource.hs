{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Removes one or more tags from the specified AWS CloudHSM resource.
-- To remove a tag, specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
module Network.AWS.CloudHSM.RemoveTagsFromResource
  ( -- * Creating a request
    RemoveTagsFromResource (..),
    mkRemoveTagsFromResource,

    -- ** Request lenses
    rtfrResourceARN,
    rtfrTagKeyList,

    -- * Destructuring the response
    RemoveTagsFromResourceResponse (..),
    mkRemoveTagsFromResourceResponse,

    -- ** Response lenses
    rtfrrsResponseStatus,
    rtfrrsStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { resourceARN ::
      Lude.Text,
    tagKeyList :: [Lude.Text]
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
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
-- * 'tagKeyList' - The tag key or keys to remove.
--
-- Specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
mkRemoveTagsFromResource ::
  -- | 'resourceARN'
  Lude.Text ->
  RemoveTagsFromResource
mkRemoveTagsFromResource pResourceARN_ =
  RemoveTagsFromResource'
    { resourceARN = pResourceARN_,
      tagKeyList = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceARN :: Lens.Lens' RemoveTagsFromResource Lude.Text
rtfrResourceARN = Lens.lens (resourceARN :: RemoveTagsFromResource -> Lude.Text) (\s a -> s {resourceARN = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The tag key or keys to remove.
--
-- Specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
--
-- /Note:/ Consider using 'tagKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeyList :: Lens.Lens' RemoveTagsFromResource [Lude.Text]
rtfrTagKeyList = Lens.lens (tagKeyList :: RemoveTagsFromResource -> [Lude.Text]) (\s a -> s {tagKeyList = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrTagKeyList "Use generic-lens or generic-optics with 'tagKeyList' instead." #-}

instance Lude.AWSRequest RemoveTagsFromResource where
  type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "Status")
      )

instance Lude.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CloudHsmFrontendService.RemoveTagsFromResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("TagKeyList" Lude..= tagKeyList)
          ]
      )

instance Lude.ToPath RemoveTagsFromResource where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTagsFromResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
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

-- | Creates a value of 'RemoveTagsFromResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the operation.
mkRemoveTagsFromResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'status'
  Lude.Text ->
  RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse pResponseStatus_ pStatus_ =
  RemoveTagsFromResourceResponse'
    { responseStatus =
        pResponseStatus_,
      status = pStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrsResponseStatus :: Lens.Lens' RemoveTagsFromResourceResponse Lude.Int
rtfrrsResponseStatus = Lens.lens (responseStatus :: RemoveTagsFromResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveTagsFromResourceResponse)
{-# DEPRECATED rtfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The status of the operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrsStatus :: Lens.Lens' RemoveTagsFromResourceResponse Lude.Text
rtfrrsStatus = Lens.lens (status :: RemoveTagsFromResourceResponse -> Lude.Text) (\s a -> s {status = a} :: RemoveTagsFromResourceResponse)
{-# DEPRECATED rtfrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}
