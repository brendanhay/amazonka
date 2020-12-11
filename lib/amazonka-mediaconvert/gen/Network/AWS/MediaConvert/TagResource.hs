{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add tags to a MediaConvert queue, preset, or job template. For information about tagging, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/tagging-resources.html
module Network.AWS.MediaConvert.TagResource
  ( -- * Creating a request
    TagResource (..),
    mkTagResource,

    -- ** Request lenses
    trARN,
    trTags,

    -- * Destructuring the response
    TagResourceResponse (..),
    mkTagResourceResponse,

    -- ** Response lenses
    trrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { arn :: Lude.Text,
    tags :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the resource that you want to tag. To get the ARN, send a GET request with the resource name.
-- * 'tags' - The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
mkTagResource ::
  -- | 'arn'
  Lude.Text ->
  TagResource
mkTagResource pARN_ = TagResource' {arn = pARN_, tags = Lude.mempty}

-- | The Amazon Resource Name (ARN) of the resource that you want to tag. To get the ARN, send a GET request with the resource name.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trARN :: Lens.Lens' TagResource Lude.Text
trARN = Lens.lens (arn :: TagResource -> Lude.Text) (\s a -> s {arn = a} :: TagResource)
{-# DEPRECATED trARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource (Lude.HashMap Lude.Text (Lude.Text))
trTags = Lens.lens (tags :: TagResource -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {tags = a} :: TagResource)
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Req.postJSON mediaConvertService
  response =
    Res.receiveEmpty
      ( \s h x ->
          TagResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TagResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TagResource where
  toJSON TagResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("arn" Lude..= arn), Lude.Just ("tags" Lude..= tags)]
      )

instance Lude.ToPath TagResource where
  toPath = Lude.const "/2017-08-29/tags"

instance Lude.ToQuery TagResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagResourceResponse' smart constructor.
newtype TagResourceResponse = TagResourceResponse'
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

-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkTagResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TagResourceResponse
mkTagResourceResponse pResponseStatus_ =
  TagResourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrsResponseStatus :: Lens.Lens' TagResourceResponse Lude.Int
trrsResponseStatus = Lens.lens (responseStatus :: TagResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TagResourceResponse)
{-# DEPRECATED trrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
