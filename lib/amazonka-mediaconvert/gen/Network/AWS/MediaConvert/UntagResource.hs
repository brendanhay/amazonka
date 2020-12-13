{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove tags from a MediaConvert queue, preset, or job template. For information about tagging, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/tagging-resources.html
module Network.AWS.MediaConvert.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urARN,
    urTagKeys,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,

    -- ** Response lenses
    urrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The Amazon Resource Name (ARN) of the resource that you want to remove tags from. To get the ARN, send a GET request with the resource name.
    arn :: Lude.Text,
    -- | The keys of the tags that you want to remove from the resource.
    tagKeys :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the resource that you want to remove tags from. To get the ARN, send a GET request with the resource name.
-- * 'tagKeys' - The keys of the tags that you want to remove from the resource.
mkUntagResource ::
  -- | 'arn'
  Lude.Text ->
  UntagResource
mkUntagResource pARN_ =
  UntagResource' {arn = pARN_, tagKeys = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the resource that you want to remove tags from. To get the ARN, send a GET request with the resource name.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urARN :: Lens.Lens' UntagResource Lude.Text
urARN = Lens.lens (arn :: UntagResource -> Lude.Text) (\s a -> s {arn = a} :: UntagResource)
{-# DEPRECATED urARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The keys of the tags that you want to remove from the resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource (Lude.Maybe [Lude.Text])
urTagKeys = Lens.lens (tagKeys :: UntagResource -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: UntagResource)
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Req.putJSON mediaConvertService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UntagResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UntagResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Lude.object
      (Lude.catMaybes [("tagKeys" Lude..=) Lude.<$> tagKeys])

instance Lude.ToPath UntagResource where
  toPath UntagResource' {..} =
    Lude.mconcat ["/2017-08-29/tags/", Lude.toBS arn]

instance Lude.ToQuery UntagResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
newtype UntagResourceResponse = UntagResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUntagResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UntagResourceResponse
mkUntagResourceResponse pResponseStatus_ =
  UntagResourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UntagResourceResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UntagResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UntagResourceResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
