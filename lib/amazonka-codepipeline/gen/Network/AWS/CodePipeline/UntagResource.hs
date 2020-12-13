{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from an AWS resource.
module Network.AWS.CodePipeline.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urTagKeys,
    urResourceARN,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,

    -- ** Response lenses
    urrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The list of keys for the tags to be removed from the resource.
    tagKeys :: [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the resource to remove tags from.
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- * 'tagKeys' - The list of keys for the tags to be removed from the resource.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource to remove tags from.
mkUntagResource ::
  -- | 'resourceARN'
  Lude.Text ->
  UntagResource
mkUntagResource pResourceARN_ =
  UntagResource'
    { tagKeys = Lude.mempty,
      resourceARN = pResourceARN_
    }

-- | The list of keys for the tags to be removed from the resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Lude.Text]
urTagKeys = Lens.lens (tagKeys :: UntagResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: UntagResource)
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The Amazon Resource Name (ARN) of the resource to remove tags from.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceARN :: Lens.Lens' UntagResource Lude.Text
urResourceARN = Lens.lens (resourceARN :: UntagResource -> Lude.Text) (\s a -> s {resourceARN = a} :: UntagResource)
{-# DEPRECATED urResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UntagResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UntagResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.UntagResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("tagKeys" Lude..= tagKeys),
            Lude.Just ("resourceArn" Lude..= resourceARN)
          ]
      )

instance Lude.ToPath UntagResource where
  toPath = Lude.const "/"

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
