{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a tag from a Amazon SWF domain.
module Network.AWS.SWF.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urTagKeys,
    urResourceARN,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The list of tags to remove from the Amazon SWF domain.
    tagKeys :: [Lude.Text],
    -- | The Amazon Resource Name (ARN) for the Amazon SWF domain.
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- * 'tagKeys' - The list of tags to remove from the Amazon SWF domain.
-- * 'resourceARN' - The Amazon Resource Name (ARN) for the Amazon SWF domain.
mkUntagResource ::
  -- | 'resourceARN'
  Lude.Text ->
  UntagResource
mkUntagResource pResourceARN_ =
  UntagResource'
    { tagKeys = Lude.mempty,
      resourceARN = pResourceARN_
    }

-- | The list of tags to remove from the Amazon SWF domain.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Lude.Text]
urTagKeys = Lens.lens (tagKeys :: UntagResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: UntagResource)
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The Amazon Resource Name (ARN) for the Amazon SWF domain.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceARN :: Lens.Lens' UntagResource Lude.Text
urResourceARN = Lens.lens (resourceARN :: UntagResource -> Lude.Text) (\s a -> s {resourceARN = a} :: UntagResource)
{-# DEPRECATED urResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Req.postJSON swfService
  response = Res.receiveNull UntagResourceResponse'

instance Lude.ToHeaders UntagResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.UntagResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
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
data UntagResourceResponse = UntagResourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
mkUntagResourceResponse ::
  UntagResourceResponse
mkUntagResourceResponse = UntagResourceResponse'
