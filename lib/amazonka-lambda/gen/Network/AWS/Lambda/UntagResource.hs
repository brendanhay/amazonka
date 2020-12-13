{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> from a function.
module Network.AWS.Lambda.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urTagKeys,
    urResource,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | A list of tag keys to remove from the function.
    tagKeys :: [Lude.Text],
    -- | The function's Amazon Resource Name (ARN).
    resource :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- * 'tagKeys' - A list of tag keys to remove from the function.
-- * 'resource' - The function's Amazon Resource Name (ARN).
mkUntagResource ::
  -- | 'resource'
  Lude.Text ->
  UntagResource
mkUntagResource pResource_ =
  UntagResource' {tagKeys = Lude.mempty, resource = pResource_}

-- | A list of tag keys to remove from the function.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Lude.Text]
urTagKeys = Lens.lens (tagKeys :: UntagResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: UntagResource)
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The function's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResource :: Lens.Lens' UntagResource Lude.Text
urResource = Lens.lens (resource :: UntagResource -> Lude.Text) (\s a -> s {resource = a} :: UntagResource)
{-# DEPRECATED urResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Req.delete lambdaService
  response = Res.receiveNull UntagResourceResponse'

instance Lude.ToHeaders UntagResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UntagResource where
  toPath UntagResource' {..} =
    Lude.mconcat ["/2017-03-31/tags/", Lude.toBS resource]

instance Lude.ToQuery UntagResource where
  toQuery UntagResource' {..} =
    Lude.mconcat
      ["tagKeys" Lude.=: Lude.toQueryList "member" tagKeys]

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
mkUntagResourceResponse ::
  UntagResourceResponse
mkUntagResourceResponse = UntagResourceResponse'
