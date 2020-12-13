{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to an AWS Cloud9 development environment.
--
-- /Important:/ Tags that you add to an AWS Cloud9 environment by using this method will NOT be automatically propagated to underlying resources.
module Network.AWS.Cloud9.TagResource
  ( -- * Creating a request
    TagResource (..),
    mkTagResource,

    -- ** Request lenses
    trResourceARN,
    trTags,

    -- * Destructuring the response
    TagResourceResponse (..),
    mkTagResourceResponse,

    -- ** Response lenses
    trrsResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Name (ARN) of the AWS Cloud9 development environment to add tags to.
    resourceARN :: Lude.Text,
    -- | The list of tags to add to the given AWS Cloud9 development environment.
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the AWS Cloud9 development environment to add tags to.
-- * 'tags' - The list of tags to add to the given AWS Cloud9 development environment.
mkTagResource ::
  -- | 'resourceARN'
  Lude.Text ->
  TagResource
mkTagResource pResourceARN_ =
  TagResource' {resourceARN = pResourceARN_, tags = Lude.mempty}

-- | The Amazon Resource Name (ARN) of the AWS Cloud9 development environment to add tags to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceARN :: Lens.Lens' TagResource Lude.Text
trResourceARN = Lens.lens (resourceARN :: TagResource -> Lude.Text) (\s a -> s {resourceARN = a} :: TagResource)
{-# DEPRECATED trResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The list of tags to add to the given AWS Cloud9 development environment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource [Tag]
trTags = Lens.lens (tags :: TagResource -> [Tag]) (\s a -> s {tags = a} :: TagResource)
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          TagResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TagResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.TagResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TagResource where
  toJSON TagResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARN" Lude..= resourceARN),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath TagResource where
  toPath = Lude.const "/"

instance Lude.ToQuery TagResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagResourceResponse' smart constructor.
newtype TagResourceResponse = TagResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
