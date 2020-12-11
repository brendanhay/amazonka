{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.GetLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the object lifecycle policy that is assigned to a container.
module Network.AWS.MediaStore.GetLifecyclePolicy
  ( -- * Creating a request
    GetLifecyclePolicy (..),
    mkGetLifecyclePolicy,

    -- ** Request lenses
    glpContainerName,

    -- * Destructuring the response
    GetLifecyclePolicyResponse (..),
    mkGetLifecyclePolicyResponse,

    -- ** Response lenses
    glprsResponseStatus,
    glprsLifecyclePolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLifecyclePolicy' smart constructor.
newtype GetLifecyclePolicy = GetLifecyclePolicy'
  { containerName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLifecyclePolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that the object lifecycle policy is assigned to.
mkGetLifecyclePolicy ::
  -- | 'containerName'
  Lude.Text ->
  GetLifecyclePolicy
mkGetLifecyclePolicy pContainerName_ =
  GetLifecyclePolicy' {containerName = pContainerName_}

-- | The name of the container that the object lifecycle policy is assigned to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpContainerName :: Lens.Lens' GetLifecyclePolicy Lude.Text
glpContainerName = Lens.lens (containerName :: GetLifecyclePolicy -> Lude.Text) (\s a -> s {containerName = a} :: GetLifecyclePolicy)
{-# DEPRECATED glpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest GetLifecyclePolicy where
  type Rs GetLifecyclePolicy = GetLifecyclePolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "LifecyclePolicy")
      )

instance Lude.ToHeaders GetLifecyclePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.GetLifecyclePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetLifecyclePolicy where
  toJSON GetLifecyclePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath GetLifecyclePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLifecyclePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { responseStatus ::
      Lude.Int,
    lifecyclePolicy :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- * 'lifecyclePolicy' - The object lifecycle policy that is assigned to the container.
-- * 'responseStatus' - The response status code.
mkGetLifecyclePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'lifecyclePolicy'
  Lude.Text ->
  GetLifecyclePolicyResponse
mkGetLifecyclePolicyResponse pResponseStatus_ pLifecyclePolicy_ =
  GetLifecyclePolicyResponse'
    { responseStatus = pResponseStatus_,
      lifecyclePolicy = pLifecyclePolicy_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprsResponseStatus :: Lens.Lens' GetLifecyclePolicyResponse Lude.Int
glprsResponseStatus = Lens.lens (responseStatus :: GetLifecyclePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLifecyclePolicyResponse)
{-# DEPRECATED glprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The object lifecycle policy that is assigned to the container.
--
-- /Note:/ Consider using 'lifecyclePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprsLifecyclePolicy :: Lens.Lens' GetLifecyclePolicyResponse Lude.Text
glprsLifecyclePolicy = Lens.lens (lifecyclePolicy :: GetLifecyclePolicyResponse -> Lude.Text) (\s a -> s {lifecyclePolicy = a} :: GetLifecyclePolicyResponse)
{-# DEPRECATED glprsLifecyclePolicy "Use generic-lens or generic-optics with 'lifecyclePolicy' instead." #-}
