{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.PutLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes an object lifecycle policy to a container. If the container already has an object lifecycle policy, the service replaces the existing policy with the new policy. It takes up to 20 minutes for the change to take effect.
--
-- For information about how to construct an object lifecycle policy, see <https://docs.aws.amazon.com/mediastore/latest/ug/policies-object-lifecycle-components.html Components of an Object Lifecycle Policy> .
module Network.AWS.MediaStore.PutLifecyclePolicy
  ( -- * Creating a request
    PutLifecyclePolicy (..),
    mkPutLifecyclePolicy,

    -- ** Request lenses
    plpContainerName,
    plpLifecyclePolicy,

    -- * Destructuring the response
    PutLifecyclePolicyResponse (..),
    mkPutLifecyclePolicyResponse,

    -- ** Response lenses
    plprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { containerName ::
      Lude.Text,
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

-- | Creates a value of 'PutLifecyclePolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that you want to assign the object lifecycle policy to.
-- * 'lifecyclePolicy' - The object lifecycle policy to apply to the container.
mkPutLifecyclePolicy ::
  -- | 'containerName'
  Lude.Text ->
  -- | 'lifecyclePolicy'
  Lude.Text ->
  PutLifecyclePolicy
mkPutLifecyclePolicy pContainerName_ pLifecyclePolicy_ =
  PutLifecyclePolicy'
    { containerName = pContainerName_,
      lifecyclePolicy = pLifecyclePolicy_
    }

-- | The name of the container that you want to assign the object lifecycle policy to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpContainerName :: Lens.Lens' PutLifecyclePolicy Lude.Text
plpContainerName = Lens.lens (containerName :: PutLifecyclePolicy -> Lude.Text) (\s a -> s {containerName = a} :: PutLifecyclePolicy)
{-# DEPRECATED plpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The object lifecycle policy to apply to the container.
--
-- /Note:/ Consider using 'lifecyclePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpLifecyclePolicy :: Lens.Lens' PutLifecyclePolicy Lude.Text
plpLifecyclePolicy = Lens.lens (lifecyclePolicy :: PutLifecyclePolicy -> Lude.Text) (\s a -> s {lifecyclePolicy = a} :: PutLifecyclePolicy)
{-# DEPRECATED plpLifecyclePolicy "Use generic-lens or generic-optics with 'lifecyclePolicy' instead." #-}

instance Lude.AWSRequest PutLifecyclePolicy where
  type Rs PutLifecyclePolicy = PutLifecyclePolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutLifecyclePolicyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutLifecyclePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.PutLifecyclePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutLifecyclePolicy where
  toJSON PutLifecyclePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ContainerName" Lude..= containerName),
            Lude.Just ("LifecyclePolicy" Lude..= lifecyclePolicy)
          ]
      )

instance Lude.ToPath PutLifecyclePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutLifecyclePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutLifecyclePolicyResponse' smart constructor.
newtype PutLifecyclePolicyResponse = PutLifecyclePolicyResponse'
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

-- | Creates a value of 'PutLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutLifecyclePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutLifecyclePolicyResponse
mkPutLifecyclePolicyResponse pResponseStatus_ =
  PutLifecyclePolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plprsResponseStatus :: Lens.Lens' PutLifecyclePolicyResponse Lude.Int
plprsResponseStatus = Lens.lens (responseStatus :: PutLifecyclePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutLifecyclePolicyResponse)
{-# DEPRECATED plprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
