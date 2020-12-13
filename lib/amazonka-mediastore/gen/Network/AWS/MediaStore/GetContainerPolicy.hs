{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.GetContainerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the access policy for the specified container. For information about the data that is included in an access policy, see the <https://aws.amazon.com/documentation/iam/ AWS Identity and Access Management User Guide> .
module Network.AWS.MediaStore.GetContainerPolicy
  ( -- * Creating a request
    GetContainerPolicy (..),
    mkGetContainerPolicy,

    -- ** Request lenses
    gcpContainerName,

    -- * Destructuring the response
    GetContainerPolicyResponse (..),
    mkGetContainerPolicyResponse,

    -- ** Response lenses
    grsPolicy,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContainerPolicy' smart constructor.
newtype GetContainerPolicy = GetContainerPolicy'
  { -- | The name of the container.
    containerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContainerPolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container.
mkGetContainerPolicy ::
  -- | 'containerName'
  Lude.Text ->
  GetContainerPolicy
mkGetContainerPolicy pContainerName_ =
  GetContainerPolicy' {containerName = pContainerName_}

-- | The name of the container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpContainerName :: Lens.Lens' GetContainerPolicy Lude.Text
gcpContainerName = Lens.lens (containerName :: GetContainerPolicy -> Lude.Text) (\s a -> s {containerName = a} :: GetContainerPolicy)
{-# DEPRECATED gcpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest GetContainerPolicy where
  type Rs GetContainerPolicy = GetContainerPolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContainerPolicyResponse'
            Lude.<$> (x Lude..:> "Policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContainerPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.GetContainerPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContainerPolicy where
  toJSON GetContainerPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath GetContainerPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContainerPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContainerPolicyResponse' smart constructor.
data GetContainerPolicyResponse = GetContainerPolicyResponse'
  { -- | The contents of the access policy.
    policy :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContainerPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - The contents of the access policy.
-- * 'responseStatus' - The response status code.
mkGetContainerPolicyResponse ::
  -- | 'policy'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetContainerPolicyResponse
mkGetContainerPolicyResponse pPolicy_ pResponseStatus_ =
  GetContainerPolicyResponse'
    { policy = pPolicy_,
      responseStatus = pResponseStatus_
    }

-- | The contents of the access policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsPolicy :: Lens.Lens' GetContainerPolicyResponse Lude.Text
grsPolicy = Lens.lens (policy :: GetContainerPolicyResponse -> Lude.Text) (\s a -> s {policy = a} :: GetContainerPolicyResponse)
{-# DEPRECATED grsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetContainerPolicyResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetContainerPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContainerPolicyResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
