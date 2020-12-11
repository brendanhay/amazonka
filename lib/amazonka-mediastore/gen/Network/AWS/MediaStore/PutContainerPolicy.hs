{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.PutContainerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an access policy for the specified container to restrict the users and clients that can access it. For information about the data that is included in an access policy, see the <https://aws.amazon.com/documentation/iam/ AWS Identity and Access Management User Guide> .
--
-- For this release of the REST API, you can create only one policy for a container. If you enter @PutContainerPolicy@ twice, the second command modifies the existing policy.
module Network.AWS.MediaStore.PutContainerPolicy
  ( -- * Creating a request
    PutContainerPolicy (..),
    mkPutContainerPolicy,

    -- ** Request lenses
    pContainerName,
    pPolicy,

    -- * Destructuring the response
    PutContainerPolicyResponse (..),
    mkPutContainerPolicyResponse,

    -- ** Response lenses
    pcprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutContainerPolicy' smart constructor.
data PutContainerPolicy = PutContainerPolicy'
  { containerName ::
      Lude.Text,
    policy :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutContainerPolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container.
-- * 'policy' - The contents of the policy, which includes the following:
--
--
--     * One @Version@ tag
--
--
--     * One @Statement@ tag that contains the standard tags for the policy.
mkPutContainerPolicy ::
  -- | 'containerName'
  Lude.Text ->
  -- | 'policy'
  Lude.Text ->
  PutContainerPolicy
mkPutContainerPolicy pContainerName_ pPolicy_ =
  PutContainerPolicy'
    { containerName = pContainerName_,
      policy = pPolicy_
    }

-- | The name of the container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pContainerName :: Lens.Lens' PutContainerPolicy Lude.Text
pContainerName = Lens.lens (containerName :: PutContainerPolicy -> Lude.Text) (\s a -> s {containerName = a} :: PutContainerPolicy)
{-# DEPRECATED pContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The contents of the policy, which includes the following:
--
--
--     * One @Version@ tag
--
--
--     * One @Statement@ tag that contains the standard tags for the policy.
--
--
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicy :: Lens.Lens' PutContainerPolicy Lude.Text
pPolicy = Lens.lens (policy :: PutContainerPolicy -> Lude.Text) (\s a -> s {policy = a} :: PutContainerPolicy)
{-# DEPRECATED pPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.AWSRequest PutContainerPolicy where
  type Rs PutContainerPolicy = PutContainerPolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutContainerPolicyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutContainerPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.PutContainerPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutContainerPolicy where
  toJSON PutContainerPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ContainerName" Lude..= containerName),
            Lude.Just ("Policy" Lude..= policy)
          ]
      )

instance Lude.ToPath PutContainerPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutContainerPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutContainerPolicyResponse' smart constructor.
newtype PutContainerPolicyResponse = PutContainerPolicyResponse'
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

-- | Creates a value of 'PutContainerPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutContainerPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutContainerPolicyResponse
mkPutContainerPolicyResponse pResponseStatus_ =
  PutContainerPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprsResponseStatus :: Lens.Lens' PutContainerPolicyResponse Lude.Int
pcprsResponseStatus = Lens.lens (responseStatus :: PutContainerPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutContainerPolicyResponse)
{-# DEPRECATED pcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
