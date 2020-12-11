{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.PutCORSPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the cross-origin resource sharing (CORS) configuration on a container so that the container can service cross-origin requests. For example, you might want to enable a request whose origin is http://www.example.com to access your AWS Elemental MediaStore container at my.example.container.com by using the browser's XMLHttpRequest capability.
--
-- To enable CORS on a container, you attach a CORS policy to the container. In the CORS policy, you configure rules that identify origins and the HTTP methods that can be executed on your container. The policy can contain up to 398,000 characters. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.
-- To learn more about CORS, see <https://docs.aws.amazon.com/mediastore/latest/ug/cors-policy.html Cross-Origin Resource Sharing (CORS) in AWS Elemental MediaStore> .
module Network.AWS.MediaStore.PutCORSPolicy
  ( -- * Creating a request
    PutCORSPolicy (..),
    mkPutCORSPolicy,

    -- ** Request lenses
    pcpContainerName,
    pcpCORSPolicy,

    -- * Destructuring the response
    PutCORSPolicyResponse (..),
    mkPutCORSPolicyResponse,

    -- ** Response lenses
    pcorsprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutCORSPolicy' smart constructor.
data PutCORSPolicy = PutCORSPolicy'
  { containerName :: Lude.Text,
    corsPolicy :: Lude.NonEmpty CORSRule
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutCORSPolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that you want to assign the CORS policy to.
-- * 'corsPolicy' - The CORS policy to apply to the container.
mkPutCORSPolicy ::
  -- | 'containerName'
  Lude.Text ->
  -- | 'corsPolicy'
  Lude.NonEmpty CORSRule ->
  PutCORSPolicy
mkPutCORSPolicy pContainerName_ pCORSPolicy_ =
  PutCORSPolicy'
    { containerName = pContainerName_,
      corsPolicy = pCORSPolicy_
    }

-- | The name of the container that you want to assign the CORS policy to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpContainerName :: Lens.Lens' PutCORSPolicy Lude.Text
pcpContainerName = Lens.lens (containerName :: PutCORSPolicy -> Lude.Text) (\s a -> s {containerName = a} :: PutCORSPolicy)
{-# DEPRECATED pcpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The CORS policy to apply to the container.
--
-- /Note:/ Consider using 'corsPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpCORSPolicy :: Lens.Lens' PutCORSPolicy (Lude.NonEmpty CORSRule)
pcpCORSPolicy = Lens.lens (corsPolicy :: PutCORSPolicy -> Lude.NonEmpty CORSRule) (\s a -> s {corsPolicy = a} :: PutCORSPolicy)
{-# DEPRECATED pcpCORSPolicy "Use generic-lens or generic-optics with 'corsPolicy' instead." #-}

instance Lude.AWSRequest PutCORSPolicy where
  type Rs PutCORSPolicy = PutCORSPolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutCORSPolicyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutCORSPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.PutCorsPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutCORSPolicy where
  toJSON PutCORSPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ContainerName" Lude..= containerName),
            Lude.Just ("CorsPolicy" Lude..= corsPolicy)
          ]
      )

instance Lude.ToPath PutCORSPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutCORSPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutCORSPolicyResponse' smart constructor.
newtype PutCORSPolicyResponse = PutCORSPolicyResponse'
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

-- | Creates a value of 'PutCORSPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutCORSPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutCORSPolicyResponse
mkPutCORSPolicyResponse pResponseStatus_ =
  PutCORSPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcorsprsResponseStatus :: Lens.Lens' PutCORSPolicyResponse Lude.Int
pcorsprsResponseStatus = Lens.lens (responseStatus :: PutCORSPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutCORSPolicyResponse)
{-# DEPRECATED pcorsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
