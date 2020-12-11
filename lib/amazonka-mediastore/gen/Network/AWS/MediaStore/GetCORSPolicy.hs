{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.GetCORSPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cross-origin resource sharing (CORS) configuration information that is set for the container.
--
-- To use this operation, you must have permission to perform the @MediaStore:GetCorsPolicy@ action. By default, the container owner has this permission and can grant it to others.
module Network.AWS.MediaStore.GetCORSPolicy
  ( -- * Creating a request
    GetCORSPolicy (..),
    mkGetCORSPolicy,

    -- ** Request lenses
    gcpContainerName,

    -- * Destructuring the response
    GetCORSPolicyResponse (..),
    mkGetCORSPolicyResponse,

    -- ** Response lenses
    gcorsprsResponseStatus,
    gcorsprsCORSPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCORSPolicy' smart constructor.
newtype GetCORSPolicy = GetCORSPolicy' {containerName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCORSPolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that the policy is assigned to.
mkGetCORSPolicy ::
  -- | 'containerName'
  Lude.Text ->
  GetCORSPolicy
mkGetCORSPolicy pContainerName_ =
  GetCORSPolicy' {containerName = pContainerName_}

-- | The name of the container that the policy is assigned to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpContainerName :: Lens.Lens' GetCORSPolicy Lude.Text
gcpContainerName = Lens.lens (containerName :: GetCORSPolicy -> Lude.Text) (\s a -> s {containerName = a} :: GetCORSPolicy)
{-# DEPRECATED gcpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest GetCORSPolicy where
  type Rs GetCORSPolicy = GetCORSPolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCORSPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "CorsPolicy")
      )

instance Lude.ToHeaders GetCORSPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.GetCorsPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCORSPolicy where
  toJSON GetCORSPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath GetCORSPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCORSPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCORSPolicyResponse' smart constructor.
data GetCORSPolicyResponse = GetCORSPolicyResponse'
  { responseStatus ::
      Lude.Int,
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

-- | Creates a value of 'GetCORSPolicyResponse' with the minimum fields required to make a request.
--
-- * 'corsPolicy' - The CORS policy assigned to the container.
-- * 'responseStatus' - The response status code.
mkGetCORSPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'corsPolicy'
  Lude.NonEmpty CORSRule ->
  GetCORSPolicyResponse
mkGetCORSPolicyResponse pResponseStatus_ pCORSPolicy_ =
  GetCORSPolicyResponse'
    { responseStatus = pResponseStatus_,
      corsPolicy = pCORSPolicy_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcorsprsResponseStatus :: Lens.Lens' GetCORSPolicyResponse Lude.Int
gcorsprsResponseStatus = Lens.lens (responseStatus :: GetCORSPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCORSPolicyResponse)
{-# DEPRECATED gcorsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The CORS policy assigned to the container.
--
-- /Note:/ Consider using 'corsPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcorsprsCORSPolicy :: Lens.Lens' GetCORSPolicyResponse (Lude.NonEmpty CORSRule)
gcorsprsCORSPolicy = Lens.lens (corsPolicy :: GetCORSPolicyResponse -> Lude.NonEmpty CORSRule) (\s a -> s {corsPolicy = a} :: GetCORSPolicyResponse)
{-# DEPRECATED gcorsprsCORSPolicy "Use generic-lens or generic-optics with 'corsPolicy' instead." #-}
