{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gcorspContainerName,

    -- * Destructuring the response
    GetCORSPolicyResponse (..),
    mkGetCORSPolicyResponse,

    -- ** Response lenses
    gcprsCORSPolicy,
    gcprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCORSPolicy' smart constructor.
newtype GetCORSPolicy = GetCORSPolicy'
  { -- | The name of the container that the policy is assigned to.
    containerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
gcorspContainerName :: Lens.Lens' GetCORSPolicy Lude.Text
gcorspContainerName = Lens.lens (containerName :: GetCORSPolicy -> Lude.Text) (\s a -> s {containerName = a} :: GetCORSPolicy)
{-# DEPRECATED gcorspContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest GetCORSPolicy where
  type Rs GetCORSPolicy = GetCORSPolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCORSPolicyResponse'
            Lude.<$> (x Lude..:> "CorsPolicy") Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | The CORS policy assigned to the container.
    corsPolicy :: Lude.NonEmpty CORSRule,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCORSPolicyResponse' with the minimum fields required to make a request.
--
-- * 'corsPolicy' - The CORS policy assigned to the container.
-- * 'responseStatus' - The response status code.
mkGetCORSPolicyResponse ::
  -- | 'corsPolicy'
  Lude.NonEmpty CORSRule ->
  -- | 'responseStatus'
  Lude.Int ->
  GetCORSPolicyResponse
mkGetCORSPolicyResponse pCORSPolicy_ pResponseStatus_ =
  GetCORSPolicyResponse'
    { corsPolicy = pCORSPolicy_,
      responseStatus = pResponseStatus_
    }

-- | The CORS policy assigned to the container.
--
-- /Note:/ Consider using 'corsPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprsCORSPolicy :: Lens.Lens' GetCORSPolicyResponse (Lude.NonEmpty CORSRule)
gcprsCORSPolicy = Lens.lens (corsPolicy :: GetCORSPolicyResponse -> Lude.NonEmpty CORSRule) (\s a -> s {corsPolicy = a} :: GetCORSPolicyResponse)
{-# DEPRECATED gcprsCORSPolicy "Use generic-lens or generic-optics with 'corsPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprsResponseStatus :: Lens.Lens' GetCORSPolicyResponse Lude.Int
gcprsResponseStatus = Lens.lens (responseStatus :: GetCORSPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCORSPolicyResponse)
{-# DEPRECATED gcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
