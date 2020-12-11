{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetAuthorizationToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an authorization token. An authorization token represents your IAM authentication credentials and can be used to access any Amazon ECR registry that your IAM principal has access to. The authorization token is valid for 12 hours.
--
-- The @authorizationToken@ returned is a base64 encoded string that can be decoded and used in a @docker login@ command to authenticate to a registry. The AWS CLI offers an @get-login-password@ command that simplifies the login process. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/Registries.html#registry_auth Registry Authentication> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.GetAuthorizationToken
  ( -- * Creating a request
    GetAuthorizationToken (..),
    mkGetAuthorizationToken,

    -- ** Request lenses
    gatRegistryIds,

    -- * Destructuring the response
    GetAuthorizationTokenResponse (..),
    mkGetAuthorizationTokenResponse,

    -- ** Response lenses
    gatrsAuthorizationData,
    gatrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAuthorizationToken' smart constructor.
newtype GetAuthorizationToken = GetAuthorizationToken'
  { registryIds ::
      Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAuthorizationToken' with the minimum fields required to make a request.
--
-- * 'registryIds' - A list of AWS account IDs that are associated with the registries for which to get AuthorizationData objects. If you do not specify a registry, the default registry is assumed.
mkGetAuthorizationToken ::
  GetAuthorizationToken
mkGetAuthorizationToken =
  GetAuthorizationToken' {registryIds = Lude.Nothing}

-- | A list of AWS account IDs that are associated with the registries for which to get AuthorizationData objects. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatRegistryIds :: Lens.Lens' GetAuthorizationToken (Lude.Maybe (Lude.NonEmpty Lude.Text))
gatRegistryIds = Lens.lens (registryIds :: GetAuthorizationToken -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {registryIds = a} :: GetAuthorizationToken)
{-# DEPRECATED gatRegistryIds "Use generic-lens or generic-optics with 'registryIds' instead." #-}

instance Lude.AWSRequest GetAuthorizationToken where
  type Rs GetAuthorizationToken = GetAuthorizationTokenResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAuthorizationTokenResponse'
            Lude.<$> (x Lude..?> "authorizationData" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAuthorizationToken where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.GetAuthorizationToken" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAuthorizationToken where
  toJSON GetAuthorizationToken' {..} =
    Lude.object
      (Lude.catMaybes [("registryIds" Lude..=) Lude.<$> registryIds])

instance Lude.ToPath GetAuthorizationToken where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAuthorizationToken where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAuthorizationTokenResponse' smart constructor.
data GetAuthorizationTokenResponse = GetAuthorizationTokenResponse'
  { authorizationData ::
      Lude.Maybe [AuthorizationData],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAuthorizationTokenResponse' with the minimum fields required to make a request.
--
-- * 'authorizationData' - A list of authorization token data objects that correspond to the @registryIds@ values in the request.
-- * 'responseStatus' - The response status code.
mkGetAuthorizationTokenResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAuthorizationTokenResponse
mkGetAuthorizationTokenResponse pResponseStatus_ =
  GetAuthorizationTokenResponse'
    { authorizationData = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of authorization token data objects that correspond to the @registryIds@ values in the request.
--
-- /Note:/ Consider using 'authorizationData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatrsAuthorizationData :: Lens.Lens' GetAuthorizationTokenResponse (Lude.Maybe [AuthorizationData])
gatrsAuthorizationData = Lens.lens (authorizationData :: GetAuthorizationTokenResponse -> Lude.Maybe [AuthorizationData]) (\s a -> s {authorizationData = a} :: GetAuthorizationTokenResponse)
{-# DEPRECATED gatrsAuthorizationData "Use generic-lens or generic-optics with 'authorizationData' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatrsResponseStatus :: Lens.Lens' GetAuthorizationTokenResponse Lude.Int
gatrsResponseStatus = Lens.lens (responseStatus :: GetAuthorizationTokenResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAuthorizationTokenResponse)
{-# DEPRECATED gatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
