{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific identity provider.
module Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
  ( -- * Creating a request
    DescribeIdentityProvider (..),
    mkDescribeIdentityProvider,

    -- ** Request lenses
    dipfUserPoolId,
    dipfProviderName,

    -- * Destructuring the response
    DescribeIdentityProviderResponse (..),
    mkDescribeIdentityProviderResponse,

    -- ** Response lenses
    diprsIdentityProvider,
    diprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeIdentityProvider' smart constructor.
data DescribeIdentityProvider = DescribeIdentityProvider'
  { -- | The user pool ID.
    userPoolId :: Lude.Text,
    -- | The identity provider name.
    providerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdentityProvider' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID.
-- * 'providerName' - The identity provider name.
mkDescribeIdentityProvider ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'providerName'
  Lude.Text ->
  DescribeIdentityProvider
mkDescribeIdentityProvider pUserPoolId_ pProviderName_ =
  DescribeIdentityProvider'
    { userPoolId = pUserPoolId_,
      providerName = pProviderName_
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipfUserPoolId :: Lens.Lens' DescribeIdentityProvider Lude.Text
dipfUserPoolId = Lens.lens (userPoolId :: DescribeIdentityProvider -> Lude.Text) (\s a -> s {userPoolId = a} :: DescribeIdentityProvider)
{-# DEPRECATED dipfUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipfProviderName :: Lens.Lens' DescribeIdentityProvider Lude.Text
dipfProviderName = Lens.lens (providerName :: DescribeIdentityProvider -> Lude.Text) (\s a -> s {providerName = a} :: DescribeIdentityProvider)
{-# DEPRECATED dipfProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

instance Lude.AWSRequest DescribeIdentityProvider where
  type Rs DescribeIdentityProvider = DescribeIdentityProviderResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeIdentityProviderResponse'
            Lude.<$> (x Lude..:> "IdentityProvider")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeIdentityProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DescribeIdentityProvider" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeIdentityProvider where
  toJSON DescribeIdentityProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("ProviderName" Lude..= providerName)
          ]
      )

instance Lude.ToPath DescribeIdentityProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeIdentityProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeIdentityProviderResponse' smart constructor.
data DescribeIdentityProviderResponse = DescribeIdentityProviderResponse'
  { -- | The identity provider that was deleted.
    identityProvider :: IdentityProviderType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdentityProviderResponse' with the minimum fields required to make a request.
--
-- * 'identityProvider' - The identity provider that was deleted.
-- * 'responseStatus' - The response status code.
mkDescribeIdentityProviderResponse ::
  -- | 'identityProvider'
  IdentityProviderType ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeIdentityProviderResponse
mkDescribeIdentityProviderResponse
  pIdentityProvider_
  pResponseStatus_ =
    DescribeIdentityProviderResponse'
      { identityProvider =
          pIdentityProvider_,
        responseStatus = pResponseStatus_
      }

-- | The identity provider that was deleted.
--
-- /Note:/ Consider using 'identityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsIdentityProvider :: Lens.Lens' DescribeIdentityProviderResponse IdentityProviderType
diprsIdentityProvider = Lens.lens (identityProvider :: DescribeIdentityProviderResponse -> IdentityProviderType) (\s a -> s {identityProvider = a} :: DescribeIdentityProviderResponse)
{-# DEPRECATED diprsIdentityProvider "Use generic-lens or generic-optics with 'identityProvider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsResponseStatus :: Lens.Lens' DescribeIdentityProviderResponse Lude.Int
diprsResponseStatus = Lens.lens (responseStatus :: DescribeIdentityProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeIdentityProviderResponse)
{-# DEPRECATED diprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
