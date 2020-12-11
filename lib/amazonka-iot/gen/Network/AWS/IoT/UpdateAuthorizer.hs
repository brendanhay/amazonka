{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an authorizer.
module Network.AWS.IoT.UpdateAuthorizer
  ( -- * Creating a request
    UpdateAuthorizer (..),
    mkUpdateAuthorizer,

    -- ** Request lenses
    uaStatus,
    uaAuthorizerFunctionARN,
    uaTokenSigningPublicKeys,
    uaTokenKeyName,
    uaAuthorizerName,

    -- * Destructuring the response
    UpdateAuthorizerResponse (..),
    mkUpdateAuthorizerResponse,

    -- ** Response lenses
    uarsAuthorizerName,
    uarsAuthorizerARN,
    uarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'
  { status ::
      Lude.Maybe AuthorizerStatus,
    authorizerFunctionARN :: Lude.Maybe Lude.Text,
    tokenSigningPublicKeys ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    tokenKeyName :: Lude.Maybe Lude.Text,
    authorizerName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAuthorizer' with the minimum fields required to make a request.
--
-- * 'authorizerFunctionARN' - The ARN of the authorizer's Lambda function.
-- * 'authorizerName' - The authorizer name.
-- * 'status' - The status of the update authorizer request.
-- * 'tokenKeyName' - The key used to extract the token from the HTTP headers.
-- * 'tokenSigningPublicKeys' - The public keys used to verify the token signature.
mkUpdateAuthorizer ::
  -- | 'authorizerName'
  Lude.Text ->
  UpdateAuthorizer
mkUpdateAuthorizer pAuthorizerName_ =
  UpdateAuthorizer'
    { status = Lude.Nothing,
      authorizerFunctionARN = Lude.Nothing,
      tokenSigningPublicKeys = Lude.Nothing,
      tokenKeyName = Lude.Nothing,
      authorizerName = pAuthorizerName_
    }

-- | The status of the update authorizer request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaStatus :: Lens.Lens' UpdateAuthorizer (Lude.Maybe AuthorizerStatus)
uaStatus = Lens.lens (status :: UpdateAuthorizer -> Lude.Maybe AuthorizerStatus) (\s a -> s {status = a} :: UpdateAuthorizer)
{-# DEPRECATED uaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the authorizer's Lambda function.
--
-- /Note:/ Consider using 'authorizerFunctionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAuthorizerFunctionARN :: Lens.Lens' UpdateAuthorizer (Lude.Maybe Lude.Text)
uaAuthorizerFunctionARN = Lens.lens (authorizerFunctionARN :: UpdateAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {authorizerFunctionARN = a} :: UpdateAuthorizer)
{-# DEPRECATED uaAuthorizerFunctionARN "Use generic-lens or generic-optics with 'authorizerFunctionARN' instead." #-}

-- | The public keys used to verify the token signature.
--
-- /Note:/ Consider using 'tokenSigningPublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTokenSigningPublicKeys :: Lens.Lens' UpdateAuthorizer (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uaTokenSigningPublicKeys = Lens.lens (tokenSigningPublicKeys :: UpdateAuthorizer -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tokenSigningPublicKeys = a} :: UpdateAuthorizer)
{-# DEPRECATED uaTokenSigningPublicKeys "Use generic-lens or generic-optics with 'tokenSigningPublicKeys' instead." #-}

-- | The key used to extract the token from the HTTP headers.
--
-- /Note:/ Consider using 'tokenKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTokenKeyName :: Lens.Lens' UpdateAuthorizer (Lude.Maybe Lude.Text)
uaTokenKeyName = Lens.lens (tokenKeyName :: UpdateAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {tokenKeyName = a} :: UpdateAuthorizer)
{-# DEPRECATED uaTokenKeyName "Use generic-lens or generic-optics with 'tokenKeyName' instead." #-}

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAuthorizerName :: Lens.Lens' UpdateAuthorizer Lude.Text
uaAuthorizerName = Lens.lens (authorizerName :: UpdateAuthorizer -> Lude.Text) (\s a -> s {authorizerName = a} :: UpdateAuthorizer)
{-# DEPRECATED uaAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

instance Lude.AWSRequest UpdateAuthorizer where
  type Rs UpdateAuthorizer = UpdateAuthorizerResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAuthorizerResponse'
            Lude.<$> (x Lude..?> "authorizerName")
            Lude.<*> (x Lude..?> "authorizerArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAuthorizer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateAuthorizer where
  toJSON UpdateAuthorizer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("authorizerFunctionArn" Lude..=) Lude.<$> authorizerFunctionARN,
            ("tokenSigningPublicKeys" Lude..=) Lude.<$> tokenSigningPublicKeys,
            ("tokenKeyName" Lude..=) Lude.<$> tokenKeyName
          ]
      )

instance Lude.ToPath UpdateAuthorizer where
  toPath UpdateAuthorizer' {..} =
    Lude.mconcat ["/authorizer/", Lude.toBS authorizerName]

instance Lude.ToQuery UpdateAuthorizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAuthorizerResponse' smart constructor.
data UpdateAuthorizerResponse = UpdateAuthorizerResponse'
  { authorizerName ::
      Lude.Maybe Lude.Text,
    authorizerARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateAuthorizerResponse' with the minimum fields required to make a request.
--
-- * 'authorizerARN' - The authorizer ARN.
-- * 'authorizerName' - The authorizer name.
-- * 'responseStatus' - The response status code.
mkUpdateAuthorizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAuthorizerResponse
mkUpdateAuthorizerResponse pResponseStatus_ =
  UpdateAuthorizerResponse'
    { authorizerName = Lude.Nothing,
      authorizerARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsAuthorizerName :: Lens.Lens' UpdateAuthorizerResponse (Lude.Maybe Lude.Text)
uarsAuthorizerName = Lens.lens (authorizerName :: UpdateAuthorizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {authorizerName = a} :: UpdateAuthorizerResponse)
{-# DEPRECATED uarsAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

-- | The authorizer ARN.
--
-- /Note:/ Consider using 'authorizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsAuthorizerARN :: Lens.Lens' UpdateAuthorizerResponse (Lude.Maybe Lude.Text)
uarsAuthorizerARN = Lens.lens (authorizerARN :: UpdateAuthorizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {authorizerARN = a} :: UpdateAuthorizerResponse)
{-# DEPRECATED uarsAuthorizerARN "Use generic-lens or generic-optics with 'authorizerARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsResponseStatus :: Lens.Lens' UpdateAuthorizerResponse Lude.Int
uarsResponseStatus = Lens.lens (responseStatus :: UpdateAuthorizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAuthorizerResponse)
{-# DEPRECATED uarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
