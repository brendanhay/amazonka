{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an authorizer.
module Network.AWS.IoT.CreateAuthorizer
  ( -- * Creating a request
    CreateAuthorizer (..),
    mkCreateAuthorizer,

    -- ** Request lenses
    caStatus,
    caSigningDisabled,
    caTokenSigningPublicKeys,
    caTokenKeyName,
    caTags,
    caAuthorizerName,
    caAuthorizerFunctionARN,

    -- * Destructuring the response
    CreateAuthorizerResponse (..),
    mkCreateAuthorizerResponse,

    -- ** Response lenses
    carsAuthorizerName,
    carsAuthorizerARN,
    carsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { status ::
      Lude.Maybe AuthorizerStatus,
    signingDisabled :: Lude.Maybe Lude.Bool,
    tokenSigningPublicKeys ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    tokenKeyName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    authorizerName :: Lude.Text,
    authorizerFunctionARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAuthorizer' with the minimum fields required to make a request.
--
-- * 'authorizerFunctionARN' - The ARN of the authorizer's Lambda function.
-- * 'authorizerName' - The authorizer name.
-- * 'signingDisabled' - Specifies whether AWS IoT validates the token signature in an authorization request.
-- * 'status' - The status of the create authorizer request.
-- * 'tags' - Metadata which can be used to manage the custom authorizer.
-- * 'tokenKeyName' - The name of the token key used to extract the token from the HTTP headers.
-- * 'tokenSigningPublicKeys' - The public keys used to verify the digital signature returned by your custom authentication service.
mkCreateAuthorizer ::
  -- | 'authorizerName'
  Lude.Text ->
  -- | 'authorizerFunctionARN'
  Lude.Text ->
  CreateAuthorizer
mkCreateAuthorizer pAuthorizerName_ pAuthorizerFunctionARN_ =
  CreateAuthorizer'
    { status = Lude.Nothing,
      signingDisabled = Lude.Nothing,
      tokenSigningPublicKeys = Lude.Nothing,
      tokenKeyName = Lude.Nothing,
      tags = Lude.Nothing,
      authorizerName = pAuthorizerName_,
      authorizerFunctionARN = pAuthorizerFunctionARN_
    }

-- | The status of the create authorizer request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStatus :: Lens.Lens' CreateAuthorizer (Lude.Maybe AuthorizerStatus)
caStatus = Lens.lens (status :: CreateAuthorizer -> Lude.Maybe AuthorizerStatus) (\s a -> s {status = a} :: CreateAuthorizer)
{-# DEPRECATED caStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies whether AWS IoT validates the token signature in an authorization request.
--
-- /Note:/ Consider using 'signingDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSigningDisabled :: Lens.Lens' CreateAuthorizer (Lude.Maybe Lude.Bool)
caSigningDisabled = Lens.lens (signingDisabled :: CreateAuthorizer -> Lude.Maybe Lude.Bool) (\s a -> s {signingDisabled = a} :: CreateAuthorizer)
{-# DEPRECATED caSigningDisabled "Use generic-lens or generic-optics with 'signingDisabled' instead." #-}

-- | The public keys used to verify the digital signature returned by your custom authentication service.
--
-- /Note:/ Consider using 'tokenSigningPublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTokenSigningPublicKeys :: Lens.Lens' CreateAuthorizer (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
caTokenSigningPublicKeys = Lens.lens (tokenSigningPublicKeys :: CreateAuthorizer -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tokenSigningPublicKeys = a} :: CreateAuthorizer)
{-# DEPRECATED caTokenSigningPublicKeys "Use generic-lens or generic-optics with 'tokenSigningPublicKeys' instead." #-}

-- | The name of the token key used to extract the token from the HTTP headers.
--
-- /Note:/ Consider using 'tokenKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTokenKeyName :: Lens.Lens' CreateAuthorizer (Lude.Maybe Lude.Text)
caTokenKeyName = Lens.lens (tokenKeyName :: CreateAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {tokenKeyName = a} :: CreateAuthorizer)
{-# DEPRECATED caTokenKeyName "Use generic-lens or generic-optics with 'tokenKeyName' instead." #-}

-- | Metadata which can be used to manage the custom authorizer.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateAuthorizer (Lude.Maybe [Tag])
caTags = Lens.lens (tags :: CreateAuthorizer -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateAuthorizer)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAuthorizerName :: Lens.Lens' CreateAuthorizer Lude.Text
caAuthorizerName = Lens.lens (authorizerName :: CreateAuthorizer -> Lude.Text) (\s a -> s {authorizerName = a} :: CreateAuthorizer)
{-# DEPRECATED caAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

-- | The ARN of the authorizer's Lambda function.
--
-- /Note:/ Consider using 'authorizerFunctionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAuthorizerFunctionARN :: Lens.Lens' CreateAuthorizer Lude.Text
caAuthorizerFunctionARN = Lens.lens (authorizerFunctionARN :: CreateAuthorizer -> Lude.Text) (\s a -> s {authorizerFunctionARN = a} :: CreateAuthorizer)
{-# DEPRECATED caAuthorizerFunctionARN "Use generic-lens or generic-optics with 'authorizerFunctionARN' instead." #-}

instance Lude.AWSRequest CreateAuthorizer where
  type Rs CreateAuthorizer = CreateAuthorizerResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAuthorizerResponse'
            Lude.<$> (x Lude..?> "authorizerName")
            Lude.<*> (x Lude..?> "authorizerArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAuthorizer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateAuthorizer where
  toJSON CreateAuthorizer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("signingDisabled" Lude..=) Lude.<$> signingDisabled,
            ("tokenSigningPublicKeys" Lude..=) Lude.<$> tokenSigningPublicKeys,
            ("tokenKeyName" Lude..=) Lude.<$> tokenKeyName,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("authorizerFunctionArn" Lude..= authorizerFunctionARN)
          ]
      )

instance Lude.ToPath CreateAuthorizer where
  toPath CreateAuthorizer' {..} =
    Lude.mconcat ["/authorizer/", Lude.toBS authorizerName]

instance Lude.ToQuery CreateAuthorizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAuthorizerResponse' smart constructor.
data CreateAuthorizerResponse = CreateAuthorizerResponse'
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

-- | Creates a value of 'CreateAuthorizerResponse' with the minimum fields required to make a request.
--
-- * 'authorizerARN' - The authorizer ARN.
-- * 'authorizerName' - The authorizer's name.
-- * 'responseStatus' - The response status code.
mkCreateAuthorizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAuthorizerResponse
mkCreateAuthorizerResponse pResponseStatus_ =
  CreateAuthorizerResponse'
    { authorizerName = Lude.Nothing,
      authorizerARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The authorizer's name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsAuthorizerName :: Lens.Lens' CreateAuthorizerResponse (Lude.Maybe Lude.Text)
carsAuthorizerName = Lens.lens (authorizerName :: CreateAuthorizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {authorizerName = a} :: CreateAuthorizerResponse)
{-# DEPRECATED carsAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

-- | The authorizer ARN.
--
-- /Note:/ Consider using 'authorizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsAuthorizerARN :: Lens.Lens' CreateAuthorizerResponse (Lude.Maybe Lude.Text)
carsAuthorizerARN = Lens.lens (authorizerARN :: CreateAuthorizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {authorizerARN = a} :: CreateAuthorizerResponse)
{-# DEPRECATED carsAuthorizerARN "Use generic-lens or generic-optics with 'authorizerARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAuthorizerResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAuthorizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAuthorizerResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
