{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetDefaultAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the default authorizer. This will be used if a websocket connection is made without specifying an authorizer.
module Network.AWS.IoT.SetDefaultAuthorizer
  ( -- * Creating a request
    SetDefaultAuthorizer (..),
    mkSetDefaultAuthorizer,

    -- ** Request lenses
    sdaAuthorizerName,

    -- * Destructuring the response
    SetDefaultAuthorizerResponse (..),
    mkSetDefaultAuthorizerResponse,

    -- ** Response lenses
    sdarsAuthorizerName,
    sdarsAuthorizerARN,
    sdarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetDefaultAuthorizer' smart constructor.
newtype SetDefaultAuthorizer = SetDefaultAuthorizer'
  { authorizerName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetDefaultAuthorizer' with the minimum fields required to make a request.
--
-- * 'authorizerName' - The authorizer name.
mkSetDefaultAuthorizer ::
  -- | 'authorizerName'
  Lude.Text ->
  SetDefaultAuthorizer
mkSetDefaultAuthorizer pAuthorizerName_ =
  SetDefaultAuthorizer' {authorizerName = pAuthorizerName_}

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdaAuthorizerName :: Lens.Lens' SetDefaultAuthorizer Lude.Text
sdaAuthorizerName = Lens.lens (authorizerName :: SetDefaultAuthorizer -> Lude.Text) (\s a -> s {authorizerName = a} :: SetDefaultAuthorizer)
{-# DEPRECATED sdaAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

instance Lude.AWSRequest SetDefaultAuthorizer where
  type Rs SetDefaultAuthorizer = SetDefaultAuthorizerResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          SetDefaultAuthorizerResponse'
            Lude.<$> (x Lude..?> "authorizerName")
            Lude.<*> (x Lude..?> "authorizerArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetDefaultAuthorizer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SetDefaultAuthorizer where
  toJSON SetDefaultAuthorizer' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("authorizerName" Lude..= authorizerName)]
      )

instance Lude.ToPath SetDefaultAuthorizer where
  toPath = Lude.const "/default-authorizer"

instance Lude.ToQuery SetDefaultAuthorizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetDefaultAuthorizerResponse' smart constructor.
data SetDefaultAuthorizerResponse = SetDefaultAuthorizerResponse'
  { authorizerName ::
      Lude.Maybe Lude.Text,
    authorizerARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'SetDefaultAuthorizerResponse' with the minimum fields required to make a request.
--
-- * 'authorizerARN' - The authorizer ARN.
-- * 'authorizerName' - The authorizer name.
-- * 'responseStatus' - The response status code.
mkSetDefaultAuthorizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetDefaultAuthorizerResponse
mkSetDefaultAuthorizerResponse pResponseStatus_ =
  SetDefaultAuthorizerResponse'
    { authorizerName = Lude.Nothing,
      authorizerARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdarsAuthorizerName :: Lens.Lens' SetDefaultAuthorizerResponse (Lude.Maybe Lude.Text)
sdarsAuthorizerName = Lens.lens (authorizerName :: SetDefaultAuthorizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {authorizerName = a} :: SetDefaultAuthorizerResponse)
{-# DEPRECATED sdarsAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

-- | The authorizer ARN.
--
-- /Note:/ Consider using 'authorizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdarsAuthorizerARN :: Lens.Lens' SetDefaultAuthorizerResponse (Lude.Maybe Lude.Text)
sdarsAuthorizerARN = Lens.lens (authorizerARN :: SetDefaultAuthorizerResponse -> Lude.Maybe Lude.Text) (\s a -> s {authorizerARN = a} :: SetDefaultAuthorizerResponse)
{-# DEPRECATED sdarsAuthorizerARN "Use generic-lens or generic-optics with 'authorizerARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdarsResponseStatus :: Lens.Lens' SetDefaultAuthorizerResponse Lude.Int
sdarsResponseStatus = Lens.lens (responseStatus :: SetDefaultAuthorizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetDefaultAuthorizerResponse)
{-# DEPRECATED sdarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
