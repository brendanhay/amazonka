{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies the specified user attributes in the user pool.
module Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
  ( -- * Creating a request
    VerifyUserAttribute (..),
    mkVerifyUserAttribute,

    -- ** Request lenses
    vuaAccessToken,
    vuaAttributeName,
    vuaCode,

    -- * Destructuring the response
    VerifyUserAttributeResponse (..),
    mkVerifyUserAttributeResponse,

    -- ** Response lenses
    vuarsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to verify user attributes.
--
-- /See:/ 'mkVerifyUserAttribute' smart constructor.
data VerifyUserAttribute = VerifyUserAttribute'
  { accessToken ::
      Lude.Sensitive Lude.Text,
    attributeName :: Lude.Text,
    code :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VerifyUserAttribute' with the minimum fields required to make a request.
--
-- * 'accessToken' - Represents the access token of the request to verify user attributes.
-- * 'attributeName' - The attribute name in the request to verify user attributes.
-- * 'code' - The verification code in the request to verify user attributes.
mkVerifyUserAttribute ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  -- | 'attributeName'
  Lude.Text ->
  -- | 'code'
  Lude.Text ->
  VerifyUserAttribute
mkVerifyUserAttribute pAccessToken_ pAttributeName_ pCode_ =
  VerifyUserAttribute'
    { accessToken = pAccessToken_,
      attributeName = pAttributeName_,
      code = pCode_
    }

-- | Represents the access token of the request to verify user attributes.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vuaAccessToken :: Lens.Lens' VerifyUserAttribute (Lude.Sensitive Lude.Text)
vuaAccessToken = Lens.lens (accessToken :: VerifyUserAttribute -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: VerifyUserAttribute)
{-# DEPRECATED vuaAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The attribute name in the request to verify user attributes.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vuaAttributeName :: Lens.Lens' VerifyUserAttribute Lude.Text
vuaAttributeName = Lens.lens (attributeName :: VerifyUserAttribute -> Lude.Text) (\s a -> s {attributeName = a} :: VerifyUserAttribute)
{-# DEPRECATED vuaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The verification code in the request to verify user attributes.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vuaCode :: Lens.Lens' VerifyUserAttribute Lude.Text
vuaCode = Lens.lens (code :: VerifyUserAttribute -> Lude.Text) (\s a -> s {code = a} :: VerifyUserAttribute)
{-# DEPRECATED vuaCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.AWSRequest VerifyUserAttribute where
  type Rs VerifyUserAttribute = VerifyUserAttributeResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          VerifyUserAttributeResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders VerifyUserAttribute where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.VerifyUserAttribute" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON VerifyUserAttribute where
  toJSON VerifyUserAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccessToken" Lude..= accessToken),
            Lude.Just ("AttributeName" Lude..= attributeName),
            Lude.Just ("Code" Lude..= code)
          ]
      )

instance Lude.ToPath VerifyUserAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery VerifyUserAttribute where
  toQuery = Lude.const Lude.mempty

-- | A container representing the response from the server from the request to verify user attributes.
--
-- /See:/ 'mkVerifyUserAttributeResponse' smart constructor.
newtype VerifyUserAttributeResponse = VerifyUserAttributeResponse'
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

-- | Creates a value of 'VerifyUserAttributeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkVerifyUserAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  VerifyUserAttributeResponse
mkVerifyUserAttributeResponse pResponseStatus_ =
  VerifyUserAttributeResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vuarsResponseStatus :: Lens.Lens' VerifyUserAttributeResponse Lude.Int
vuarsResponseStatus = Lens.lens (responseStatus :: VerifyUserAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: VerifyUserAttributeResponse)
{-# DEPRECATED vuarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
