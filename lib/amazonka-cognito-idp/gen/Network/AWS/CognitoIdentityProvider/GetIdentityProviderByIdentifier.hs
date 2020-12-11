{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified identity provider.
module Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
  ( -- * Creating a request
    GetIdentityProviderByIdentifier (..),
    mkGetIdentityProviderByIdentifier,

    -- ** Request lenses
    gipbiUserPoolId,
    gipbiIdpIdentifier,

    -- * Destructuring the response
    GetIdentityProviderByIdentifierResponse (..),
    mkGetIdentityProviderByIdentifierResponse,

    -- ** Response lenses
    gipbirsResponseStatus,
    gipbirsIdentityProvider,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetIdentityProviderByIdentifier' smart constructor.
data GetIdentityProviderByIdentifier = GetIdentityProviderByIdentifier'
  { userPoolId ::
      Lude.Text,
    idpIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityProviderByIdentifier' with the minimum fields required to make a request.
--
-- * 'idpIdentifier' - The identity provider ID.
-- * 'userPoolId' - The user pool ID.
mkGetIdentityProviderByIdentifier ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'idpIdentifier'
  Lude.Text ->
  GetIdentityProviderByIdentifier
mkGetIdentityProviderByIdentifier pUserPoolId_ pIdpIdentifier_ =
  GetIdentityProviderByIdentifier'
    { userPoolId = pUserPoolId_,
      idpIdentifier = pIdpIdentifier_
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipbiUserPoolId :: Lens.Lens' GetIdentityProviderByIdentifier Lude.Text
gipbiUserPoolId = Lens.lens (userPoolId :: GetIdentityProviderByIdentifier -> Lude.Text) (\s a -> s {userPoolId = a} :: GetIdentityProviderByIdentifier)
{-# DEPRECATED gipbiUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identity provider ID.
--
-- /Note:/ Consider using 'idpIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipbiIdpIdentifier :: Lens.Lens' GetIdentityProviderByIdentifier Lude.Text
gipbiIdpIdentifier = Lens.lens (idpIdentifier :: GetIdentityProviderByIdentifier -> Lude.Text) (\s a -> s {idpIdentifier = a} :: GetIdentityProviderByIdentifier)
{-# DEPRECATED gipbiIdpIdentifier "Use generic-lens or generic-optics with 'idpIdentifier' instead." #-}

instance Lude.AWSRequest GetIdentityProviderByIdentifier where
  type
    Rs GetIdentityProviderByIdentifier =
      GetIdentityProviderByIdentifierResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetIdentityProviderByIdentifierResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "IdentityProvider")
      )

instance Lude.ToHeaders GetIdentityProviderByIdentifier where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.GetIdentityProviderByIdentifier" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetIdentityProviderByIdentifier where
  toJSON GetIdentityProviderByIdentifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("IdpIdentifier" Lude..= idpIdentifier)
          ]
      )

instance Lude.ToPath GetIdentityProviderByIdentifier where
  toPath = Lude.const "/"

instance Lude.ToQuery GetIdentityProviderByIdentifier where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetIdentityProviderByIdentifierResponse' smart constructor.
data GetIdentityProviderByIdentifierResponse = GetIdentityProviderByIdentifierResponse'
  { responseStatus ::
      Lude.Int,
    identityProvider ::
      IdentityProviderType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityProviderByIdentifierResponse' with the minimum fields required to make a request.
--
-- * 'identityProvider' - The identity provider object.
-- * 'responseStatus' - The response status code.
mkGetIdentityProviderByIdentifierResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'identityProvider'
  IdentityProviderType ->
  GetIdentityProviderByIdentifierResponse
mkGetIdentityProviderByIdentifierResponse
  pResponseStatus_
  pIdentityProvider_ =
    GetIdentityProviderByIdentifierResponse'
      { responseStatus =
          pResponseStatus_,
        identityProvider = pIdentityProvider_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipbirsResponseStatus :: Lens.Lens' GetIdentityProviderByIdentifierResponse Lude.Int
gipbirsResponseStatus = Lens.lens (responseStatus :: GetIdentityProviderByIdentifierResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIdentityProviderByIdentifierResponse)
{-# DEPRECATED gipbirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The identity provider object.
--
-- /Note:/ Consider using 'identityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipbirsIdentityProvider :: Lens.Lens' GetIdentityProviderByIdentifierResponse IdentityProviderType
gipbirsIdentityProvider = Lens.lens (identityProvider :: GetIdentityProviderByIdentifierResponse -> IdentityProviderType) (\s a -> s {identityProvider = a} :: GetIdentityProviderByIdentifierResponse)
{-# DEPRECATED gipbirsIdentityProvider "Use generic-lens or generic-optics with 'identityProvider' instead." #-}
