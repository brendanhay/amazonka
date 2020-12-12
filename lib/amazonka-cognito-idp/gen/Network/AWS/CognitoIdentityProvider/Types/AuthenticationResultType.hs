{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AuthenticationResultType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AuthenticationResultType
  ( AuthenticationResultType (..),

    -- * Smart constructor
    mkAuthenticationResultType,

    -- * Lenses
    artAccessToken,
    artRefreshToken,
    artNewDeviceMetadata,
    artExpiresIn,
    artTokenType,
    artIdToken,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The authentication result.
--
-- /See:/ 'mkAuthenticationResultType' smart constructor.
data AuthenticationResultType = AuthenticationResultType'
  { accessToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    refreshToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    newDeviceMetadata ::
      Lude.Maybe NewDeviceMetadataType,
    expiresIn :: Lude.Maybe Lude.Int,
    tokenType :: Lude.Maybe Lude.Text,
    idToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthenticationResultType' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token.
-- * 'expiresIn' - The expiration period of the authentication result in seconds.
-- * 'idToken' - The ID token.
-- * 'newDeviceMetadata' - The new device metadata from an authentication result.
-- * 'refreshToken' - The refresh token.
-- * 'tokenType' - The token type.
mkAuthenticationResultType ::
  AuthenticationResultType
mkAuthenticationResultType =
  AuthenticationResultType'
    { accessToken = Lude.Nothing,
      refreshToken = Lude.Nothing,
      newDeviceMetadata = Lude.Nothing,
      expiresIn = Lude.Nothing,
      tokenType = Lude.Nothing,
      idToken = Lude.Nothing
    }

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artAccessToken :: Lens.Lens' AuthenticationResultType (Lude.Maybe (Lude.Sensitive Lude.Text))
artAccessToken = Lens.lens (accessToken :: AuthenticationResultType -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {accessToken = a} :: AuthenticationResultType)
{-# DEPRECATED artAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The refresh token.
--
-- /Note:/ Consider using 'refreshToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artRefreshToken :: Lens.Lens' AuthenticationResultType (Lude.Maybe (Lude.Sensitive Lude.Text))
artRefreshToken = Lens.lens (refreshToken :: AuthenticationResultType -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {refreshToken = a} :: AuthenticationResultType)
{-# DEPRECATED artRefreshToken "Use generic-lens or generic-optics with 'refreshToken' instead." #-}

-- | The new device metadata from an authentication result.
--
-- /Note:/ Consider using 'newDeviceMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artNewDeviceMetadata :: Lens.Lens' AuthenticationResultType (Lude.Maybe NewDeviceMetadataType)
artNewDeviceMetadata = Lens.lens (newDeviceMetadata :: AuthenticationResultType -> Lude.Maybe NewDeviceMetadataType) (\s a -> s {newDeviceMetadata = a} :: AuthenticationResultType)
{-# DEPRECATED artNewDeviceMetadata "Use generic-lens or generic-optics with 'newDeviceMetadata' instead." #-}

-- | The expiration period of the authentication result in seconds.
--
-- /Note:/ Consider using 'expiresIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artExpiresIn :: Lens.Lens' AuthenticationResultType (Lude.Maybe Lude.Int)
artExpiresIn = Lens.lens (expiresIn :: AuthenticationResultType -> Lude.Maybe Lude.Int) (\s a -> s {expiresIn = a} :: AuthenticationResultType)
{-# DEPRECATED artExpiresIn "Use generic-lens or generic-optics with 'expiresIn' instead." #-}

-- | The token type.
--
-- /Note:/ Consider using 'tokenType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artTokenType :: Lens.Lens' AuthenticationResultType (Lude.Maybe Lude.Text)
artTokenType = Lens.lens (tokenType :: AuthenticationResultType -> Lude.Maybe Lude.Text) (\s a -> s {tokenType = a} :: AuthenticationResultType)
{-# DEPRECATED artTokenType "Use generic-lens or generic-optics with 'tokenType' instead." #-}

-- | The ID token.
--
-- /Note:/ Consider using 'idToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artIdToken :: Lens.Lens' AuthenticationResultType (Lude.Maybe (Lude.Sensitive Lude.Text))
artIdToken = Lens.lens (idToken :: AuthenticationResultType -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {idToken = a} :: AuthenticationResultType)
{-# DEPRECATED artIdToken "Use generic-lens or generic-optics with 'idToken' instead." #-}

instance Lude.FromJSON AuthenticationResultType where
  parseJSON =
    Lude.withObject
      "AuthenticationResultType"
      ( \x ->
          AuthenticationResultType'
            Lude.<$> (x Lude..:? "AccessToken")
            Lude.<*> (x Lude..:? "RefreshToken")
            Lude.<*> (x Lude..:? "NewDeviceMetadata")
            Lude.<*> (x Lude..:? "ExpiresIn")
            Lude.<*> (x Lude..:? "TokenType")
            Lude.<*> (x Lude..:? "IdToken")
      )
