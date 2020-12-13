{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Credentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Credentials
  ( Credentials (..),

    -- * Smart constructor
    mkCredentials,

    -- * Lenses
    cAccessTokenExpiration,
    cAccessToken,
    cRefreshToken,
    cRefreshTokenExpiration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains credentials to use for federation.
--
-- /See:/ 'mkCredentials' smart constructor.
data Credentials = Credentials'
  { -- | A token generated with an expiration time for the session a user is logged in to Amazon Connect.
    accessTokenExpiration :: Lude.Maybe Lude.Timestamp,
    -- | An access token generated for a federated user to access Amazon Connect.
    accessToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Renews a token generated for a user to access the Amazon Connect instance.
    refreshToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Renews the expiration timer for a generated token.
    refreshTokenExpiration :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Credentials' with the minimum fields required to make a request.
--
-- * 'accessTokenExpiration' - A token generated with an expiration time for the session a user is logged in to Amazon Connect.
-- * 'accessToken' - An access token generated for a federated user to access Amazon Connect.
-- * 'refreshToken' - Renews a token generated for a user to access the Amazon Connect instance.
-- * 'refreshTokenExpiration' - Renews the expiration timer for a generated token.
mkCredentials ::
  Credentials
mkCredentials =
  Credentials'
    { accessTokenExpiration = Lude.Nothing,
      accessToken = Lude.Nothing,
      refreshToken = Lude.Nothing,
      refreshTokenExpiration = Lude.Nothing
    }

-- | A token generated with an expiration time for the session a user is logged in to Amazon Connect.
--
-- /Note:/ Consider using 'accessTokenExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAccessTokenExpiration :: Lens.Lens' Credentials (Lude.Maybe Lude.Timestamp)
cAccessTokenExpiration = Lens.lens (accessTokenExpiration :: Credentials -> Lude.Maybe Lude.Timestamp) (\s a -> s {accessTokenExpiration = a} :: Credentials)
{-# DEPRECATED cAccessTokenExpiration "Use generic-lens or generic-optics with 'accessTokenExpiration' instead." #-}

-- | An access token generated for a federated user to access Amazon Connect.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAccessToken :: Lens.Lens' Credentials (Lude.Maybe (Lude.Sensitive Lude.Text))
cAccessToken = Lens.lens (accessToken :: Credentials -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {accessToken = a} :: Credentials)
{-# DEPRECATED cAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | Renews a token generated for a user to access the Amazon Connect instance.
--
-- /Note:/ Consider using 'refreshToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRefreshToken :: Lens.Lens' Credentials (Lude.Maybe (Lude.Sensitive Lude.Text))
cRefreshToken = Lens.lens (refreshToken :: Credentials -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {refreshToken = a} :: Credentials)
{-# DEPRECATED cRefreshToken "Use generic-lens or generic-optics with 'refreshToken' instead." #-}

-- | Renews the expiration timer for a generated token.
--
-- /Note:/ Consider using 'refreshTokenExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRefreshTokenExpiration :: Lens.Lens' Credentials (Lude.Maybe Lude.Timestamp)
cRefreshTokenExpiration = Lens.lens (refreshTokenExpiration :: Credentials -> Lude.Maybe Lude.Timestamp) (\s a -> s {refreshTokenExpiration = a} :: Credentials)
{-# DEPRECATED cRefreshTokenExpiration "Use generic-lens or generic-optics with 'refreshTokenExpiration' instead." #-}

instance Lude.FromJSON Credentials where
  parseJSON =
    Lude.withObject
      "Credentials"
      ( \x ->
          Credentials'
            Lude.<$> (x Lude..:? "AccessTokenExpiration")
            Lude.<*> (x Lude..:? "AccessToken")
            Lude.<*> (x Lude..:? "RefreshToken")
            Lude.<*> (x Lude..:? "RefreshTokenExpiration")
      )
