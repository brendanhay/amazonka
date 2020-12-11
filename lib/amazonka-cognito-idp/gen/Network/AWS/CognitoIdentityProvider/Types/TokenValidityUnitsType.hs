-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType
  ( TokenValidityUnitsType (..),

    -- * Smart constructor
    mkTokenValidityUnitsType,

    -- * Lenses
    tvutAccessToken,
    tvutRefreshToken,
    tvutIdToken,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The data type for TokenValidityUnits that specifics the time measurements for token validity.
--
-- /See:/ 'mkTokenValidityUnitsType' smart constructor.
data TokenValidityUnitsType = TokenValidityUnitsType'
  { accessToken ::
      Lude.Maybe TimeUnitsType,
    refreshToken :: Lude.Maybe TimeUnitsType,
    idToken :: Lude.Maybe TimeUnitsType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TokenValidityUnitsType' with the minimum fields required to make a request.
--
-- * 'accessToken' - A time unit in “seconds”, “minutes”, “hours” or “days” for the value in AccessTokenValidity, defaults to hours.
-- * 'idToken' - A time unit in “seconds”, “minutes”, “hours” or “days” for the value in IdTokenValidity, defaults to hours.
-- * 'refreshToken' - A time unit in “seconds”, “minutes”, “hours” or “days” for the value in RefreshTokenValidity, defaults to days.
mkTokenValidityUnitsType ::
  TokenValidityUnitsType
mkTokenValidityUnitsType =
  TokenValidityUnitsType'
    { accessToken = Lude.Nothing,
      refreshToken = Lude.Nothing,
      idToken = Lude.Nothing
    }

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in AccessTokenValidity, defaults to hours.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvutAccessToken :: Lens.Lens' TokenValidityUnitsType (Lude.Maybe TimeUnitsType)
tvutAccessToken = Lens.lens (accessToken :: TokenValidityUnitsType -> Lude.Maybe TimeUnitsType) (\s a -> s {accessToken = a} :: TokenValidityUnitsType)
{-# DEPRECATED tvutAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in RefreshTokenValidity, defaults to days.
--
-- /Note:/ Consider using 'refreshToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvutRefreshToken :: Lens.Lens' TokenValidityUnitsType (Lude.Maybe TimeUnitsType)
tvutRefreshToken = Lens.lens (refreshToken :: TokenValidityUnitsType -> Lude.Maybe TimeUnitsType) (\s a -> s {refreshToken = a} :: TokenValidityUnitsType)
{-# DEPRECATED tvutRefreshToken "Use generic-lens or generic-optics with 'refreshToken' instead." #-}

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in IdTokenValidity, defaults to hours.
--
-- /Note:/ Consider using 'idToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvutIdToken :: Lens.Lens' TokenValidityUnitsType (Lude.Maybe TimeUnitsType)
tvutIdToken = Lens.lens (idToken :: TokenValidityUnitsType -> Lude.Maybe TimeUnitsType) (\s a -> s {idToken = a} :: TokenValidityUnitsType)
{-# DEPRECATED tvutIdToken "Use generic-lens or generic-optics with 'idToken' instead." #-}

instance Lude.FromJSON TokenValidityUnitsType where
  parseJSON =
    Lude.withObject
      "TokenValidityUnitsType"
      ( \x ->
          TokenValidityUnitsType'
            Lude.<$> (x Lude..:? "AccessToken")
            Lude.<*> (x Lude..:? "RefreshToken")
            Lude.<*> (x Lude..:? "IdToken")
      )

instance Lude.ToJSON TokenValidityUnitsType where
  toJSON TokenValidityUnitsType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AccessToken" Lude..=) Lude.<$> accessToken,
            ("RefreshToken" Lude..=) Lude.<$> refreshToken,
            ("IdToken" Lude..=) Lude.<$> idToken
          ]
      )
