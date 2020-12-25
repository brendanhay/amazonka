{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    tvutIdToken,
    tvutRefreshToken,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The data type for TokenValidityUnits that specifics the time measurements for token validity.
--
-- /See:/ 'mkTokenValidityUnitsType' smart constructor.
data TokenValidityUnitsType = TokenValidityUnitsType'
  { -- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in AccessTokenValidity, defaults to hours.
    accessToken :: Core.Maybe Types.TimeUnitsType,
    -- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in IdTokenValidity, defaults to hours.
    idToken :: Core.Maybe Types.TimeUnitsType,
    -- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in RefreshTokenValidity, defaults to days.
    refreshToken :: Core.Maybe Types.TimeUnitsType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TokenValidityUnitsType' value with any optional fields omitted.
mkTokenValidityUnitsType ::
  TokenValidityUnitsType
mkTokenValidityUnitsType =
  TokenValidityUnitsType'
    { accessToken = Core.Nothing,
      idToken = Core.Nothing,
      refreshToken = Core.Nothing
    }

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in AccessTokenValidity, defaults to hours.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvutAccessToken :: Lens.Lens' TokenValidityUnitsType (Core.Maybe Types.TimeUnitsType)
tvutAccessToken = Lens.field @"accessToken"
{-# DEPRECATED tvutAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in IdTokenValidity, defaults to hours.
--
-- /Note:/ Consider using 'idToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvutIdToken :: Lens.Lens' TokenValidityUnitsType (Core.Maybe Types.TimeUnitsType)
tvutIdToken = Lens.field @"idToken"
{-# DEPRECATED tvutIdToken "Use generic-lens or generic-optics with 'idToken' instead." #-}

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in RefreshTokenValidity, defaults to days.
--
-- /Note:/ Consider using 'refreshToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvutRefreshToken :: Lens.Lens' TokenValidityUnitsType (Core.Maybe Types.TimeUnitsType)
tvutRefreshToken = Lens.field @"refreshToken"
{-# DEPRECATED tvutRefreshToken "Use generic-lens or generic-optics with 'refreshToken' instead." #-}

instance Core.FromJSON TokenValidityUnitsType where
  toJSON TokenValidityUnitsType {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessToken" Core..=) Core.<$> accessToken,
            ("IdToken" Core..=) Core.<$> idToken,
            ("RefreshToken" Core..=) Core.<$> refreshToken
          ]
      )

instance Core.FromJSON TokenValidityUnitsType where
  parseJSON =
    Core.withObject "TokenValidityUnitsType" Core.$
      \x ->
        TokenValidityUnitsType'
          Core.<$> (x Core..:? "AccessToken")
          Core.<*> (x Core..:? "IdToken")
          Core.<*> (x Core..:? "RefreshToken")
