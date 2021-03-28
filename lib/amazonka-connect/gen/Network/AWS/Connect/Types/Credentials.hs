{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Credentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.Credentials
  ( Credentials (..)
  -- * Smart constructor
  , mkCredentials
  -- * Lenses
  , cAccessToken
  , cAccessTokenExpiration
  , cRefreshToken
  , cRefreshTokenExpiration
  ) where

import qualified Network.AWS.Connect.Types.SecurityToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains credentials to use for federation.
--
-- /See:/ 'mkCredentials' smart constructor.
data Credentials = Credentials'
  { accessToken :: Core.Maybe Types.SecurityToken
    -- ^ An access token generated for a federated user to access Amazon Connect.
  , accessTokenExpiration :: Core.Maybe Core.NominalDiffTime
    -- ^ A token generated with an expiration time for the session a user is logged in to Amazon Connect.
  , refreshToken :: Core.Maybe Types.SecurityToken
    -- ^ Renews a token generated for a user to access the Amazon Connect instance.
  , refreshTokenExpiration :: Core.Maybe Core.NominalDiffTime
    -- ^ Renews the expiration timer for a generated token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Credentials' value with any optional fields omitted.
mkCredentials
    :: Credentials
mkCredentials
  = Credentials'{accessToken = Core.Nothing,
                 accessTokenExpiration = Core.Nothing, refreshToken = Core.Nothing,
                 refreshTokenExpiration = Core.Nothing}

-- | An access token generated for a federated user to access Amazon Connect.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAccessToken :: Lens.Lens' Credentials (Core.Maybe Types.SecurityToken)
cAccessToken = Lens.field @"accessToken"
{-# INLINEABLE cAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

-- | A token generated with an expiration time for the session a user is logged in to Amazon Connect.
--
-- /Note:/ Consider using 'accessTokenExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAccessTokenExpiration :: Lens.Lens' Credentials (Core.Maybe Core.NominalDiffTime)
cAccessTokenExpiration = Lens.field @"accessTokenExpiration"
{-# INLINEABLE cAccessTokenExpiration #-}
{-# DEPRECATED accessTokenExpiration "Use generic-lens or generic-optics with 'accessTokenExpiration' instead"  #-}

-- | Renews a token generated for a user to access the Amazon Connect instance.
--
-- /Note:/ Consider using 'refreshToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRefreshToken :: Lens.Lens' Credentials (Core.Maybe Types.SecurityToken)
cRefreshToken = Lens.field @"refreshToken"
{-# INLINEABLE cRefreshToken #-}
{-# DEPRECATED refreshToken "Use generic-lens or generic-optics with 'refreshToken' instead"  #-}

-- | Renews the expiration timer for a generated token.
--
-- /Note:/ Consider using 'refreshTokenExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRefreshTokenExpiration :: Lens.Lens' Credentials (Core.Maybe Core.NominalDiffTime)
cRefreshTokenExpiration = Lens.field @"refreshTokenExpiration"
{-# INLINEABLE cRefreshTokenExpiration #-}
{-# DEPRECATED refreshTokenExpiration "Use generic-lens or generic-optics with 'refreshTokenExpiration' instead"  #-}

instance Core.FromJSON Credentials where
        parseJSON
          = Core.withObject "Credentials" Core.$
              \ x ->
                Credentials' Core.<$>
                  (x Core..:? "AccessToken") Core.<*>
                    x Core..:? "AccessTokenExpiration"
                    Core.<*> x Core..:? "RefreshToken"
                    Core.<*> x Core..:? "RefreshTokenExpiration"
