{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.OpenIDConnectConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.OpenIDConnectConfig
  ( OpenIDConnectConfig (..)
  -- * Smart constructor
  , mkOpenIDConnectConfig
  -- * Lenses
  , oidccIssuer
  , oidccAuthTTL
  , oidccClientId
  , oidccIatTTL
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an OpenID Connect configuration.
--
-- /See:/ 'mkOpenIDConnectConfig' smart constructor.
data OpenIDConnectConfig = OpenIDConnectConfig'
  { issuer :: Core.Text
    -- ^ The issuer for the OpenID Connect configuration. The issuer returned by discovery must exactly match the value of @iss@ in the ID token.
  , authTTL :: Core.Maybe Core.Integer
    -- ^ The number of milliseconds a token is valid after being authenticated.
  , clientId :: Core.Maybe Core.Text
    -- ^ The client identifier of the Relying party at the OpenID identity provider. This identifier is typically obtained when the Relying party is registered with the OpenID identity provider. You can specify a regular expression so the AWS AppSync can validate against multiple client identifiers at a time.
  , iatTTL :: Core.Maybe Core.Integer
    -- ^ The number of milliseconds a token is valid after being issued to a user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OpenIDConnectConfig' value with any optional fields omitted.
mkOpenIDConnectConfig
    :: Core.Text -- ^ 'issuer'
    -> OpenIDConnectConfig
mkOpenIDConnectConfig issuer
  = OpenIDConnectConfig'{issuer, authTTL = Core.Nothing,
                         clientId = Core.Nothing, iatTTL = Core.Nothing}

-- | The issuer for the OpenID Connect configuration. The issuer returned by discovery must exactly match the value of @iss@ in the ID token.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oidccIssuer :: Lens.Lens' OpenIDConnectConfig Core.Text
oidccIssuer = Lens.field @"issuer"
{-# INLINEABLE oidccIssuer #-}
{-# DEPRECATED issuer "Use generic-lens or generic-optics with 'issuer' instead"  #-}

-- | The number of milliseconds a token is valid after being authenticated.
--
-- /Note:/ Consider using 'authTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oidccAuthTTL :: Lens.Lens' OpenIDConnectConfig (Core.Maybe Core.Integer)
oidccAuthTTL = Lens.field @"authTTL"
{-# INLINEABLE oidccAuthTTL #-}
{-# DEPRECATED authTTL "Use generic-lens or generic-optics with 'authTTL' instead"  #-}

-- | The client identifier of the Relying party at the OpenID identity provider. This identifier is typically obtained when the Relying party is registered with the OpenID identity provider. You can specify a regular expression so the AWS AppSync can validate against multiple client identifiers at a time.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oidccClientId :: Lens.Lens' OpenIDConnectConfig (Core.Maybe Core.Text)
oidccClientId = Lens.field @"clientId"
{-# INLINEABLE oidccClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The number of milliseconds a token is valid after being issued to a user.
--
-- /Note:/ Consider using 'iatTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oidccIatTTL :: Lens.Lens' OpenIDConnectConfig (Core.Maybe Core.Integer)
oidccIatTTL = Lens.field @"iatTTL"
{-# INLINEABLE oidccIatTTL #-}
{-# DEPRECATED iatTTL "Use generic-lens or generic-optics with 'iatTTL' instead"  #-}

instance Core.FromJSON OpenIDConnectConfig where
        toJSON OpenIDConnectConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("issuer" Core..= issuer),
                  ("authTTL" Core..=) Core.<$> authTTL,
                  ("clientId" Core..=) Core.<$> clientId,
                  ("iatTTL" Core..=) Core.<$> iatTTL])

instance Core.FromJSON OpenIDConnectConfig where
        parseJSON
          = Core.withObject "OpenIDConnectConfig" Core.$
              \ x ->
                OpenIDConnectConfig' Core.<$>
                  (x Core..: "issuer") Core.<*> x Core..:? "authTTL" Core.<*>
                    x Core..:? "clientId"
                    Core.<*> x Core..:? "iatTTL"
