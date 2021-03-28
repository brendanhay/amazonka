{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
  ( AdditionalAuthenticationProvider (..)
  -- * Smart constructor
  , mkAdditionalAuthenticationProvider
  -- * Lenses
  , aapAuthenticationType
  , aapOpenIDConnectConfig
  , aapUserPoolConfig
  ) where

import qualified Network.AWS.AppSync.Types.AuthenticationType as Types
import qualified Network.AWS.AppSync.Types.CognitoUserPoolConfig as Types
import qualified Network.AWS.AppSync.Types.OpenIDConnectConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an additional authentication provider.
--
-- /See:/ 'mkAdditionalAuthenticationProvider' smart constructor.
data AdditionalAuthenticationProvider = AdditionalAuthenticationProvider'
  { authenticationType :: Core.Maybe Types.AuthenticationType
    -- ^ The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
  , openIDConnectConfig :: Core.Maybe Types.OpenIDConnectConfig
    -- ^ The OpenID Connect configuration.
  , userPoolConfig :: Core.Maybe Types.CognitoUserPoolConfig
    -- ^ The Amazon Cognito user pool configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdditionalAuthenticationProvider' value with any optional fields omitted.
mkAdditionalAuthenticationProvider
    :: AdditionalAuthenticationProvider
mkAdditionalAuthenticationProvider
  = AdditionalAuthenticationProvider'{authenticationType =
                                        Core.Nothing,
                                      openIDConnectConfig = Core.Nothing,
                                      userPoolConfig = Core.Nothing}

-- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aapAuthenticationType :: Lens.Lens' AdditionalAuthenticationProvider (Core.Maybe Types.AuthenticationType)
aapAuthenticationType = Lens.field @"authenticationType"
{-# INLINEABLE aapAuthenticationType #-}
{-# DEPRECATED authenticationType "Use generic-lens or generic-optics with 'authenticationType' instead"  #-}

-- | The OpenID Connect configuration.
--
-- /Note:/ Consider using 'openIDConnectConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aapOpenIDConnectConfig :: Lens.Lens' AdditionalAuthenticationProvider (Core.Maybe Types.OpenIDConnectConfig)
aapOpenIDConnectConfig = Lens.field @"openIDConnectConfig"
{-# INLINEABLE aapOpenIDConnectConfig #-}
{-# DEPRECATED openIDConnectConfig "Use generic-lens or generic-optics with 'openIDConnectConfig' instead"  #-}

-- | The Amazon Cognito user pool configuration.
--
-- /Note:/ Consider using 'userPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aapUserPoolConfig :: Lens.Lens' AdditionalAuthenticationProvider (Core.Maybe Types.CognitoUserPoolConfig)
aapUserPoolConfig = Lens.field @"userPoolConfig"
{-# INLINEABLE aapUserPoolConfig #-}
{-# DEPRECATED userPoolConfig "Use generic-lens or generic-optics with 'userPoolConfig' instead"  #-}

instance Core.FromJSON AdditionalAuthenticationProvider where
        toJSON AdditionalAuthenticationProvider{..}
          = Core.object
              (Core.catMaybes
                 [("authenticationType" Core..=) Core.<$> authenticationType,
                  ("openIDConnectConfig" Core..=) Core.<$> openIDConnectConfig,
                  ("userPoolConfig" Core..=) Core.<$> userPoolConfig])

instance Core.FromJSON AdditionalAuthenticationProvider where
        parseJSON
          = Core.withObject "AdditionalAuthenticationProvider" Core.$
              \ x ->
                AdditionalAuthenticationProvider' Core.<$>
                  (x Core..:? "authenticationType") Core.<*>
                    x Core..:? "openIDConnectConfig"
                    Core.<*> x Core..:? "userPoolConfig"
