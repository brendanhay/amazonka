{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.HttpDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.HttpDataSourceConfig
  ( HttpDataSourceConfig (..)
  -- * Smart constructor
  , mkHttpDataSourceConfig
  -- * Lenses
  , hdscAuthorizationConfig
  , hdscEndpoint
  ) where

import qualified Network.AWS.AppSync.Types.AuthorizationConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an HTTP data source configuration.
--
-- /See:/ 'mkHttpDataSourceConfig' smart constructor.
data HttpDataSourceConfig = HttpDataSourceConfig'
  { authorizationConfig :: Core.Maybe Types.AuthorizationConfig
    -- ^ The authorization config in case the HTTP endpoint requires authorization.
  , endpoint :: Core.Maybe Core.Text
    -- ^ The HTTP URL endpoint. You can either specify the domain name or IP, and port combination, and the URL scheme must be HTTP or HTTPS. If the port is not specified, AWS AppSync uses the default port 80 for the HTTP endpoint and port 443 for HTTPS endpoints.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpDataSourceConfig' value with any optional fields omitted.
mkHttpDataSourceConfig
    :: HttpDataSourceConfig
mkHttpDataSourceConfig
  = HttpDataSourceConfig'{authorizationConfig = Core.Nothing,
                          endpoint = Core.Nothing}

-- | The authorization config in case the HTTP endpoint requires authorization.
--
-- /Note:/ Consider using 'authorizationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hdscAuthorizationConfig :: Lens.Lens' HttpDataSourceConfig (Core.Maybe Types.AuthorizationConfig)
hdscAuthorizationConfig = Lens.field @"authorizationConfig"
{-# INLINEABLE hdscAuthorizationConfig #-}
{-# DEPRECATED authorizationConfig "Use generic-lens or generic-optics with 'authorizationConfig' instead"  #-}

-- | The HTTP URL endpoint. You can either specify the domain name or IP, and port combination, and the URL scheme must be HTTP or HTTPS. If the port is not specified, AWS AppSync uses the default port 80 for the HTTP endpoint and port 443 for HTTPS endpoints.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hdscEndpoint :: Lens.Lens' HttpDataSourceConfig (Core.Maybe Core.Text)
hdscEndpoint = Lens.field @"endpoint"
{-# INLINEABLE hdscEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

instance Core.FromJSON HttpDataSourceConfig where
        toJSON HttpDataSourceConfig{..}
          = Core.object
              (Core.catMaybes
                 [("authorizationConfig" Core..=) Core.<$> authorizationConfig,
                  ("endpoint" Core..=) Core.<$> endpoint])

instance Core.FromJSON HttpDataSourceConfig where
        parseJSON
          = Core.withObject "HttpDataSourceConfig" Core.$
              \ x ->
                HttpDataSourceConfig' Core.<$>
                  (x Core..:? "authorizationConfig") Core.<*> x Core..:? "endpoint"
