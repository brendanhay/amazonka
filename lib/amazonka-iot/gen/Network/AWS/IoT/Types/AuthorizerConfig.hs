{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuthorizerConfig
  ( AuthorizerConfig (..)
  -- * Smart constructor
  , mkAuthorizerConfig
  -- * Lenses
  , acAllowAuthorizerOverride
  , acDefaultAuthorizerName
  ) where

import qualified Network.AWS.IoT.Types.DefaultAuthorizerName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that specifies the authorization service for a domain.
--
-- /See:/ 'mkAuthorizerConfig' smart constructor.
data AuthorizerConfig = AuthorizerConfig'
  { allowAuthorizerOverride :: Core.Maybe Core.Bool
    -- ^ A Boolean that specifies whether the domain configuration's authorization service can be overridden.
  , defaultAuthorizerName :: Core.Maybe Types.DefaultAuthorizerName
    -- ^ The name of the authorization service for a domain configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizerConfig' value with any optional fields omitted.
mkAuthorizerConfig
    :: AuthorizerConfig
mkAuthorizerConfig
  = AuthorizerConfig'{allowAuthorizerOverride = Core.Nothing,
                      defaultAuthorizerName = Core.Nothing}

-- | A Boolean that specifies whether the domain configuration's authorization service can be overridden.
--
-- /Note:/ Consider using 'allowAuthorizerOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAllowAuthorizerOverride :: Lens.Lens' AuthorizerConfig (Core.Maybe Core.Bool)
acAllowAuthorizerOverride = Lens.field @"allowAuthorizerOverride"
{-# INLINEABLE acAllowAuthorizerOverride #-}
{-# DEPRECATED allowAuthorizerOverride "Use generic-lens or generic-optics with 'allowAuthorizerOverride' instead"  #-}

-- | The name of the authorization service for a domain configuration.
--
-- /Note:/ Consider using 'defaultAuthorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acDefaultAuthorizerName :: Lens.Lens' AuthorizerConfig (Core.Maybe Types.DefaultAuthorizerName)
acDefaultAuthorizerName = Lens.field @"defaultAuthorizerName"
{-# INLINEABLE acDefaultAuthorizerName #-}
{-# DEPRECATED defaultAuthorizerName "Use generic-lens or generic-optics with 'defaultAuthorizerName' instead"  #-}

instance Core.FromJSON AuthorizerConfig where
        toJSON AuthorizerConfig{..}
          = Core.object
              (Core.catMaybes
                 [("allowAuthorizerOverride" Core..=) Core.<$>
                    allowAuthorizerOverride,
                  ("defaultAuthorizerName" Core..=) Core.<$> defaultAuthorizerName])

instance Core.FromJSON AuthorizerConfig where
        parseJSON
          = Core.withObject "AuthorizerConfig" Core.$
              \ x ->
                AuthorizerConfig' Core.<$>
                  (x Core..:? "allowAuthorizerOverride") Core.<*>
                    x Core..:? "defaultAuthorizerName"
