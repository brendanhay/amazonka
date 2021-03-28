{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionConfiguration
  ( ActionConfiguration (..)
  -- * Smart constructor
  , mkActionConfiguration
  -- * Lenses
  , acConfiguration
  ) where

import qualified Network.AWS.CodePipeline.Types.ActionConfigurationKey as Types
import qualified Network.AWS.CodePipeline.Types.ActionConfigurationValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about an action configuration.
--
-- /See:/ 'mkActionConfiguration' smart constructor.
newtype ActionConfiguration = ActionConfiguration'
  { configuration :: Core.Maybe (Core.HashMap Types.ActionConfigurationKey Types.ActionConfigurationValue)
    -- ^ The configuration data for the action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ActionConfiguration' value with any optional fields omitted.
mkActionConfiguration
    :: ActionConfiguration
mkActionConfiguration
  = ActionConfiguration'{configuration = Core.Nothing}

-- | The configuration data for the action.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acConfiguration :: Lens.Lens' ActionConfiguration (Core.Maybe (Core.HashMap Types.ActionConfigurationKey Types.ActionConfigurationValue))
acConfiguration = Lens.field @"configuration"
{-# INLINEABLE acConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

instance Core.FromJSON ActionConfiguration where
        parseJSON
          = Core.withObject "ActionConfiguration" Core.$
              \ x -> ActionConfiguration' Core.<$> (x Core..:? "configuration")
