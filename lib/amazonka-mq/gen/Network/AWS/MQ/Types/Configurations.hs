{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.Configurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.Configurations
  ( Configurations (..)
  -- * Smart constructor
  , mkConfigurations
  -- * Lenses
  , cCurrent
  , cHistory
  , cPending
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types.ConfigurationId as Types
import qualified Network.AWS.Prelude as Core

-- | Broker configuration information
--
-- /See:/ 'mkConfigurations' smart constructor.
data Configurations = Configurations'
  { current :: Core.Maybe Types.ConfigurationId
    -- ^ The current configuration of the broker.
  , history :: Core.Maybe [Types.ConfigurationId]
    -- ^ The history of configurations applied to the broker.
  , pending :: Core.Maybe Types.ConfigurationId
    -- ^ The pending configuration of the broker.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Configurations' value with any optional fields omitted.
mkConfigurations
    :: Configurations
mkConfigurations
  = Configurations'{current = Core.Nothing, history = Core.Nothing,
                    pending = Core.Nothing}

-- | The current configuration of the broker.
--
-- /Note:/ Consider using 'current' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCurrent :: Lens.Lens' Configurations (Core.Maybe Types.ConfigurationId)
cCurrent = Lens.field @"current"
{-# INLINEABLE cCurrent #-}
{-# DEPRECATED current "Use generic-lens or generic-optics with 'current' instead"  #-}

-- | The history of configurations applied to the broker.
--
-- /Note:/ Consider using 'history' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHistory :: Lens.Lens' Configurations (Core.Maybe [Types.ConfigurationId])
cHistory = Lens.field @"history"
{-# INLINEABLE cHistory #-}
{-# DEPRECATED history "Use generic-lens or generic-optics with 'history' instead"  #-}

-- | The pending configuration of the broker.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPending :: Lens.Lens' Configurations (Core.Maybe Types.ConfigurationId)
cPending = Lens.field @"pending"
{-# INLINEABLE cPending #-}
{-# DEPRECATED pending "Use generic-lens or generic-optics with 'pending' instead"  #-}

instance Core.FromJSON Configurations where
        parseJSON
          = Core.withObject "Configurations" Core.$
              \ x ->
                Configurations' Core.<$>
                  (x Core..:? "current") Core.<*> x Core..:? "history" Core.<*>
                    x Core..:? "pending"
