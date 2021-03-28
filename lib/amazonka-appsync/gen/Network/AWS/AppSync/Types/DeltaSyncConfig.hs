{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DeltaSyncConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.DeltaSyncConfig
  ( DeltaSyncConfig (..)
  -- * Smart constructor
  , mkDeltaSyncConfig
  -- * Lenses
  , dscBaseTableTTL
  , dscDeltaSyncTableName
  , dscDeltaSyncTableTTL
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Delta Sync configuration.
--
-- /See:/ 'mkDeltaSyncConfig' smart constructor.
data DeltaSyncConfig = DeltaSyncConfig'
  { baseTableTTL :: Core.Maybe Core.Integer
    -- ^ The number of minutes an Item is stored in the datasource.
  , deltaSyncTableName :: Core.Maybe Core.Text
    -- ^ The Delta Sync table name.
  , deltaSyncTableTTL :: Core.Maybe Core.Integer
    -- ^ The number of minutes a Delta Sync log entry is stored in the Delta Sync table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeltaSyncConfig' value with any optional fields omitted.
mkDeltaSyncConfig
    :: DeltaSyncConfig
mkDeltaSyncConfig
  = DeltaSyncConfig'{baseTableTTL = Core.Nothing,
                     deltaSyncTableName = Core.Nothing,
                     deltaSyncTableTTL = Core.Nothing}

-- | The number of minutes an Item is stored in the datasource.
--
-- /Note:/ Consider using 'baseTableTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscBaseTableTTL :: Lens.Lens' DeltaSyncConfig (Core.Maybe Core.Integer)
dscBaseTableTTL = Lens.field @"baseTableTTL"
{-# INLINEABLE dscBaseTableTTL #-}
{-# DEPRECATED baseTableTTL "Use generic-lens or generic-optics with 'baseTableTTL' instead"  #-}

-- | The Delta Sync table name.
--
-- /Note:/ Consider using 'deltaSyncTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscDeltaSyncTableName :: Lens.Lens' DeltaSyncConfig (Core.Maybe Core.Text)
dscDeltaSyncTableName = Lens.field @"deltaSyncTableName"
{-# INLINEABLE dscDeltaSyncTableName #-}
{-# DEPRECATED deltaSyncTableName "Use generic-lens or generic-optics with 'deltaSyncTableName' instead"  #-}

-- | The number of minutes a Delta Sync log entry is stored in the Delta Sync table.
--
-- /Note:/ Consider using 'deltaSyncTableTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscDeltaSyncTableTTL :: Lens.Lens' DeltaSyncConfig (Core.Maybe Core.Integer)
dscDeltaSyncTableTTL = Lens.field @"deltaSyncTableTTL"
{-# INLINEABLE dscDeltaSyncTableTTL #-}
{-# DEPRECATED deltaSyncTableTTL "Use generic-lens or generic-optics with 'deltaSyncTableTTL' instead"  #-}

instance Core.FromJSON DeltaSyncConfig where
        toJSON DeltaSyncConfig{..}
          = Core.object
              (Core.catMaybes
                 [("baseTableTTL" Core..=) Core.<$> baseTableTTL,
                  ("deltaSyncTableName" Core..=) Core.<$> deltaSyncTableName,
                  ("deltaSyncTableTTL" Core..=) Core.<$> deltaSyncTableTTL])

instance Core.FromJSON DeltaSyncConfig where
        parseJSON
          = Core.withObject "DeltaSyncConfig" Core.$
              \ x ->
                DeltaSyncConfig' Core.<$>
                  (x Core..:? "baseTableTTL") Core.<*>
                    x Core..:? "deltaSyncTableName"
                    Core.<*> x Core..:? "deltaSyncTableTTL"
