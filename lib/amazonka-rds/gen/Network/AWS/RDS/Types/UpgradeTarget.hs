{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.UpgradeTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.UpgradeTarget
  ( UpgradeTarget (..)
  -- * Smart constructor
  , mkUpgradeTarget
  -- * Lenses
  , utAutoUpgrade
  , utDescription
  , utEngine
  , utEngineVersion
  , utIsMajorVersionUpgrade
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The version of the database engine that a DB instance can be upgraded to.
--
-- /See:/ 'mkUpgradeTarget' smart constructor.
data UpgradeTarget = UpgradeTarget'
  { autoUpgrade :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the target version is applied to any source DB instances that have @AutoMinorVersionUpgrade@ set to true.
  , description :: Core.Maybe Core.Text
    -- ^ The version of the database engine that a DB instance can be upgraded to.
  , engine :: Core.Maybe Core.Text
    -- ^ The name of the upgrade target database engine.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The version number of the upgrade target database engine.
  , isMajorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether a database engine is upgraded to a major version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpgradeTarget' value with any optional fields omitted.
mkUpgradeTarget
    :: UpgradeTarget
mkUpgradeTarget
  = UpgradeTarget'{autoUpgrade = Core.Nothing,
                   description = Core.Nothing, engine = Core.Nothing,
                   engineVersion = Core.Nothing, isMajorVersionUpgrade = Core.Nothing}

-- | A value that indicates whether the target version is applied to any source DB instances that have @AutoMinorVersionUpgrade@ set to true.
--
-- /Note:/ Consider using 'autoUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utAutoUpgrade :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Bool)
utAutoUpgrade = Lens.field @"autoUpgrade"
{-# INLINEABLE utAutoUpgrade #-}
{-# DEPRECATED autoUpgrade "Use generic-lens or generic-optics with 'autoUpgrade' instead"  #-}

-- | The version of the database engine that a DB instance can be upgraded to.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDescription :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Text)
utDescription = Lens.field @"description"
{-# INLINEABLE utDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the upgrade target database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utEngine :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Text)
utEngine = Lens.field @"engine"
{-# INLINEABLE utEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The version number of the upgrade target database engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utEngineVersion :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Text)
utEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE utEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | A value that indicates whether a database engine is upgraded to a major version.
--
-- /Note:/ Consider using 'isMajorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utIsMajorVersionUpgrade :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Bool)
utIsMajorVersionUpgrade = Lens.field @"isMajorVersionUpgrade"
{-# INLINEABLE utIsMajorVersionUpgrade #-}
{-# DEPRECATED isMajorVersionUpgrade "Use generic-lens or generic-optics with 'isMajorVersionUpgrade' instead"  #-}

instance Core.FromXML UpgradeTarget where
        parseXML x
          = UpgradeTarget' Core.<$>
              (x Core..@? "AutoUpgrade") Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "Engine"
                Core.<*> x Core..@? "EngineVersion"
                Core.<*> x Core..@? "IsMajorVersionUpgrade"
