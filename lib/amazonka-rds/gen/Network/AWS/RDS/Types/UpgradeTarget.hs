{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.UpgradeTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.UpgradeTarget
  ( UpgradeTarget (..),

    -- * Smart constructor
    mkUpgradeTarget,

    -- * Lenses
    utAutoUpgrade,
    utDescription,
    utEngine,
    utEngineVersion,
    utIsMajorVersionUpgrade,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | The version of the database engine that a DB instance can be upgraded to.
--
-- /See:/ 'mkUpgradeTarget' smart constructor.
data UpgradeTarget = UpgradeTarget'
  { -- | A value that indicates whether the target version is applied to any source DB instances that have @AutoMinorVersionUpgrade@ set to true.
    autoUpgrade :: Core.Maybe Core.Bool,
    -- | The version of the database engine that a DB instance can be upgraded to.
    description :: Core.Maybe Types.String,
    -- | The name of the upgrade target database engine.
    engine :: Core.Maybe Types.String,
    -- | The version number of the upgrade target database engine.
    engineVersion :: Core.Maybe Types.String,
    -- | A value that indicates whether a database engine is upgraded to a major version.
    isMajorVersionUpgrade :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpgradeTarget' value with any optional fields omitted.
mkUpgradeTarget ::
  UpgradeTarget
mkUpgradeTarget =
  UpgradeTarget'
    { autoUpgrade = Core.Nothing,
      description = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      isMajorVersionUpgrade = Core.Nothing
    }

-- | A value that indicates whether the target version is applied to any source DB instances that have @AutoMinorVersionUpgrade@ set to true.
--
-- /Note:/ Consider using 'autoUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utAutoUpgrade :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Bool)
utAutoUpgrade = Lens.field @"autoUpgrade"
{-# DEPRECATED utAutoUpgrade "Use generic-lens or generic-optics with 'autoUpgrade' instead." #-}

-- | The version of the database engine that a DB instance can be upgraded to.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDescription :: Lens.Lens' UpgradeTarget (Core.Maybe Types.String)
utDescription = Lens.field @"description"
{-# DEPRECATED utDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the upgrade target database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utEngine :: Lens.Lens' UpgradeTarget (Core.Maybe Types.String)
utEngine = Lens.field @"engine"
{-# DEPRECATED utEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The version number of the upgrade target database engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utEngineVersion :: Lens.Lens' UpgradeTarget (Core.Maybe Types.String)
utEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED utEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A value that indicates whether a database engine is upgraded to a major version.
--
-- /Note:/ Consider using 'isMajorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utIsMajorVersionUpgrade :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Bool)
utIsMajorVersionUpgrade = Lens.field @"isMajorVersionUpgrade"
{-# DEPRECATED utIsMajorVersionUpgrade "Use generic-lens or generic-optics with 'isMajorVersionUpgrade' instead." #-}

instance Core.FromXML UpgradeTarget where
  parseXML x =
    UpgradeTarget'
      Core.<$> (x Core..@? "AutoUpgrade")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "IsMajorVersionUpgrade")
