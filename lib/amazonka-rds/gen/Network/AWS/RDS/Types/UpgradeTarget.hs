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
    utEngineVersion,
    utIsMajorVersionUpgrade,
    utEngine,
    utAutoUpgrade,
    utDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The version of the database engine that a DB instance can be upgraded to.
--
-- /See:/ 'mkUpgradeTarget' smart constructor.
data UpgradeTarget = UpgradeTarget'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    isMajorVersionUpgrade :: Lude.Maybe Lude.Bool,
    engine :: Lude.Maybe Lude.Text,
    autoUpgrade :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpgradeTarget' with the minimum fields required to make a request.
--
-- * 'autoUpgrade' - A value that indicates whether the target version is applied to any source DB instances that have @AutoMinorVersionUpgrade@ set to true.
-- * 'description' - The version of the database engine that a DB instance can be upgraded to.
-- * 'engine' - The name of the upgrade target database engine.
-- * 'engineVersion' - The version number of the upgrade target database engine.
-- * 'isMajorVersionUpgrade' - A value that indicates whether a database engine is upgraded to a major version.
mkUpgradeTarget ::
  UpgradeTarget
mkUpgradeTarget =
  UpgradeTarget'
    { engineVersion = Lude.Nothing,
      isMajorVersionUpgrade = Lude.Nothing,
      engine = Lude.Nothing,
      autoUpgrade = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The version number of the upgrade target database engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utEngineVersion :: Lens.Lens' UpgradeTarget (Lude.Maybe Lude.Text)
utEngineVersion = Lens.lens (engineVersion :: UpgradeTarget -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: UpgradeTarget)
{-# DEPRECATED utEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A value that indicates whether a database engine is upgraded to a major version.
--
-- /Note:/ Consider using 'isMajorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utIsMajorVersionUpgrade :: Lens.Lens' UpgradeTarget (Lude.Maybe Lude.Bool)
utIsMajorVersionUpgrade = Lens.lens (isMajorVersionUpgrade :: UpgradeTarget -> Lude.Maybe Lude.Bool) (\s a -> s {isMajorVersionUpgrade = a} :: UpgradeTarget)
{-# DEPRECATED utIsMajorVersionUpgrade "Use generic-lens or generic-optics with 'isMajorVersionUpgrade' instead." #-}

-- | The name of the upgrade target database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utEngine :: Lens.Lens' UpgradeTarget (Lude.Maybe Lude.Text)
utEngine = Lens.lens (engine :: UpgradeTarget -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: UpgradeTarget)
{-# DEPRECATED utEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | A value that indicates whether the target version is applied to any source DB instances that have @AutoMinorVersionUpgrade@ set to true.
--
-- /Note:/ Consider using 'autoUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utAutoUpgrade :: Lens.Lens' UpgradeTarget (Lude.Maybe Lude.Bool)
utAutoUpgrade = Lens.lens (autoUpgrade :: UpgradeTarget -> Lude.Maybe Lude.Bool) (\s a -> s {autoUpgrade = a} :: UpgradeTarget)
{-# DEPRECATED utAutoUpgrade "Use generic-lens or generic-optics with 'autoUpgrade' instead." #-}

-- | The version of the database engine that a DB instance can be upgraded to.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDescription :: Lens.Lens' UpgradeTarget (Lude.Maybe Lude.Text)
utDescription = Lens.lens (description :: UpgradeTarget -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpgradeTarget)
{-# DEPRECATED utDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML UpgradeTarget where
  parseXML x =
    UpgradeTarget'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "IsMajorVersionUpgrade")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "AutoUpgrade")
      Lude.<*> (x Lude..@? "Description")
