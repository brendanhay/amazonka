{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.UpgradeTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.UpgradeTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The version of the database engine that a DB instance can be upgraded
-- to.
--
-- /See:/ 'newUpgradeTarget' smart constructor.
data UpgradeTarget = UpgradeTarget'
  { -- | A value that indicates whether the target version is applied to any
    -- source DB instances that have @AutoMinorVersionUpgrade@ set to true.
    autoUpgrade :: Core.Maybe Core.Bool,
    -- | A list of the supported DB engine modes for the target engine version.
    supportedEngineModes :: Core.Maybe [Core.Text],
    -- | The version number of the upgrade target database engine.
    engineVersion :: Core.Maybe Core.Text,
    -- | A value that indicates whether you can use Aurora global databases with
    -- the target engine version.
    supportsGlobalDatabases :: Core.Maybe Core.Bool,
    -- | The name of the upgrade target database engine.
    engine :: Core.Maybe Core.Text,
    -- | The version of the database engine that a DB instance can be upgraded
    -- to.
    description :: Core.Maybe Core.Text,
    -- | A value that indicates whether upgrading to the target version requires
    -- upgrading the major version of the database engine.
    isMajorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | A value that indicates whether you can use Aurora parallel query with
    -- the target engine version.
    supportsParallelQuery :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpgradeTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoUpgrade', 'upgradeTarget_autoUpgrade' - A value that indicates whether the target version is applied to any
-- source DB instances that have @AutoMinorVersionUpgrade@ set to true.
--
-- 'supportedEngineModes', 'upgradeTarget_supportedEngineModes' - A list of the supported DB engine modes for the target engine version.
--
-- 'engineVersion', 'upgradeTarget_engineVersion' - The version number of the upgrade target database engine.
--
-- 'supportsGlobalDatabases', 'upgradeTarget_supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with
-- the target engine version.
--
-- 'engine', 'upgradeTarget_engine' - The name of the upgrade target database engine.
--
-- 'description', 'upgradeTarget_description' - The version of the database engine that a DB instance can be upgraded
-- to.
--
-- 'isMajorVersionUpgrade', 'upgradeTarget_isMajorVersionUpgrade' - A value that indicates whether upgrading to the target version requires
-- upgrading the major version of the database engine.
--
-- 'supportsParallelQuery', 'upgradeTarget_supportsParallelQuery' - A value that indicates whether you can use Aurora parallel query with
-- the target engine version.
newUpgradeTarget ::
  UpgradeTarget
newUpgradeTarget =
  UpgradeTarget'
    { autoUpgrade = Core.Nothing,
      supportedEngineModes = Core.Nothing,
      engineVersion = Core.Nothing,
      supportsGlobalDatabases = Core.Nothing,
      engine = Core.Nothing,
      description = Core.Nothing,
      isMajorVersionUpgrade = Core.Nothing,
      supportsParallelQuery = Core.Nothing
    }

-- | A value that indicates whether the target version is applied to any
-- source DB instances that have @AutoMinorVersionUpgrade@ set to true.
upgradeTarget_autoUpgrade :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Bool)
upgradeTarget_autoUpgrade = Lens.lens (\UpgradeTarget' {autoUpgrade} -> autoUpgrade) (\s@UpgradeTarget' {} a -> s {autoUpgrade = a} :: UpgradeTarget)

-- | A list of the supported DB engine modes for the target engine version.
upgradeTarget_supportedEngineModes :: Lens.Lens' UpgradeTarget (Core.Maybe [Core.Text])
upgradeTarget_supportedEngineModes = Lens.lens (\UpgradeTarget' {supportedEngineModes} -> supportedEngineModes) (\s@UpgradeTarget' {} a -> s {supportedEngineModes = a} :: UpgradeTarget) Core.. Lens.mapping Lens._Coerce

-- | The version number of the upgrade target database engine.
upgradeTarget_engineVersion :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Text)
upgradeTarget_engineVersion = Lens.lens (\UpgradeTarget' {engineVersion} -> engineVersion) (\s@UpgradeTarget' {} a -> s {engineVersion = a} :: UpgradeTarget)

-- | A value that indicates whether you can use Aurora global databases with
-- the target engine version.
upgradeTarget_supportsGlobalDatabases :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Bool)
upgradeTarget_supportsGlobalDatabases = Lens.lens (\UpgradeTarget' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@UpgradeTarget' {} a -> s {supportsGlobalDatabases = a} :: UpgradeTarget)

-- | The name of the upgrade target database engine.
upgradeTarget_engine :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Text)
upgradeTarget_engine = Lens.lens (\UpgradeTarget' {engine} -> engine) (\s@UpgradeTarget' {} a -> s {engine = a} :: UpgradeTarget)

-- | The version of the database engine that a DB instance can be upgraded
-- to.
upgradeTarget_description :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Text)
upgradeTarget_description = Lens.lens (\UpgradeTarget' {description} -> description) (\s@UpgradeTarget' {} a -> s {description = a} :: UpgradeTarget)

-- | A value that indicates whether upgrading to the target version requires
-- upgrading the major version of the database engine.
upgradeTarget_isMajorVersionUpgrade :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Bool)
upgradeTarget_isMajorVersionUpgrade = Lens.lens (\UpgradeTarget' {isMajorVersionUpgrade} -> isMajorVersionUpgrade) (\s@UpgradeTarget' {} a -> s {isMajorVersionUpgrade = a} :: UpgradeTarget)

-- | A value that indicates whether you can use Aurora parallel query with
-- the target engine version.
upgradeTarget_supportsParallelQuery :: Lens.Lens' UpgradeTarget (Core.Maybe Core.Bool)
upgradeTarget_supportsParallelQuery = Lens.lens (\UpgradeTarget' {supportsParallelQuery} -> supportsParallelQuery) (\s@UpgradeTarget' {} a -> s {supportsParallelQuery = a} :: UpgradeTarget)

instance Core.FromXML UpgradeTarget where
  parseXML x =
    UpgradeTarget'
      Core.<$> (x Core..@? "AutoUpgrade")
      Core.<*> ( x Core..@? "SupportedEngineModes"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "SupportsGlobalDatabases")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "IsMajorVersionUpgrade")
      Core.<*> (x Core..@? "SupportsParallelQuery")

instance Core.Hashable UpgradeTarget

instance Core.NFData UpgradeTarget
