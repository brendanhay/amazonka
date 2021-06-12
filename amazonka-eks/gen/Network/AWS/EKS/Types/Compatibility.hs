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
-- Module      : Network.AWS.EKS.Types.Compatibility
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.Compatibility where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Compatibility information.
--
-- /See:/ 'newCompatibility' smart constructor.
data Compatibility = Compatibility'
  { -- | The supported default version.
    defaultVersion :: Core.Maybe Core.Bool,
    -- | The supported compute platform.
    platformVersions :: Core.Maybe [Core.Text],
    -- | The supported Kubernetes version of the cluster.
    clusterVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Compatibility' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultVersion', 'compatibility_defaultVersion' - The supported default version.
--
-- 'platformVersions', 'compatibility_platformVersions' - The supported compute platform.
--
-- 'clusterVersion', 'compatibility_clusterVersion' - The supported Kubernetes version of the cluster.
newCompatibility ::
  Compatibility
newCompatibility =
  Compatibility'
    { defaultVersion = Core.Nothing,
      platformVersions = Core.Nothing,
      clusterVersion = Core.Nothing
    }

-- | The supported default version.
compatibility_defaultVersion :: Lens.Lens' Compatibility (Core.Maybe Core.Bool)
compatibility_defaultVersion = Lens.lens (\Compatibility' {defaultVersion} -> defaultVersion) (\s@Compatibility' {} a -> s {defaultVersion = a} :: Compatibility)

-- | The supported compute platform.
compatibility_platformVersions :: Lens.Lens' Compatibility (Core.Maybe [Core.Text])
compatibility_platformVersions = Lens.lens (\Compatibility' {platformVersions} -> platformVersions) (\s@Compatibility' {} a -> s {platformVersions = a} :: Compatibility) Core.. Lens.mapping Lens._Coerce

-- | The supported Kubernetes version of the cluster.
compatibility_clusterVersion :: Lens.Lens' Compatibility (Core.Maybe Core.Text)
compatibility_clusterVersion = Lens.lens (\Compatibility' {clusterVersion} -> clusterVersion) (\s@Compatibility' {} a -> s {clusterVersion = a} :: Compatibility)

instance Core.FromJSON Compatibility where
  parseJSON =
    Core.withObject
      "Compatibility"
      ( \x ->
          Compatibility'
            Core.<$> (x Core..:? "defaultVersion")
            Core.<*> (x Core..:? "platformVersions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "clusterVersion")
      )

instance Core.Hashable Compatibility

instance Core.NFData Compatibility
