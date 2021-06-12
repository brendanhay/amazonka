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
-- Module      : Network.AWS.Redshift.Types.ClusterVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterVersion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes a cluster version, including the parameter group family and
-- description of the version.
--
-- /See:/ 'newClusterVersion' smart constructor.
data ClusterVersion = ClusterVersion'
  { -- | The name of the cluster parameter group family for the cluster.
    clusterParameterGroupFamily :: Core.Maybe Core.Text,
    -- | The description of the cluster version.
    description :: Core.Maybe Core.Text,
    -- | The version number used by the cluster.
    clusterVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterParameterGroupFamily', 'clusterVersion_clusterParameterGroupFamily' - The name of the cluster parameter group family for the cluster.
--
-- 'description', 'clusterVersion_description' - The description of the cluster version.
--
-- 'clusterVersion', 'clusterVersion_clusterVersion' - The version number used by the cluster.
newClusterVersion ::
  ClusterVersion
newClusterVersion =
  ClusterVersion'
    { clusterParameterGroupFamily =
        Core.Nothing,
      description = Core.Nothing,
      clusterVersion = Core.Nothing
    }

-- | The name of the cluster parameter group family for the cluster.
clusterVersion_clusterParameterGroupFamily :: Lens.Lens' ClusterVersion (Core.Maybe Core.Text)
clusterVersion_clusterParameterGroupFamily = Lens.lens (\ClusterVersion' {clusterParameterGroupFamily} -> clusterParameterGroupFamily) (\s@ClusterVersion' {} a -> s {clusterParameterGroupFamily = a} :: ClusterVersion)

-- | The description of the cluster version.
clusterVersion_description :: Lens.Lens' ClusterVersion (Core.Maybe Core.Text)
clusterVersion_description = Lens.lens (\ClusterVersion' {description} -> description) (\s@ClusterVersion' {} a -> s {description = a} :: ClusterVersion)

-- | The version number used by the cluster.
clusterVersion_clusterVersion :: Lens.Lens' ClusterVersion (Core.Maybe Core.Text)
clusterVersion_clusterVersion = Lens.lens (\ClusterVersion' {clusterVersion} -> clusterVersion) (\s@ClusterVersion' {} a -> s {clusterVersion = a} :: ClusterVersion)

instance Core.FromXML ClusterVersion where
  parseXML x =
    ClusterVersion'
      Core.<$> (x Core..@? "ClusterParameterGroupFamily")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "ClusterVersion")

instance Core.Hashable ClusterVersion

instance Core.NFData ClusterVersion
