{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes a cluster version, including the parameter group family and
-- description of the version.
--
-- /See:/ 'newClusterVersion' smart constructor.
data ClusterVersion = ClusterVersion'
  { -- | The name of the cluster parameter group family for the cluster.
    clusterParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The description of the cluster version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The version number used by the cluster.
    clusterVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      description = Prelude.Nothing,
      clusterVersion = Prelude.Nothing
    }

-- | The name of the cluster parameter group family for the cluster.
clusterVersion_clusterParameterGroupFamily :: Lens.Lens' ClusterVersion (Prelude.Maybe Prelude.Text)
clusterVersion_clusterParameterGroupFamily = Lens.lens (\ClusterVersion' {clusterParameterGroupFamily} -> clusterParameterGroupFamily) (\s@ClusterVersion' {} a -> s {clusterParameterGroupFamily = a} :: ClusterVersion)

-- | The description of the cluster version.
clusterVersion_description :: Lens.Lens' ClusterVersion (Prelude.Maybe Prelude.Text)
clusterVersion_description = Lens.lens (\ClusterVersion' {description} -> description) (\s@ClusterVersion' {} a -> s {description = a} :: ClusterVersion)

-- | The version number used by the cluster.
clusterVersion_clusterVersion :: Lens.Lens' ClusterVersion (Prelude.Maybe Prelude.Text)
clusterVersion_clusterVersion = Lens.lens (\ClusterVersion' {clusterVersion} -> clusterVersion) (\s@ClusterVersion' {} a -> s {clusterVersion = a} :: ClusterVersion)

instance Prelude.FromXML ClusterVersion where
  parseXML x =
    ClusterVersion'
      Prelude.<$> (x Prelude..@? "ClusterParameterGroupFamily")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "ClusterVersion")

instance Prelude.Hashable ClusterVersion

instance Prelude.NFData ClusterVersion
