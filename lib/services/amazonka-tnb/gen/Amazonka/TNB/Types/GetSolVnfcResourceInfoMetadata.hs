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
-- Module      : Amazonka.TNB.Types.GetSolVnfcResourceInfoMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.GetSolVnfcResourceInfoMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata of a network function.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
--
-- /See:/ 'newGetSolVnfcResourceInfoMetadata' smart constructor.
data GetSolVnfcResourceInfoMetadata = GetSolVnfcResourceInfoMetadata'
  { -- | Information about the cluster.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Information about the helm chart.
    helmChart :: Prelude.Maybe Prelude.Text,
    -- | Information about the node group.
    nodeGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolVnfcResourceInfoMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'getSolVnfcResourceInfoMetadata_cluster' - Information about the cluster.
--
-- 'helmChart', 'getSolVnfcResourceInfoMetadata_helmChart' - Information about the helm chart.
--
-- 'nodeGroup', 'getSolVnfcResourceInfoMetadata_nodeGroup' - Information about the node group.
newGetSolVnfcResourceInfoMetadata ::
  GetSolVnfcResourceInfoMetadata
newGetSolVnfcResourceInfoMetadata =
  GetSolVnfcResourceInfoMetadata'
    { cluster =
        Prelude.Nothing,
      helmChart = Prelude.Nothing,
      nodeGroup = Prelude.Nothing
    }

-- | Information about the cluster.
getSolVnfcResourceInfoMetadata_cluster :: Lens.Lens' GetSolVnfcResourceInfoMetadata (Prelude.Maybe Prelude.Text)
getSolVnfcResourceInfoMetadata_cluster = Lens.lens (\GetSolVnfcResourceInfoMetadata' {cluster} -> cluster) (\s@GetSolVnfcResourceInfoMetadata' {} a -> s {cluster = a} :: GetSolVnfcResourceInfoMetadata)

-- | Information about the helm chart.
getSolVnfcResourceInfoMetadata_helmChart :: Lens.Lens' GetSolVnfcResourceInfoMetadata (Prelude.Maybe Prelude.Text)
getSolVnfcResourceInfoMetadata_helmChart = Lens.lens (\GetSolVnfcResourceInfoMetadata' {helmChart} -> helmChart) (\s@GetSolVnfcResourceInfoMetadata' {} a -> s {helmChart = a} :: GetSolVnfcResourceInfoMetadata)

-- | Information about the node group.
getSolVnfcResourceInfoMetadata_nodeGroup :: Lens.Lens' GetSolVnfcResourceInfoMetadata (Prelude.Maybe Prelude.Text)
getSolVnfcResourceInfoMetadata_nodeGroup = Lens.lens (\GetSolVnfcResourceInfoMetadata' {nodeGroup} -> nodeGroup) (\s@GetSolVnfcResourceInfoMetadata' {} a -> s {nodeGroup = a} :: GetSolVnfcResourceInfoMetadata)

instance Data.FromJSON GetSolVnfcResourceInfoMetadata where
  parseJSON =
    Data.withObject
      "GetSolVnfcResourceInfoMetadata"
      ( \x ->
          GetSolVnfcResourceInfoMetadata'
            Prelude.<$> (x Data..:? "cluster")
            Prelude.<*> (x Data..:? "helmChart")
            Prelude.<*> (x Data..:? "nodeGroup")
      )

instance
  Prelude.Hashable
    GetSolVnfcResourceInfoMetadata
  where
  hashWithSalt
    _salt
    GetSolVnfcResourceInfoMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` cluster
        `Prelude.hashWithSalt` helmChart
        `Prelude.hashWithSalt` nodeGroup

instance
  Prelude.NFData
    GetSolVnfcResourceInfoMetadata
  where
  rnf GetSolVnfcResourceInfoMetadata' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf helmChart
      `Prelude.seq` Prelude.rnf nodeGroup
