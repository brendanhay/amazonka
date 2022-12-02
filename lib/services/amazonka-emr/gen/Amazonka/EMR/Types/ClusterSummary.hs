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
-- Module      : Amazonka.EMR.Types.ClusterSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ClusterSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.ClusterStatus
import qualified Amazonka.Prelude as Prelude

-- | The summary description of the cluster.
--
-- /See:/ 'newClusterSummary' smart constructor.
data ClusterSummary = ClusterSummary'
  { -- | The Amazon Resource Name of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost where the cluster is
    -- launched.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The details about the current status of the cluster.
    status :: Prelude.Maybe ClusterStatus,
    -- | The unique identifier for the cluster.
    id :: Prelude.Maybe Prelude.Text,
    -- | An approximation of the cost of the cluster, represented in
    -- m1.small\/hours. This value is incremented one time for every hour an
    -- m1.small instance runs. Larger instances are weighted more, so an EC2
    -- instance that is roughly four times more expensive would result in the
    -- normalized instance hours being incremented by four. This result is only
    -- an approximation and does not reflect the actual billing rate.
    normalizedInstanceHours :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'clusterSummary_clusterArn' - The Amazon Resource Name of the cluster.
--
-- 'name', 'clusterSummary_name' - The name of the cluster.
--
-- 'outpostArn', 'clusterSummary_outpostArn' - The Amazon Resource Name (ARN) of the Outpost where the cluster is
-- launched.
--
-- 'status', 'clusterSummary_status' - The details about the current status of the cluster.
--
-- 'id', 'clusterSummary_id' - The unique identifier for the cluster.
--
-- 'normalizedInstanceHours', 'clusterSummary_normalizedInstanceHours' - An approximation of the cost of the cluster, represented in
-- m1.small\/hours. This value is incremented one time for every hour an
-- m1.small instance runs. Larger instances are weighted more, so an EC2
-- instance that is roughly four times more expensive would result in the
-- normalized instance hours being incremented by four. This result is only
-- an approximation and does not reflect the actual billing rate.
newClusterSummary ::
  ClusterSummary
newClusterSummary =
  ClusterSummary'
    { clusterArn = Prelude.Nothing,
      name = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      normalizedInstanceHours = Prelude.Nothing
    }

-- | The Amazon Resource Name of the cluster.
clusterSummary_clusterArn :: Lens.Lens' ClusterSummary (Prelude.Maybe Prelude.Text)
clusterSummary_clusterArn = Lens.lens (\ClusterSummary' {clusterArn} -> clusterArn) (\s@ClusterSummary' {} a -> s {clusterArn = a} :: ClusterSummary)

-- | The name of the cluster.
clusterSummary_name :: Lens.Lens' ClusterSummary (Prelude.Maybe Prelude.Text)
clusterSummary_name = Lens.lens (\ClusterSummary' {name} -> name) (\s@ClusterSummary' {} a -> s {name = a} :: ClusterSummary)

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is
-- launched.
clusterSummary_outpostArn :: Lens.Lens' ClusterSummary (Prelude.Maybe Prelude.Text)
clusterSummary_outpostArn = Lens.lens (\ClusterSummary' {outpostArn} -> outpostArn) (\s@ClusterSummary' {} a -> s {outpostArn = a} :: ClusterSummary)

-- | The details about the current status of the cluster.
clusterSummary_status :: Lens.Lens' ClusterSummary (Prelude.Maybe ClusterStatus)
clusterSummary_status = Lens.lens (\ClusterSummary' {status} -> status) (\s@ClusterSummary' {} a -> s {status = a} :: ClusterSummary)

-- | The unique identifier for the cluster.
clusterSummary_id :: Lens.Lens' ClusterSummary (Prelude.Maybe Prelude.Text)
clusterSummary_id = Lens.lens (\ClusterSummary' {id} -> id) (\s@ClusterSummary' {} a -> s {id = a} :: ClusterSummary)

-- | An approximation of the cost of the cluster, represented in
-- m1.small\/hours. This value is incremented one time for every hour an
-- m1.small instance runs. Larger instances are weighted more, so an EC2
-- instance that is roughly four times more expensive would result in the
-- normalized instance hours being incremented by four. This result is only
-- an approximation and does not reflect the actual billing rate.
clusterSummary_normalizedInstanceHours :: Lens.Lens' ClusterSummary (Prelude.Maybe Prelude.Int)
clusterSummary_normalizedInstanceHours = Lens.lens (\ClusterSummary' {normalizedInstanceHours} -> normalizedInstanceHours) (\s@ClusterSummary' {} a -> s {normalizedInstanceHours = a} :: ClusterSummary)

instance Data.FromJSON ClusterSummary where
  parseJSON =
    Data.withObject
      "ClusterSummary"
      ( \x ->
          ClusterSummary'
            Prelude.<$> (x Data..:? "ClusterArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OutpostArn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "NormalizedInstanceHours")
      )

instance Prelude.Hashable ClusterSummary where
  hashWithSalt _salt ClusterSummary' {..} =
    _salt `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` normalizedInstanceHours

instance Prelude.NFData ClusterSummary where
  rnf ClusterSummary' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf normalizedInstanceHours
