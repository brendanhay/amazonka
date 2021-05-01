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
-- Module      : Network.AWS.EMR.Types.ClusterSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterSummary where

import Network.AWS.EMR.Types.ClusterStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The summary description of the cluster.
--
-- /See:/ 'newClusterSummary' smart constructor.
data ClusterSummary = ClusterSummary'
  { -- | The Amazon Resource Name of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The details about the current status of the cluster.
    status :: Prelude.Maybe ClusterStatus,
    -- | The Amazon Resource Name (ARN) of the Outpost where the cluster is
    -- launched.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the cluster.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | An approximation of the cost of the cluster, represented in
    -- m1.small\/hours. This value is incremented one time for every hour an
    -- m1.small instance runs. Larger instances are weighted more, so an EC2
    -- instance that is roughly four times more expensive would result in the
    -- normalized instance hours being incremented by four. This result is only
    -- an approximation and does not reflect the actual billing rate.
    normalizedInstanceHours :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'status', 'clusterSummary_status' - The details about the current status of the cluster.
--
-- 'outpostArn', 'clusterSummary_outpostArn' - The Amazon Resource Name (ARN) of the Outpost where the cluster is
-- launched.
--
-- 'id', 'clusterSummary_id' - The unique identifier for the cluster.
--
-- 'name', 'clusterSummary_name' - The name of the cluster.
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
      status = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      normalizedInstanceHours = Prelude.Nothing
    }

-- | The Amazon Resource Name of the cluster.
clusterSummary_clusterArn :: Lens.Lens' ClusterSummary (Prelude.Maybe Prelude.Text)
clusterSummary_clusterArn = Lens.lens (\ClusterSummary' {clusterArn} -> clusterArn) (\s@ClusterSummary' {} a -> s {clusterArn = a} :: ClusterSummary)

-- | The details about the current status of the cluster.
clusterSummary_status :: Lens.Lens' ClusterSummary (Prelude.Maybe ClusterStatus)
clusterSummary_status = Lens.lens (\ClusterSummary' {status} -> status) (\s@ClusterSummary' {} a -> s {status = a} :: ClusterSummary)

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is
-- launched.
clusterSummary_outpostArn :: Lens.Lens' ClusterSummary (Prelude.Maybe Prelude.Text)
clusterSummary_outpostArn = Lens.lens (\ClusterSummary' {outpostArn} -> outpostArn) (\s@ClusterSummary' {} a -> s {outpostArn = a} :: ClusterSummary)

-- | The unique identifier for the cluster.
clusterSummary_id :: Lens.Lens' ClusterSummary (Prelude.Maybe Prelude.Text)
clusterSummary_id = Lens.lens (\ClusterSummary' {id} -> id) (\s@ClusterSummary' {} a -> s {id = a} :: ClusterSummary)

-- | The name of the cluster.
clusterSummary_name :: Lens.Lens' ClusterSummary (Prelude.Maybe Prelude.Text)
clusterSummary_name = Lens.lens (\ClusterSummary' {name} -> name) (\s@ClusterSummary' {} a -> s {name = a} :: ClusterSummary)

-- | An approximation of the cost of the cluster, represented in
-- m1.small\/hours. This value is incremented one time for every hour an
-- m1.small instance runs. Larger instances are weighted more, so an EC2
-- instance that is roughly four times more expensive would result in the
-- normalized instance hours being incremented by four. This result is only
-- an approximation and does not reflect the actual billing rate.
clusterSummary_normalizedInstanceHours :: Lens.Lens' ClusterSummary (Prelude.Maybe Prelude.Int)
clusterSummary_normalizedInstanceHours = Lens.lens (\ClusterSummary' {normalizedInstanceHours} -> normalizedInstanceHours) (\s@ClusterSummary' {} a -> s {normalizedInstanceHours = a} :: ClusterSummary)

instance Prelude.FromJSON ClusterSummary where
  parseJSON =
    Prelude.withObject
      "ClusterSummary"
      ( \x ->
          ClusterSummary'
            Prelude.<$> (x Prelude..:? "ClusterArn")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "OutpostArn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "NormalizedInstanceHours")
      )

instance Prelude.Hashable ClusterSummary

instance Prelude.NFData ClusterSummary
