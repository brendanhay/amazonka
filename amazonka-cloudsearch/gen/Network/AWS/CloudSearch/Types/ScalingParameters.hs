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
-- Module      : Network.AWS.CloudSearch.Types.ScalingParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ScalingParameters where

import Network.AWS.CloudSearch.Types.PartitionInstanceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The desired instance type and desired number of replicas of each index
-- partition.
--
-- /See:/ 'newScalingParameters' smart constructor.
data ScalingParameters = ScalingParameters'
  { -- | The number of replicas you want to preconfigure for each index
    -- partition.
    desiredReplicationCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of partitions you want to preconfigure for your domain. Only
    -- valid when you select @m2.2xlarge@ as the desired instance type.
    desiredPartitionCount :: Prelude.Maybe Prelude.Natural,
    -- | The instance type that you want to preconfigure for your domain. For
    -- example, @search.m1.small@.
    desiredInstanceType :: Prelude.Maybe PartitionInstanceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalingParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredReplicationCount', 'scalingParameters_desiredReplicationCount' - The number of replicas you want to preconfigure for each index
-- partition.
--
-- 'desiredPartitionCount', 'scalingParameters_desiredPartitionCount' - The number of partitions you want to preconfigure for your domain. Only
-- valid when you select @m2.2xlarge@ as the desired instance type.
--
-- 'desiredInstanceType', 'scalingParameters_desiredInstanceType' - The instance type that you want to preconfigure for your domain. For
-- example, @search.m1.small@.
newScalingParameters ::
  ScalingParameters
newScalingParameters =
  ScalingParameters'
    { desiredReplicationCount =
        Prelude.Nothing,
      desiredPartitionCount = Prelude.Nothing,
      desiredInstanceType = Prelude.Nothing
    }

-- | The number of replicas you want to preconfigure for each index
-- partition.
scalingParameters_desiredReplicationCount :: Lens.Lens' ScalingParameters (Prelude.Maybe Prelude.Natural)
scalingParameters_desiredReplicationCount = Lens.lens (\ScalingParameters' {desiredReplicationCount} -> desiredReplicationCount) (\s@ScalingParameters' {} a -> s {desiredReplicationCount = a} :: ScalingParameters)

-- | The number of partitions you want to preconfigure for your domain. Only
-- valid when you select @m2.2xlarge@ as the desired instance type.
scalingParameters_desiredPartitionCount :: Lens.Lens' ScalingParameters (Prelude.Maybe Prelude.Natural)
scalingParameters_desiredPartitionCount = Lens.lens (\ScalingParameters' {desiredPartitionCount} -> desiredPartitionCount) (\s@ScalingParameters' {} a -> s {desiredPartitionCount = a} :: ScalingParameters)

-- | The instance type that you want to preconfigure for your domain. For
-- example, @search.m1.small@.
scalingParameters_desiredInstanceType :: Lens.Lens' ScalingParameters (Prelude.Maybe PartitionInstanceType)
scalingParameters_desiredInstanceType = Lens.lens (\ScalingParameters' {desiredInstanceType} -> desiredInstanceType) (\s@ScalingParameters' {} a -> s {desiredInstanceType = a} :: ScalingParameters)

instance Core.FromXML ScalingParameters where
  parseXML x =
    ScalingParameters'
      Prelude.<$> (x Core..@? "DesiredReplicationCount")
      Prelude.<*> (x Core..@? "DesiredPartitionCount")
      Prelude.<*> (x Core..@? "DesiredInstanceType")

instance Prelude.Hashable ScalingParameters

instance Prelude.NFData ScalingParameters

instance Core.ToQuery ScalingParameters where
  toQuery ScalingParameters' {..} =
    Prelude.mconcat
      [ "DesiredReplicationCount"
          Core.=: desiredReplicationCount,
        "DesiredPartitionCount"
          Core.=: desiredPartitionCount,
        "DesiredInstanceType" Core.=: desiredInstanceType
      ]
