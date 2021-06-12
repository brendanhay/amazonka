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

-- | The desired instance type and desired number of replicas of each index
-- partition.
--
-- /See:/ 'newScalingParameters' smart constructor.
data ScalingParameters = ScalingParameters'
  { -- | The number of replicas you want to preconfigure for each index
    -- partition.
    desiredReplicationCount :: Core.Maybe Core.Natural,
    -- | The number of partitions you want to preconfigure for your domain. Only
    -- valid when you select @m2.2xlarge@ as the desired instance type.
    desiredPartitionCount :: Core.Maybe Core.Natural,
    -- | The instance type that you want to preconfigure for your domain. For
    -- example, @search.m1.small@.
    desiredInstanceType :: Core.Maybe PartitionInstanceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      desiredPartitionCount = Core.Nothing,
      desiredInstanceType = Core.Nothing
    }

-- | The number of replicas you want to preconfigure for each index
-- partition.
scalingParameters_desiredReplicationCount :: Lens.Lens' ScalingParameters (Core.Maybe Core.Natural)
scalingParameters_desiredReplicationCount = Lens.lens (\ScalingParameters' {desiredReplicationCount} -> desiredReplicationCount) (\s@ScalingParameters' {} a -> s {desiredReplicationCount = a} :: ScalingParameters)

-- | The number of partitions you want to preconfigure for your domain. Only
-- valid when you select @m2.2xlarge@ as the desired instance type.
scalingParameters_desiredPartitionCount :: Lens.Lens' ScalingParameters (Core.Maybe Core.Natural)
scalingParameters_desiredPartitionCount = Lens.lens (\ScalingParameters' {desiredPartitionCount} -> desiredPartitionCount) (\s@ScalingParameters' {} a -> s {desiredPartitionCount = a} :: ScalingParameters)

-- | The instance type that you want to preconfigure for your domain. For
-- example, @search.m1.small@.
scalingParameters_desiredInstanceType :: Lens.Lens' ScalingParameters (Core.Maybe PartitionInstanceType)
scalingParameters_desiredInstanceType = Lens.lens (\ScalingParameters' {desiredInstanceType} -> desiredInstanceType) (\s@ScalingParameters' {} a -> s {desiredInstanceType = a} :: ScalingParameters)

instance Core.FromXML ScalingParameters where
  parseXML x =
    ScalingParameters'
      Core.<$> (x Core..@? "DesiredReplicationCount")
      Core.<*> (x Core..@? "DesiredPartitionCount")
      Core.<*> (x Core..@? "DesiredInstanceType")

instance Core.Hashable ScalingParameters

instance Core.NFData ScalingParameters

instance Core.ToQuery ScalingParameters where
  toQuery ScalingParameters' {..} =
    Core.mconcat
      [ "DesiredReplicationCount"
          Core.=: desiredReplicationCount,
        "DesiredPartitionCount"
          Core.=: desiredPartitionCount,
        "DesiredInstanceType" Core.=: desiredInstanceType
      ]
