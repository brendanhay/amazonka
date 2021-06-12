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
-- Module      : Network.AWS.CostExplorer.Types.EC2ResourceUtilization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2ResourceUtilization where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.EBSResourceUtilization
import qualified Network.AWS.Lens as Lens

-- | Utilization metrics of the instance.
--
-- /See:/ 'newEC2ResourceUtilization' smart constructor.
data EC2ResourceUtilization = EC2ResourceUtilization'
  { -- | Maximum observed or expected storage utilization of the instance (does
    -- not measure EBS storage).
    maxStorageUtilizationPercentage :: Core.Maybe Core.Text,
    -- | Maximum observed or expected memory utilization of the instance.
    maxMemoryUtilizationPercentage :: Core.Maybe Core.Text,
    -- | The EBS field that contains a list of EBS metrics associated with the
    -- current instance.
    eBSResourceUtilization :: Core.Maybe EBSResourceUtilization,
    -- | Maximum observed or expected CPU utilization of the instance.
    maxCpuUtilizationPercentage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EC2ResourceUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxStorageUtilizationPercentage', 'eC2ResourceUtilization_maxStorageUtilizationPercentage' - Maximum observed or expected storage utilization of the instance (does
-- not measure EBS storage).
--
-- 'maxMemoryUtilizationPercentage', 'eC2ResourceUtilization_maxMemoryUtilizationPercentage' - Maximum observed or expected memory utilization of the instance.
--
-- 'eBSResourceUtilization', 'eC2ResourceUtilization_eBSResourceUtilization' - The EBS field that contains a list of EBS metrics associated with the
-- current instance.
--
-- 'maxCpuUtilizationPercentage', 'eC2ResourceUtilization_maxCpuUtilizationPercentage' - Maximum observed or expected CPU utilization of the instance.
newEC2ResourceUtilization ::
  EC2ResourceUtilization
newEC2ResourceUtilization =
  EC2ResourceUtilization'
    { maxStorageUtilizationPercentage =
        Core.Nothing,
      maxMemoryUtilizationPercentage = Core.Nothing,
      eBSResourceUtilization = Core.Nothing,
      maxCpuUtilizationPercentage = Core.Nothing
    }

-- | Maximum observed or expected storage utilization of the instance (does
-- not measure EBS storage).
eC2ResourceUtilization_maxStorageUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Core.Maybe Core.Text)
eC2ResourceUtilization_maxStorageUtilizationPercentage = Lens.lens (\EC2ResourceUtilization' {maxStorageUtilizationPercentage} -> maxStorageUtilizationPercentage) (\s@EC2ResourceUtilization' {} a -> s {maxStorageUtilizationPercentage = a} :: EC2ResourceUtilization)

-- | Maximum observed or expected memory utilization of the instance.
eC2ResourceUtilization_maxMemoryUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Core.Maybe Core.Text)
eC2ResourceUtilization_maxMemoryUtilizationPercentage = Lens.lens (\EC2ResourceUtilization' {maxMemoryUtilizationPercentage} -> maxMemoryUtilizationPercentage) (\s@EC2ResourceUtilization' {} a -> s {maxMemoryUtilizationPercentage = a} :: EC2ResourceUtilization)

-- | The EBS field that contains a list of EBS metrics associated with the
-- current instance.
eC2ResourceUtilization_eBSResourceUtilization :: Lens.Lens' EC2ResourceUtilization (Core.Maybe EBSResourceUtilization)
eC2ResourceUtilization_eBSResourceUtilization = Lens.lens (\EC2ResourceUtilization' {eBSResourceUtilization} -> eBSResourceUtilization) (\s@EC2ResourceUtilization' {} a -> s {eBSResourceUtilization = a} :: EC2ResourceUtilization)

-- | Maximum observed or expected CPU utilization of the instance.
eC2ResourceUtilization_maxCpuUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Core.Maybe Core.Text)
eC2ResourceUtilization_maxCpuUtilizationPercentage = Lens.lens (\EC2ResourceUtilization' {maxCpuUtilizationPercentage} -> maxCpuUtilizationPercentage) (\s@EC2ResourceUtilization' {} a -> s {maxCpuUtilizationPercentage = a} :: EC2ResourceUtilization)

instance Core.FromJSON EC2ResourceUtilization where
  parseJSON =
    Core.withObject
      "EC2ResourceUtilization"
      ( \x ->
          EC2ResourceUtilization'
            Core.<$> (x Core..:? "MaxStorageUtilizationPercentage")
            Core.<*> (x Core..:? "MaxMemoryUtilizationPercentage")
            Core.<*> (x Core..:? "EBSResourceUtilization")
            Core.<*> (x Core..:? "MaxCpuUtilizationPercentage")
      )

instance Core.Hashable EC2ResourceUtilization

instance Core.NFData EC2ResourceUtilization
