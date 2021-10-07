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
import Network.AWS.CostExplorer.Types.DiskResourceUtilization
import Network.AWS.CostExplorer.Types.EBSResourceUtilization
import Network.AWS.CostExplorer.Types.NetworkResourceUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Utilization metrics of the instance.
--
-- /See:/ 'newEC2ResourceUtilization' smart constructor.
data EC2ResourceUtilization = EC2ResourceUtilization'
  { -- | The maximum observed or expected storage utilization of the instance.
    -- This doesn\'t include EBS storage.
    maxStorageUtilizationPercentage :: Prelude.Maybe Prelude.Text,
    -- | The maximum observed or expected memory utilization of the instance.
    maxMemoryUtilizationPercentage :: Prelude.Maybe Prelude.Text,
    -- | The EBS field that contains a list of EBS metrics that are associated
    -- with the current instance.
    eBSResourceUtilization :: Prelude.Maybe EBSResourceUtilization,
    -- | The network field that contains a list of network metrics that are
    -- associated with the current instance.
    networkResourceUtilization :: Prelude.Maybe NetworkResourceUtilization,
    -- | The maximum observed or expected CPU utilization of the instance.
    maxCpuUtilizationPercentage :: Prelude.Maybe Prelude.Text,
    -- | The field that contains a list of disk (local storage) metrics that are
    -- associated with the current instance.
    diskResourceUtilization :: Prelude.Maybe DiskResourceUtilization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2ResourceUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxStorageUtilizationPercentage', 'eC2ResourceUtilization_maxStorageUtilizationPercentage' - The maximum observed or expected storage utilization of the instance.
-- This doesn\'t include EBS storage.
--
-- 'maxMemoryUtilizationPercentage', 'eC2ResourceUtilization_maxMemoryUtilizationPercentage' - The maximum observed or expected memory utilization of the instance.
--
-- 'eBSResourceUtilization', 'eC2ResourceUtilization_eBSResourceUtilization' - The EBS field that contains a list of EBS metrics that are associated
-- with the current instance.
--
-- 'networkResourceUtilization', 'eC2ResourceUtilization_networkResourceUtilization' - The network field that contains a list of network metrics that are
-- associated with the current instance.
--
-- 'maxCpuUtilizationPercentage', 'eC2ResourceUtilization_maxCpuUtilizationPercentage' - The maximum observed or expected CPU utilization of the instance.
--
-- 'diskResourceUtilization', 'eC2ResourceUtilization_diskResourceUtilization' - The field that contains a list of disk (local storage) metrics that are
-- associated with the current instance.
newEC2ResourceUtilization ::
  EC2ResourceUtilization
newEC2ResourceUtilization =
  EC2ResourceUtilization'
    { maxStorageUtilizationPercentage =
        Prelude.Nothing,
      maxMemoryUtilizationPercentage = Prelude.Nothing,
      eBSResourceUtilization = Prelude.Nothing,
      networkResourceUtilization = Prelude.Nothing,
      maxCpuUtilizationPercentage = Prelude.Nothing,
      diskResourceUtilization = Prelude.Nothing
    }

-- | The maximum observed or expected storage utilization of the instance.
-- This doesn\'t include EBS storage.
eC2ResourceUtilization_maxStorageUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Prelude.Maybe Prelude.Text)
eC2ResourceUtilization_maxStorageUtilizationPercentage = Lens.lens (\EC2ResourceUtilization' {maxStorageUtilizationPercentage} -> maxStorageUtilizationPercentage) (\s@EC2ResourceUtilization' {} a -> s {maxStorageUtilizationPercentage = a} :: EC2ResourceUtilization)

-- | The maximum observed or expected memory utilization of the instance.
eC2ResourceUtilization_maxMemoryUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Prelude.Maybe Prelude.Text)
eC2ResourceUtilization_maxMemoryUtilizationPercentage = Lens.lens (\EC2ResourceUtilization' {maxMemoryUtilizationPercentage} -> maxMemoryUtilizationPercentage) (\s@EC2ResourceUtilization' {} a -> s {maxMemoryUtilizationPercentage = a} :: EC2ResourceUtilization)

-- | The EBS field that contains a list of EBS metrics that are associated
-- with the current instance.
eC2ResourceUtilization_eBSResourceUtilization :: Lens.Lens' EC2ResourceUtilization (Prelude.Maybe EBSResourceUtilization)
eC2ResourceUtilization_eBSResourceUtilization = Lens.lens (\EC2ResourceUtilization' {eBSResourceUtilization} -> eBSResourceUtilization) (\s@EC2ResourceUtilization' {} a -> s {eBSResourceUtilization = a} :: EC2ResourceUtilization)

-- | The network field that contains a list of network metrics that are
-- associated with the current instance.
eC2ResourceUtilization_networkResourceUtilization :: Lens.Lens' EC2ResourceUtilization (Prelude.Maybe NetworkResourceUtilization)
eC2ResourceUtilization_networkResourceUtilization = Lens.lens (\EC2ResourceUtilization' {networkResourceUtilization} -> networkResourceUtilization) (\s@EC2ResourceUtilization' {} a -> s {networkResourceUtilization = a} :: EC2ResourceUtilization)

-- | The maximum observed or expected CPU utilization of the instance.
eC2ResourceUtilization_maxCpuUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Prelude.Maybe Prelude.Text)
eC2ResourceUtilization_maxCpuUtilizationPercentage = Lens.lens (\EC2ResourceUtilization' {maxCpuUtilizationPercentage} -> maxCpuUtilizationPercentage) (\s@EC2ResourceUtilization' {} a -> s {maxCpuUtilizationPercentage = a} :: EC2ResourceUtilization)

-- | The field that contains a list of disk (local storage) metrics that are
-- associated with the current instance.
eC2ResourceUtilization_diskResourceUtilization :: Lens.Lens' EC2ResourceUtilization (Prelude.Maybe DiskResourceUtilization)
eC2ResourceUtilization_diskResourceUtilization = Lens.lens (\EC2ResourceUtilization' {diskResourceUtilization} -> diskResourceUtilization) (\s@EC2ResourceUtilization' {} a -> s {diskResourceUtilization = a} :: EC2ResourceUtilization)

instance Core.FromJSON EC2ResourceUtilization where
  parseJSON =
    Core.withObject
      "EC2ResourceUtilization"
      ( \x ->
          EC2ResourceUtilization'
            Prelude.<$> (x Core..:? "MaxStorageUtilizationPercentage")
            Prelude.<*> (x Core..:? "MaxMemoryUtilizationPercentage")
            Prelude.<*> (x Core..:? "EBSResourceUtilization")
            Prelude.<*> (x Core..:? "NetworkResourceUtilization")
            Prelude.<*> (x Core..:? "MaxCpuUtilizationPercentage")
            Prelude.<*> (x Core..:? "DiskResourceUtilization")
      )

instance Prelude.Hashable EC2ResourceUtilization

instance Prelude.NFData EC2ResourceUtilization
