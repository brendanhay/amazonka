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
-- Module      : Network.AWS.CostExplorer.Types.EC2ResourceUtilization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2ResourceUtilization where

import Network.AWS.CostExplorer.Types.EBSResourceUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Utilization metrics of the instance.
--
-- /See:/ 'newEC2ResourceUtilization' smart constructor.
data EC2ResourceUtilization = EC2ResourceUtilization'
  { -- | Maximum observed or expected storage utilization of the instance (does
    -- not measure EBS storage).
    maxStorageUtilizationPercentage :: Prelude.Maybe Prelude.Text,
    -- | Maximum observed or expected memory utilization of the instance.
    maxMemoryUtilizationPercentage :: Prelude.Maybe Prelude.Text,
    -- | The EBS field that contains a list of EBS metrics associated with the
    -- current instance.
    eBSResourceUtilization :: Prelude.Maybe EBSResourceUtilization,
    -- | Maximum observed or expected CPU utilization of the instance.
    maxCpuUtilizationPercentage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      maxMemoryUtilizationPercentage = Prelude.Nothing,
      eBSResourceUtilization = Prelude.Nothing,
      maxCpuUtilizationPercentage = Prelude.Nothing
    }

-- | Maximum observed or expected storage utilization of the instance (does
-- not measure EBS storage).
eC2ResourceUtilization_maxStorageUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Prelude.Maybe Prelude.Text)
eC2ResourceUtilization_maxStorageUtilizationPercentage = Lens.lens (\EC2ResourceUtilization' {maxStorageUtilizationPercentage} -> maxStorageUtilizationPercentage) (\s@EC2ResourceUtilization' {} a -> s {maxStorageUtilizationPercentage = a} :: EC2ResourceUtilization)

-- | Maximum observed or expected memory utilization of the instance.
eC2ResourceUtilization_maxMemoryUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Prelude.Maybe Prelude.Text)
eC2ResourceUtilization_maxMemoryUtilizationPercentage = Lens.lens (\EC2ResourceUtilization' {maxMemoryUtilizationPercentage} -> maxMemoryUtilizationPercentage) (\s@EC2ResourceUtilization' {} a -> s {maxMemoryUtilizationPercentage = a} :: EC2ResourceUtilization)

-- | The EBS field that contains a list of EBS metrics associated with the
-- current instance.
eC2ResourceUtilization_eBSResourceUtilization :: Lens.Lens' EC2ResourceUtilization (Prelude.Maybe EBSResourceUtilization)
eC2ResourceUtilization_eBSResourceUtilization = Lens.lens (\EC2ResourceUtilization' {eBSResourceUtilization} -> eBSResourceUtilization) (\s@EC2ResourceUtilization' {} a -> s {eBSResourceUtilization = a} :: EC2ResourceUtilization)

-- | Maximum observed or expected CPU utilization of the instance.
eC2ResourceUtilization_maxCpuUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Prelude.Maybe Prelude.Text)
eC2ResourceUtilization_maxCpuUtilizationPercentage = Lens.lens (\EC2ResourceUtilization' {maxCpuUtilizationPercentage} -> maxCpuUtilizationPercentage) (\s@EC2ResourceUtilization' {} a -> s {maxCpuUtilizationPercentage = a} :: EC2ResourceUtilization)

instance Prelude.FromJSON EC2ResourceUtilization where
  parseJSON =
    Prelude.withObject
      "EC2ResourceUtilization"
      ( \x ->
          EC2ResourceUtilization'
            Prelude.<$> (x Prelude..:? "MaxStorageUtilizationPercentage")
            Prelude.<*> (x Prelude..:? "MaxMemoryUtilizationPercentage")
            Prelude.<*> (x Prelude..:? "EBSResourceUtilization")
            Prelude.<*> (x Prelude..:? "MaxCpuUtilizationPercentage")
      )

instance Prelude.Hashable EC2ResourceUtilization

instance Prelude.NFData EC2ResourceUtilization
