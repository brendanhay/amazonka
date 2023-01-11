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
-- Module      : Amazonka.EMR.Types.EbsBlockDeviceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.EbsBlockDeviceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.VolumeSpecification
import qualified Amazonka.Prelude as Prelude

-- | Configuration of requested EBS block device associated with the instance
-- group with count of volumes that are associated to every instance.
--
-- /See:/ 'newEbsBlockDeviceConfig' smart constructor.
data EbsBlockDeviceConfig = EbsBlockDeviceConfig'
  { -- | Number of EBS volumes with a specific volume configuration that are
    -- associated with every instance in the instance group
    volumesPerInstance :: Prelude.Maybe Prelude.Int,
    -- | EBS volume specifications such as volume type, IOPS, size (GiB) and
    -- throughput (MiB\/s) that are requested for the EBS volume attached to an
    -- EC2 instance in the cluster.
    volumeSpecification :: VolumeSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EbsBlockDeviceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumesPerInstance', 'ebsBlockDeviceConfig_volumesPerInstance' - Number of EBS volumes with a specific volume configuration that are
-- associated with every instance in the instance group
--
-- 'volumeSpecification', 'ebsBlockDeviceConfig_volumeSpecification' - EBS volume specifications such as volume type, IOPS, size (GiB) and
-- throughput (MiB\/s) that are requested for the EBS volume attached to an
-- EC2 instance in the cluster.
newEbsBlockDeviceConfig ::
  -- | 'volumeSpecification'
  VolumeSpecification ->
  EbsBlockDeviceConfig
newEbsBlockDeviceConfig pVolumeSpecification_ =
  EbsBlockDeviceConfig'
    { volumesPerInstance =
        Prelude.Nothing,
      volumeSpecification = pVolumeSpecification_
    }

-- | Number of EBS volumes with a specific volume configuration that are
-- associated with every instance in the instance group
ebsBlockDeviceConfig_volumesPerInstance :: Lens.Lens' EbsBlockDeviceConfig (Prelude.Maybe Prelude.Int)
ebsBlockDeviceConfig_volumesPerInstance = Lens.lens (\EbsBlockDeviceConfig' {volumesPerInstance} -> volumesPerInstance) (\s@EbsBlockDeviceConfig' {} a -> s {volumesPerInstance = a} :: EbsBlockDeviceConfig)

-- | EBS volume specifications such as volume type, IOPS, size (GiB) and
-- throughput (MiB\/s) that are requested for the EBS volume attached to an
-- EC2 instance in the cluster.
ebsBlockDeviceConfig_volumeSpecification :: Lens.Lens' EbsBlockDeviceConfig VolumeSpecification
ebsBlockDeviceConfig_volumeSpecification = Lens.lens (\EbsBlockDeviceConfig' {volumeSpecification} -> volumeSpecification) (\s@EbsBlockDeviceConfig' {} a -> s {volumeSpecification = a} :: EbsBlockDeviceConfig)

instance Prelude.Hashable EbsBlockDeviceConfig where
  hashWithSalt _salt EbsBlockDeviceConfig' {..} =
    _salt `Prelude.hashWithSalt` volumesPerInstance
      `Prelude.hashWithSalt` volumeSpecification

instance Prelude.NFData EbsBlockDeviceConfig where
  rnf EbsBlockDeviceConfig' {..} =
    Prelude.rnf volumesPerInstance
      `Prelude.seq` Prelude.rnf volumeSpecification

instance Data.ToJSON EbsBlockDeviceConfig where
  toJSON EbsBlockDeviceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VolumesPerInstance" Data..=)
              Prelude.<$> volumesPerInstance,
            Prelude.Just
              ("VolumeSpecification" Data..= volumeSpecification)
          ]
      )
