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
-- Module      : Network.AWS.EMR.Types.EbsBlockDeviceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EbsBlockDeviceConfig where

import Network.AWS.EMR.Types.VolumeSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration of requested EBS block device associated with the instance
-- group with count of volumes that will be associated to every instance.
--
-- /See:/ 'newEbsBlockDeviceConfig' smart constructor.
data EbsBlockDeviceConfig = EbsBlockDeviceConfig'
  { -- | Number of EBS volumes with a specific volume configuration that will be
    -- associated with every instance in the instance group
    volumesPerInstance :: Prelude.Maybe Prelude.Int,
    -- | EBS volume specifications such as volume type, IOPS, and size (GiB) that
    -- will be requested for the EBS volume attached to an EC2 instance in the
    -- cluster.
    volumeSpecification :: VolumeSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EbsBlockDeviceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumesPerInstance', 'ebsBlockDeviceConfig_volumesPerInstance' - Number of EBS volumes with a specific volume configuration that will be
-- associated with every instance in the instance group
--
-- 'volumeSpecification', 'ebsBlockDeviceConfig_volumeSpecification' - EBS volume specifications such as volume type, IOPS, and size (GiB) that
-- will be requested for the EBS volume attached to an EC2 instance in the
-- cluster.
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

-- | Number of EBS volumes with a specific volume configuration that will be
-- associated with every instance in the instance group
ebsBlockDeviceConfig_volumesPerInstance :: Lens.Lens' EbsBlockDeviceConfig (Prelude.Maybe Prelude.Int)
ebsBlockDeviceConfig_volumesPerInstance = Lens.lens (\EbsBlockDeviceConfig' {volumesPerInstance} -> volumesPerInstance) (\s@EbsBlockDeviceConfig' {} a -> s {volumesPerInstance = a} :: EbsBlockDeviceConfig)

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that
-- will be requested for the EBS volume attached to an EC2 instance in the
-- cluster.
ebsBlockDeviceConfig_volumeSpecification :: Lens.Lens' EbsBlockDeviceConfig VolumeSpecification
ebsBlockDeviceConfig_volumeSpecification = Lens.lens (\EbsBlockDeviceConfig' {volumeSpecification} -> volumeSpecification) (\s@EbsBlockDeviceConfig' {} a -> s {volumeSpecification = a} :: EbsBlockDeviceConfig)

instance Prelude.Hashable EbsBlockDeviceConfig

instance Prelude.NFData EbsBlockDeviceConfig

instance Prelude.ToJSON EbsBlockDeviceConfig where
  toJSON EbsBlockDeviceConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("VolumesPerInstance" Prelude..=)
              Prelude.<$> volumesPerInstance,
            Prelude.Just
              ( "VolumeSpecification"
                  Prelude..= volumeSpecification
              )
          ]
      )
