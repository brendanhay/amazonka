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
-- Module      : Network.AWS.EMR.Types.EbsBlockDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EbsBlockDevice where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.VolumeSpecification
import qualified Network.AWS.Lens as Lens

-- | Configuration of requested EBS block device associated with the instance
-- group.
--
-- /See:/ 'newEbsBlockDevice' smart constructor.
data EbsBlockDevice = EbsBlockDevice'
  { -- | The device name that is exposed to the instance, such as \/dev\/sdh.
    device :: Core.Maybe Core.Text,
    -- | EBS volume specifications such as volume type, IOPS, and size (GiB) that
    -- will be requested for the EBS volume attached to an EC2 instance in the
    -- cluster.
    volumeSpecification :: Core.Maybe VolumeSpecification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EbsBlockDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'device', 'ebsBlockDevice_device' - The device name that is exposed to the instance, such as \/dev\/sdh.
--
-- 'volumeSpecification', 'ebsBlockDevice_volumeSpecification' - EBS volume specifications such as volume type, IOPS, and size (GiB) that
-- will be requested for the EBS volume attached to an EC2 instance in the
-- cluster.
newEbsBlockDevice ::
  EbsBlockDevice
newEbsBlockDevice =
  EbsBlockDevice'
    { device = Core.Nothing,
      volumeSpecification = Core.Nothing
    }

-- | The device name that is exposed to the instance, such as \/dev\/sdh.
ebsBlockDevice_device :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Text)
ebsBlockDevice_device = Lens.lens (\EbsBlockDevice' {device} -> device) (\s@EbsBlockDevice' {} a -> s {device = a} :: EbsBlockDevice)

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that
-- will be requested for the EBS volume attached to an EC2 instance in the
-- cluster.
ebsBlockDevice_volumeSpecification :: Lens.Lens' EbsBlockDevice (Core.Maybe VolumeSpecification)
ebsBlockDevice_volumeSpecification = Lens.lens (\EbsBlockDevice' {volumeSpecification} -> volumeSpecification) (\s@EbsBlockDevice' {} a -> s {volumeSpecification = a} :: EbsBlockDevice)

instance Core.FromJSON EbsBlockDevice where
  parseJSON =
    Core.withObject
      "EbsBlockDevice"
      ( \x ->
          EbsBlockDevice'
            Core.<$> (x Core..:? "Device")
            Core.<*> (x Core..:? "VolumeSpecification")
      )

instance Core.Hashable EbsBlockDevice

instance Core.NFData EbsBlockDevice
