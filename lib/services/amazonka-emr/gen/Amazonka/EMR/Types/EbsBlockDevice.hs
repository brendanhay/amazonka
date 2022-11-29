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
-- Module      : Amazonka.EMR.Types.EbsBlockDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.EbsBlockDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types.VolumeSpecification
import qualified Amazonka.Prelude as Prelude

-- | Configuration of requested EBS block device associated with the instance
-- group.
--
-- /See:/ 'newEbsBlockDevice' smart constructor.
data EbsBlockDevice = EbsBlockDevice'
  { -- | The device name that is exposed to the instance, such as \/dev\/sdh.
    device :: Prelude.Maybe Prelude.Text,
    -- | EBS volume specifications such as volume type, IOPS, size (GiB) and
    -- throughput (MiB\/s) that are requested for the EBS volume attached to an
    -- EC2 instance in the cluster.
    volumeSpecification :: Prelude.Maybe VolumeSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'volumeSpecification', 'ebsBlockDevice_volumeSpecification' - EBS volume specifications such as volume type, IOPS, size (GiB) and
-- throughput (MiB\/s) that are requested for the EBS volume attached to an
-- EC2 instance in the cluster.
newEbsBlockDevice ::
  EbsBlockDevice
newEbsBlockDevice =
  EbsBlockDevice'
    { device = Prelude.Nothing,
      volumeSpecification = Prelude.Nothing
    }

-- | The device name that is exposed to the instance, such as \/dev\/sdh.
ebsBlockDevice_device :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Text)
ebsBlockDevice_device = Lens.lens (\EbsBlockDevice' {device} -> device) (\s@EbsBlockDevice' {} a -> s {device = a} :: EbsBlockDevice)

-- | EBS volume specifications such as volume type, IOPS, size (GiB) and
-- throughput (MiB\/s) that are requested for the EBS volume attached to an
-- EC2 instance in the cluster.
ebsBlockDevice_volumeSpecification :: Lens.Lens' EbsBlockDevice (Prelude.Maybe VolumeSpecification)
ebsBlockDevice_volumeSpecification = Lens.lens (\EbsBlockDevice' {volumeSpecification} -> volumeSpecification) (\s@EbsBlockDevice' {} a -> s {volumeSpecification = a} :: EbsBlockDevice)

instance Core.FromJSON EbsBlockDevice where
  parseJSON =
    Core.withObject
      "EbsBlockDevice"
      ( \x ->
          EbsBlockDevice'
            Prelude.<$> (x Core..:? "Device")
            Prelude.<*> (x Core..:? "VolumeSpecification")
      )

instance Prelude.Hashable EbsBlockDevice where
  hashWithSalt _salt EbsBlockDevice' {..} =
    _salt `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` volumeSpecification

instance Prelude.NFData EbsBlockDevice where
  rnf EbsBlockDevice' {..} =
    Prelude.rnf device
      `Prelude.seq` Prelude.rnf volumeSpecification
