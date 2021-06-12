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
-- Module      : Network.AWS.EMR.Types.EbsVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EbsVolume where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | EBS block device that\'s attached to an EC2 instance.
--
-- /See:/ 'newEbsVolume' smart constructor.
data EbsVolume = EbsVolume'
  { -- | The device name that is exposed to the instance, such as \/dev\/sdh.
    device :: Core.Maybe Core.Text,
    -- | The volume identifier of the EBS volume.
    volumeId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EbsVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'device', 'ebsVolume_device' - The device name that is exposed to the instance, such as \/dev\/sdh.
--
-- 'volumeId', 'ebsVolume_volumeId' - The volume identifier of the EBS volume.
newEbsVolume ::
  EbsVolume
newEbsVolume =
  EbsVolume'
    { device = Core.Nothing,
      volumeId = Core.Nothing
    }

-- | The device name that is exposed to the instance, such as \/dev\/sdh.
ebsVolume_device :: Lens.Lens' EbsVolume (Core.Maybe Core.Text)
ebsVolume_device = Lens.lens (\EbsVolume' {device} -> device) (\s@EbsVolume' {} a -> s {device = a} :: EbsVolume)

-- | The volume identifier of the EBS volume.
ebsVolume_volumeId :: Lens.Lens' EbsVolume (Core.Maybe Core.Text)
ebsVolume_volumeId = Lens.lens (\EbsVolume' {volumeId} -> volumeId) (\s@EbsVolume' {} a -> s {volumeId = a} :: EbsVolume)

instance Core.FromJSON EbsVolume where
  parseJSON =
    Core.withObject
      "EbsVolume"
      ( \x ->
          EbsVolume'
            Core.<$> (x Core..:? "Device")
            Core.<*> (x Core..:? "VolumeId")
      )

instance Core.Hashable EbsVolume

instance Core.NFData EbsVolume
