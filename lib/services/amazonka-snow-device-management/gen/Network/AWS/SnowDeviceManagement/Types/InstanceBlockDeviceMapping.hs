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
-- Module      : Network.AWS.SnowDeviceManagement.Types.InstanceBlockDeviceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SnowDeviceManagement.Types.InstanceBlockDeviceMapping where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SnowDeviceManagement.Types.EbsInstanceBlockDevice

-- | The description of a block device mapping.
--
-- /See:/ 'newInstanceBlockDeviceMapping' smart constructor.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping'
  { -- | The parameters used to automatically set up Amazon Elastic Block Store
    -- (Amazon EBS) volumes when the instance is launched.
    ebs :: Prelude.Maybe EbsInstanceBlockDevice,
    -- | The block device name.
    deviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceBlockDeviceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebs', 'instanceBlockDeviceMapping_ebs' - The parameters used to automatically set up Amazon Elastic Block Store
-- (Amazon EBS) volumes when the instance is launched.
--
-- 'deviceName', 'instanceBlockDeviceMapping_deviceName' - The block device name.
newInstanceBlockDeviceMapping ::
  InstanceBlockDeviceMapping
newInstanceBlockDeviceMapping =
  InstanceBlockDeviceMapping'
    { ebs = Prelude.Nothing,
      deviceName = Prelude.Nothing
    }

-- | The parameters used to automatically set up Amazon Elastic Block Store
-- (Amazon EBS) volumes when the instance is launched.
instanceBlockDeviceMapping_ebs :: Lens.Lens' InstanceBlockDeviceMapping (Prelude.Maybe EbsInstanceBlockDevice)
instanceBlockDeviceMapping_ebs = Lens.lens (\InstanceBlockDeviceMapping' {ebs} -> ebs) (\s@InstanceBlockDeviceMapping' {} a -> s {ebs = a} :: InstanceBlockDeviceMapping)

-- | The block device name.
instanceBlockDeviceMapping_deviceName :: Lens.Lens' InstanceBlockDeviceMapping (Prelude.Maybe Prelude.Text)
instanceBlockDeviceMapping_deviceName = Lens.lens (\InstanceBlockDeviceMapping' {deviceName} -> deviceName) (\s@InstanceBlockDeviceMapping' {} a -> s {deviceName = a} :: InstanceBlockDeviceMapping)

instance Core.FromJSON InstanceBlockDeviceMapping where
  parseJSON =
    Core.withObject
      "InstanceBlockDeviceMapping"
      ( \x ->
          InstanceBlockDeviceMapping'
            Prelude.<$> (x Core..:? "ebs")
            Prelude.<*> (x Core..:? "deviceName")
      )

instance Prelude.Hashable InstanceBlockDeviceMapping

instance Prelude.NFData InstanceBlockDeviceMapping
