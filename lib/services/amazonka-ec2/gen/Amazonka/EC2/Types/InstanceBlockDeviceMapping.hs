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
-- Module      : Amazonka.EC2.Types.InstanceBlockDeviceMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceBlockDeviceMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.EbsInstanceBlockDevice
import qualified Amazonka.Prelude as Prelude

-- | Describes a block device mapping.
--
-- /See:/ 'newInstanceBlockDeviceMapping' smart constructor.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping'
  { -- | The device name (for example, @\/dev\/sdh@ or @xvdh@).
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | Parameters used to automatically set up EBS volumes when the instance is
    -- launched.
    ebs :: Prelude.Maybe EbsInstanceBlockDevice
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
-- 'deviceName', 'instanceBlockDeviceMapping_deviceName' - The device name (for example, @\/dev\/sdh@ or @xvdh@).
--
-- 'ebs', 'instanceBlockDeviceMapping_ebs' - Parameters used to automatically set up EBS volumes when the instance is
-- launched.
newInstanceBlockDeviceMapping ::
  InstanceBlockDeviceMapping
newInstanceBlockDeviceMapping =
  InstanceBlockDeviceMapping'
    { deviceName =
        Prelude.Nothing,
      ebs = Prelude.Nothing
    }

-- | The device name (for example, @\/dev\/sdh@ or @xvdh@).
instanceBlockDeviceMapping_deviceName :: Lens.Lens' InstanceBlockDeviceMapping (Prelude.Maybe Prelude.Text)
instanceBlockDeviceMapping_deviceName = Lens.lens (\InstanceBlockDeviceMapping' {deviceName} -> deviceName) (\s@InstanceBlockDeviceMapping' {} a -> s {deviceName = a} :: InstanceBlockDeviceMapping)

-- | Parameters used to automatically set up EBS volumes when the instance is
-- launched.
instanceBlockDeviceMapping_ebs :: Lens.Lens' InstanceBlockDeviceMapping (Prelude.Maybe EbsInstanceBlockDevice)
instanceBlockDeviceMapping_ebs = Lens.lens (\InstanceBlockDeviceMapping' {ebs} -> ebs) (\s@InstanceBlockDeviceMapping' {} a -> s {ebs = a} :: InstanceBlockDeviceMapping)

instance Data.FromXML InstanceBlockDeviceMapping where
  parseXML x =
    InstanceBlockDeviceMapping'
      Prelude.<$> (x Data..@? "deviceName")
      Prelude.<*> (x Data..@? "ebs")

instance Prelude.Hashable InstanceBlockDeviceMapping where
  hashWithSalt _salt InstanceBlockDeviceMapping' {..} =
    _salt
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` ebs

instance Prelude.NFData InstanceBlockDeviceMapping where
  rnf InstanceBlockDeviceMapping' {..} =
    Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf ebs
