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
-- Module      : Network.AWS.EC2.Types.InstanceBlockDeviceMappingSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceBlockDeviceMappingSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EbsInstanceBlockDeviceSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a block device mapping entry.
--
-- /See:/ 'newInstanceBlockDeviceMappingSpecification' smart constructor.
data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification'
  { -- | Parameters used to automatically set up EBS volumes when the instance is
    -- launched.
    ebs :: Prelude.Maybe EbsInstanceBlockDeviceSpecification,
    -- | suppress the specified device included in the block device mapping.
    noDevice :: Prelude.Maybe Prelude.Text,
    -- | The virtual device name.
    virtualName :: Prelude.Maybe Prelude.Text,
    -- | The device name (for example, @\/dev\/sdh@ or @xvdh@).
    deviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceBlockDeviceMappingSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebs', 'instanceBlockDeviceMappingSpecification_ebs' - Parameters used to automatically set up EBS volumes when the instance is
-- launched.
--
-- 'noDevice', 'instanceBlockDeviceMappingSpecification_noDevice' - suppress the specified device included in the block device mapping.
--
-- 'virtualName', 'instanceBlockDeviceMappingSpecification_virtualName' - The virtual device name.
--
-- 'deviceName', 'instanceBlockDeviceMappingSpecification_deviceName' - The device name (for example, @\/dev\/sdh@ or @xvdh@).
newInstanceBlockDeviceMappingSpecification ::
  InstanceBlockDeviceMappingSpecification
newInstanceBlockDeviceMappingSpecification =
  InstanceBlockDeviceMappingSpecification'
    { ebs =
        Prelude.Nothing,
      noDevice = Prelude.Nothing,
      virtualName = Prelude.Nothing,
      deviceName = Prelude.Nothing
    }

-- | Parameters used to automatically set up EBS volumes when the instance is
-- launched.
instanceBlockDeviceMappingSpecification_ebs :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Prelude.Maybe EbsInstanceBlockDeviceSpecification)
instanceBlockDeviceMappingSpecification_ebs = Lens.lens (\InstanceBlockDeviceMappingSpecification' {ebs} -> ebs) (\s@InstanceBlockDeviceMappingSpecification' {} a -> s {ebs = a} :: InstanceBlockDeviceMappingSpecification)

-- | suppress the specified device included in the block device mapping.
instanceBlockDeviceMappingSpecification_noDevice :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Prelude.Maybe Prelude.Text)
instanceBlockDeviceMappingSpecification_noDevice = Lens.lens (\InstanceBlockDeviceMappingSpecification' {noDevice} -> noDevice) (\s@InstanceBlockDeviceMappingSpecification' {} a -> s {noDevice = a} :: InstanceBlockDeviceMappingSpecification)

-- | The virtual device name.
instanceBlockDeviceMappingSpecification_virtualName :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Prelude.Maybe Prelude.Text)
instanceBlockDeviceMappingSpecification_virtualName = Lens.lens (\InstanceBlockDeviceMappingSpecification' {virtualName} -> virtualName) (\s@InstanceBlockDeviceMappingSpecification' {} a -> s {virtualName = a} :: InstanceBlockDeviceMappingSpecification)

-- | The device name (for example, @\/dev\/sdh@ or @xvdh@).
instanceBlockDeviceMappingSpecification_deviceName :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Prelude.Maybe Prelude.Text)
instanceBlockDeviceMappingSpecification_deviceName = Lens.lens (\InstanceBlockDeviceMappingSpecification' {deviceName} -> deviceName) (\s@InstanceBlockDeviceMappingSpecification' {} a -> s {deviceName = a} :: InstanceBlockDeviceMappingSpecification)

instance
  Prelude.Hashable
    InstanceBlockDeviceMappingSpecification

instance
  Prelude.NFData
    InstanceBlockDeviceMappingSpecification

instance
  Prelude.ToQuery
    InstanceBlockDeviceMappingSpecification
  where
  toQuery InstanceBlockDeviceMappingSpecification' {..} =
    Prelude.mconcat
      [ "Ebs" Prelude.=: ebs,
        "NoDevice" Prelude.=: noDevice,
        "VirtualName" Prelude.=: virtualName,
        "DeviceName" Prelude.=: deviceName
      ]
