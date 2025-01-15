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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails

-- | Information about a block device mapping for an Amazon Elastic Compute
-- Cloud (Amazon EC2) launch template.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' smart constructor.
data AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails = AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails'
  { -- | The device name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | Parameters used to automatically set up Amazon EBS volumes when the
    -- instance is launched.
    ebs :: Prelude.Maybe AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails,
    -- | Omits the device from the block device mapping when an empty string is
    -- specified.
    noDevice :: Prelude.Maybe Prelude.Text,
    -- | The virtual device name (ephemeralN). Instance store volumes are
    -- numbered starting from 0. An instance type with 2 available instance
    -- store volumes can specify mappings for @ephemeral0@ and @ephemeral1@.
    -- The number of available instance store volumes depends on the instance
    -- type.
    virtualName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceName', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_deviceName' - The device name.
--
-- 'ebs', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_ebs' - Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
--
-- 'noDevice', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_noDevice' - Omits the device from the block device mapping when an empty string is
-- specified.
--
-- 'virtualName', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_virtualName' - The virtual device name (ephemeralN). Instance store volumes are
-- numbered starting from 0. An instance type with 2 available instance
-- store volumes can specify mappings for @ephemeral0@ and @ephemeral1@.
-- The number of available instance store volumes depends on the instance
-- type.
newAwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails ::
  AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
newAwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails =
  AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails'
    { deviceName =
        Prelude.Nothing,
      ebs = Prelude.Nothing,
      noDevice =
        Prelude.Nothing,
      virtualName =
        Prelude.Nothing
    }

-- | The device name.
awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_deviceName :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_deviceName = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {deviceName} -> deviceName) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {} a -> s {deviceName = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails)

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_ebs :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails (Prelude.Maybe AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails)
awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_ebs = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {ebs} -> ebs) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {} a -> s {ebs = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails)

-- | Omits the device from the block device mapping when an empty string is
-- specified.
awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_noDevice :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_noDevice = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {noDevice} -> noDevice) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {} a -> s {noDevice = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails)

-- | The virtual device name (ephemeralN). Instance store volumes are
-- numbered starting from 0. An instance type with 2 available instance
-- store volumes can specify mappings for @ephemeral0@ and @ephemeral1@.
-- The number of available instance store volumes depends on the instance
-- type.
awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_virtualName :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_virtualName = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {virtualName} -> virtualName) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {} a -> s {virtualName = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails'
            Prelude.<$> (x Data..:? "DeviceName")
            Prelude.<*> (x Data..:? "Ebs")
            Prelude.<*> (x Data..:? "NoDevice")
            Prelude.<*> (x Data..:? "VirtualName")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {..} =
      _salt
        `Prelude.hashWithSalt` deviceName
        `Prelude.hashWithSalt` ebs
        `Prelude.hashWithSalt` noDevice
        `Prelude.hashWithSalt` virtualName

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
  where
  rnf
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {..} =
      Prelude.rnf deviceName `Prelude.seq`
        Prelude.rnf ebs `Prelude.seq`
          Prelude.rnf noDevice `Prelude.seq`
            Prelude.rnf virtualName

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DeviceName" Data..=) Prelude.<$> deviceName,
              ("Ebs" Data..=) Prelude.<$> ebs,
              ("NoDevice" Data..=) Prelude.<$> noDevice,
              ("VirtualName" Data..=) Prelude.<$> virtualName
            ]
        )
