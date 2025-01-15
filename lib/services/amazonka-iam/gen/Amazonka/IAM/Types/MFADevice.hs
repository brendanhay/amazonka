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
-- Module      : Amazonka.IAM.Types.MFADevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.MFADevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an MFA device.
--
-- This data type is used as a response element in the ListMFADevices
-- operation.
--
-- /See:/ 'newMFADevice' smart constructor.
data MFADevice = MFADevice'
  { -- | The user with whom the MFA device is associated.
    userName :: Prelude.Text,
    -- | The serial number that uniquely identifies the MFA device. For virtual
    -- MFA devices, the serial number is the device ARN.
    serialNumber :: Prelude.Text,
    -- | The date when the MFA device was enabled for the user.
    enableDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MFADevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'mfaDevice_userName' - The user with whom the MFA device is associated.
--
-- 'serialNumber', 'mfaDevice_serialNumber' - The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
--
-- 'enableDate', 'mfaDevice_enableDate' - The date when the MFA device was enabled for the user.
newMFADevice ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'serialNumber'
  Prelude.Text ->
  -- | 'enableDate'
  Prelude.UTCTime ->
  MFADevice
newMFADevice pUserName_ pSerialNumber_ pEnableDate_ =
  MFADevice'
    { userName = pUserName_,
      serialNumber = pSerialNumber_,
      enableDate = Data._Time Lens.# pEnableDate_
    }

-- | The user with whom the MFA device is associated.
mfaDevice_userName :: Lens.Lens' MFADevice Prelude.Text
mfaDevice_userName = Lens.lens (\MFADevice' {userName} -> userName) (\s@MFADevice' {} a -> s {userName = a} :: MFADevice)

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
mfaDevice_serialNumber :: Lens.Lens' MFADevice Prelude.Text
mfaDevice_serialNumber = Lens.lens (\MFADevice' {serialNumber} -> serialNumber) (\s@MFADevice' {} a -> s {serialNumber = a} :: MFADevice)

-- | The date when the MFA device was enabled for the user.
mfaDevice_enableDate :: Lens.Lens' MFADevice Prelude.UTCTime
mfaDevice_enableDate = Lens.lens (\MFADevice' {enableDate} -> enableDate) (\s@MFADevice' {} a -> s {enableDate = a} :: MFADevice) Prelude.. Data._Time

instance Data.FromXML MFADevice where
  parseXML x =
    MFADevice'
      Prelude.<$> (x Data..@ "UserName")
      Prelude.<*> (x Data..@ "SerialNumber")
      Prelude.<*> (x Data..@ "EnableDate")

instance Prelude.Hashable MFADevice where
  hashWithSalt _salt MFADevice' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` enableDate

instance Prelude.NFData MFADevice where
  rnf MFADevice' {..} =
    Prelude.rnf userName `Prelude.seq`
      Prelude.rnf serialNumber `Prelude.seq`
        Prelude.rnf enableDate
