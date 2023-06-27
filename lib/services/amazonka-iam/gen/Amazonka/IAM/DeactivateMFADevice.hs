{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IAM.DeactivateMFADevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified MFA device and removes it from association
-- with the user name for which it was originally enabled.
--
-- For more information about creating and working with virtual MFA
-- devices, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Enabling a virtual multi-factor authentication (MFA) device>
-- in the /IAM User Guide/.
module Amazonka.IAM.DeactivateMFADevice
  ( -- * Creating a Request
    DeactivateMFADevice (..),
    newDeactivateMFADevice,

    -- * Request Lenses
    deactivateMFADevice_userName,
    deactivateMFADevice_serialNumber,

    -- * Destructuring the Response
    DeactivateMFADeviceResponse (..),
    newDeactivateMFADeviceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeactivateMFADevice' smart constructor.
data DeactivateMFADevice = DeactivateMFADevice'
  { -- | The name of the user whose MFA device you want to deactivate.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
    -- | The serial number that uniquely identifies the MFA device. For virtual
    -- MFA devices, the serial number is the device ARN.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@:\/-
    serialNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateMFADevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deactivateMFADevice_userName' - The name of the user whose MFA device you want to deactivate.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'serialNumber', 'deactivateMFADevice_serialNumber' - The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@:\/-
newDeactivateMFADevice ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'serialNumber'
  Prelude.Text ->
  DeactivateMFADevice
newDeactivateMFADevice pUserName_ pSerialNumber_ =
  DeactivateMFADevice'
    { userName = pUserName_,
      serialNumber = pSerialNumber_
    }

-- | The name of the user whose MFA device you want to deactivate.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deactivateMFADevice_userName :: Lens.Lens' DeactivateMFADevice Prelude.Text
deactivateMFADevice_userName = Lens.lens (\DeactivateMFADevice' {userName} -> userName) (\s@DeactivateMFADevice' {} a -> s {userName = a} :: DeactivateMFADevice)

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@:\/-
deactivateMFADevice_serialNumber :: Lens.Lens' DeactivateMFADevice Prelude.Text
deactivateMFADevice_serialNumber = Lens.lens (\DeactivateMFADevice' {serialNumber} -> serialNumber) (\s@DeactivateMFADevice' {} a -> s {serialNumber = a} :: DeactivateMFADevice)

instance Core.AWSRequest DeactivateMFADevice where
  type
    AWSResponse DeactivateMFADevice =
      DeactivateMFADeviceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeactivateMFADeviceResponse'

instance Prelude.Hashable DeactivateMFADevice where
  hashWithSalt _salt DeactivateMFADevice' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` serialNumber

instance Prelude.NFData DeactivateMFADevice where
  rnf DeactivateMFADevice' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf serialNumber

instance Data.ToHeaders DeactivateMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeactivateMFADevice where
  toPath = Prelude.const "/"

instance Data.ToQuery DeactivateMFADevice where
  toQuery DeactivateMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeactivateMFADevice" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName,
        "SerialNumber" Data.=: serialNumber
      ]

-- | /See:/ 'newDeactivateMFADeviceResponse' smart constructor.
data DeactivateMFADeviceResponse = DeactivateMFADeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeactivateMFADeviceResponse ::
  DeactivateMFADeviceResponse
newDeactivateMFADeviceResponse =
  DeactivateMFADeviceResponse'

instance Prelude.NFData DeactivateMFADeviceResponse where
  rnf _ = ()
