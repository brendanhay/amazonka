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
-- Module      : Amazonka.IAM.EnableMFADevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified MFA device and associates it with the specified
-- IAM user. When enabled, the MFA device is required for every subsequent
-- login by the IAM user associated with the device.
module Amazonka.IAM.EnableMFADevice
  ( -- * Creating a Request
    EnableMFADevice (..),
    newEnableMFADevice,

    -- * Request Lenses
    enableMFADevice_userName,
    enableMFADevice_serialNumber,
    enableMFADevice_authenticationCode1,
    enableMFADevice_authenticationCode2,

    -- * Destructuring the Response
    EnableMFADeviceResponse (..),
    newEnableMFADeviceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableMFADevice' smart constructor.
data EnableMFADevice = EnableMFADevice'
  { -- | The name of the IAM user for whom you want to enable the MFA device.
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
    serialNumber :: Prelude.Text,
    -- | An authentication code emitted by the device.
    --
    -- The format for this parameter is a string of six digits.
    --
    -- Submit your request immediately after generating the authentication
    -- codes. If you generate the codes and then wait too long to submit the
    -- request, the MFA device successfully associates with the user but the
    -- MFA device becomes out of sync. This happens because time-based one-time
    -- passwords (TOTP) expire after a short period of time. If this happens,
    -- you can
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device>.
    authenticationCode1 :: Prelude.Text,
    -- | A subsequent authentication code emitted by the device.
    --
    -- The format for this parameter is a string of six digits.
    --
    -- Submit your request immediately after generating the authentication
    -- codes. If you generate the codes and then wait too long to submit the
    -- request, the MFA device successfully associates with the user but the
    -- MFA device becomes out of sync. This happens because time-based one-time
    -- passwords (TOTP) expire after a short period of time. If this happens,
    -- you can
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device>.
    authenticationCode2 :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableMFADevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'enableMFADevice_userName' - The name of the IAM user for whom you want to enable the MFA device.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'serialNumber', 'enableMFADevice_serialNumber' - The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@:\/-
--
-- 'authenticationCode1', 'enableMFADevice_authenticationCode1' - An authentication code emitted by the device.
--
-- The format for this parameter is a string of six digits.
--
-- Submit your request immediately after generating the authentication
-- codes. If you generate the codes and then wait too long to submit the
-- request, the MFA device successfully associates with the user but the
-- MFA device becomes out of sync. This happens because time-based one-time
-- passwords (TOTP) expire after a short period of time. If this happens,
-- you can
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device>.
--
-- 'authenticationCode2', 'enableMFADevice_authenticationCode2' - A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a string of six digits.
--
-- Submit your request immediately after generating the authentication
-- codes. If you generate the codes and then wait too long to submit the
-- request, the MFA device successfully associates with the user but the
-- MFA device becomes out of sync. This happens because time-based one-time
-- passwords (TOTP) expire after a short period of time. If this happens,
-- you can
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device>.
newEnableMFADevice ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'serialNumber'
  Prelude.Text ->
  -- | 'authenticationCode1'
  Prelude.Text ->
  -- | 'authenticationCode2'
  Prelude.Text ->
  EnableMFADevice
newEnableMFADevice
  pUserName_
  pSerialNumber_
  pAuthenticationCode1_
  pAuthenticationCode2_ =
    EnableMFADevice'
      { userName = pUserName_,
        serialNumber = pSerialNumber_,
        authenticationCode1 = pAuthenticationCode1_,
        authenticationCode2 = pAuthenticationCode2_
      }

-- | The name of the IAM user for whom you want to enable the MFA device.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
enableMFADevice_userName :: Lens.Lens' EnableMFADevice Prelude.Text
enableMFADevice_userName = Lens.lens (\EnableMFADevice' {userName} -> userName) (\s@EnableMFADevice' {} a -> s {userName = a} :: EnableMFADevice)

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@:\/-
enableMFADevice_serialNumber :: Lens.Lens' EnableMFADevice Prelude.Text
enableMFADevice_serialNumber = Lens.lens (\EnableMFADevice' {serialNumber} -> serialNumber) (\s@EnableMFADevice' {} a -> s {serialNumber = a} :: EnableMFADevice)

-- | An authentication code emitted by the device.
--
-- The format for this parameter is a string of six digits.
--
-- Submit your request immediately after generating the authentication
-- codes. If you generate the codes and then wait too long to submit the
-- request, the MFA device successfully associates with the user but the
-- MFA device becomes out of sync. This happens because time-based one-time
-- passwords (TOTP) expire after a short period of time. If this happens,
-- you can
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device>.
enableMFADevice_authenticationCode1 :: Lens.Lens' EnableMFADevice Prelude.Text
enableMFADevice_authenticationCode1 = Lens.lens (\EnableMFADevice' {authenticationCode1} -> authenticationCode1) (\s@EnableMFADevice' {} a -> s {authenticationCode1 = a} :: EnableMFADevice)

-- | A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a string of six digits.
--
-- Submit your request immediately after generating the authentication
-- codes. If you generate the codes and then wait too long to submit the
-- request, the MFA device successfully associates with the user but the
-- MFA device becomes out of sync. This happens because time-based one-time
-- passwords (TOTP) expire after a short period of time. If this happens,
-- you can
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device>.
enableMFADevice_authenticationCode2 :: Lens.Lens' EnableMFADevice Prelude.Text
enableMFADevice_authenticationCode2 = Lens.lens (\EnableMFADevice' {authenticationCode2} -> authenticationCode2) (\s@EnableMFADevice' {} a -> s {authenticationCode2 = a} :: EnableMFADevice)

instance Core.AWSRequest EnableMFADevice where
  type
    AWSResponse EnableMFADevice =
      EnableMFADeviceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull EnableMFADeviceResponse'

instance Prelude.Hashable EnableMFADevice where
  hashWithSalt _salt EnableMFADevice' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` authenticationCode1
      `Prelude.hashWithSalt` authenticationCode2

instance Prelude.NFData EnableMFADevice where
  rnf EnableMFADevice' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf authenticationCode1
      `Prelude.seq` Prelude.rnf authenticationCode2

instance Data.ToHeaders EnableMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableMFADevice where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableMFADevice where
  toQuery EnableMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableMFADevice" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName,
        "SerialNumber" Data.=: serialNumber,
        "AuthenticationCode1" Data.=: authenticationCode1,
        "AuthenticationCode2" Data.=: authenticationCode2
      ]

-- | /See:/ 'newEnableMFADeviceResponse' smart constructor.
data EnableMFADeviceResponse = EnableMFADeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableMFADeviceResponse ::
  EnableMFADeviceResponse
newEnableMFADeviceResponse = EnableMFADeviceResponse'

instance Prelude.NFData EnableMFADeviceResponse where
  rnf _ = ()
