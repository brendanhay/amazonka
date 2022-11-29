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
-- Module      : Amazonka.IAM.ResyncMFADevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Synchronizes the specified MFA device with its IAM resource object on
-- the Amazon Web Services servers.
--
-- For more information about creating and working with virtual MFA
-- devices, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a virtual MFA device>
-- in the /IAM User Guide/.
module Amazonka.IAM.ResyncMFADevice
  ( -- * Creating a Request
    ResyncMFADevice (..),
    newResyncMFADevice,

    -- * Request Lenses
    resyncMFADevice_userName,
    resyncMFADevice_serialNumber,
    resyncMFADevice_authenticationCode1,
    resyncMFADevice_authenticationCode2,

    -- * Destructuring the Response
    ResyncMFADeviceResponse (..),
    newResyncMFADeviceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResyncMFADevice' smart constructor.
data ResyncMFADevice = ResyncMFADevice'
  { -- | The name of the user whose MFA device you want to resynchronize.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
    -- | Serial number that uniquely identifies the MFA device.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    serialNumber :: Prelude.Text,
    -- | An authentication code emitted by the device.
    --
    -- The format for this parameter is a sequence of six digits.
    authenticationCode1 :: Prelude.Text,
    -- | A subsequent authentication code emitted by the device.
    --
    -- The format for this parameter is a sequence of six digits.
    authenticationCode2 :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResyncMFADevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'resyncMFADevice_userName' - The name of the user whose MFA device you want to resynchronize.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'serialNumber', 'resyncMFADevice_serialNumber' - Serial number that uniquely identifies the MFA device.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'authenticationCode1', 'resyncMFADevice_authenticationCode1' - An authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
--
-- 'authenticationCode2', 'resyncMFADevice_authenticationCode2' - A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
newResyncMFADevice ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'serialNumber'
  Prelude.Text ->
  -- | 'authenticationCode1'
  Prelude.Text ->
  -- | 'authenticationCode2'
  Prelude.Text ->
  ResyncMFADevice
newResyncMFADevice
  pUserName_
  pSerialNumber_
  pAuthenticationCode1_
  pAuthenticationCode2_ =
    ResyncMFADevice'
      { userName = pUserName_,
        serialNumber = pSerialNumber_,
        authenticationCode1 = pAuthenticationCode1_,
        authenticationCode2 = pAuthenticationCode2_
      }

-- | The name of the user whose MFA device you want to resynchronize.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
resyncMFADevice_userName :: Lens.Lens' ResyncMFADevice Prelude.Text
resyncMFADevice_userName = Lens.lens (\ResyncMFADevice' {userName} -> userName) (\s@ResyncMFADevice' {} a -> s {userName = a} :: ResyncMFADevice)

-- | Serial number that uniquely identifies the MFA device.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
resyncMFADevice_serialNumber :: Lens.Lens' ResyncMFADevice Prelude.Text
resyncMFADevice_serialNumber = Lens.lens (\ResyncMFADevice' {serialNumber} -> serialNumber) (\s@ResyncMFADevice' {} a -> s {serialNumber = a} :: ResyncMFADevice)

-- | An authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
resyncMFADevice_authenticationCode1 :: Lens.Lens' ResyncMFADevice Prelude.Text
resyncMFADevice_authenticationCode1 = Lens.lens (\ResyncMFADevice' {authenticationCode1} -> authenticationCode1) (\s@ResyncMFADevice' {} a -> s {authenticationCode1 = a} :: ResyncMFADevice)

-- | A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
resyncMFADevice_authenticationCode2 :: Lens.Lens' ResyncMFADevice Prelude.Text
resyncMFADevice_authenticationCode2 = Lens.lens (\ResyncMFADevice' {authenticationCode2} -> authenticationCode2) (\s@ResyncMFADevice' {} a -> s {authenticationCode2 = a} :: ResyncMFADevice)

instance Core.AWSRequest ResyncMFADevice where
  type
    AWSResponse ResyncMFADevice =
      ResyncMFADeviceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ResyncMFADeviceResponse'

instance Prelude.Hashable ResyncMFADevice where
  hashWithSalt _salt ResyncMFADevice' {..} =
    _salt `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` authenticationCode1
      `Prelude.hashWithSalt` authenticationCode2

instance Prelude.NFData ResyncMFADevice where
  rnf ResyncMFADevice' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf authenticationCode1
      `Prelude.seq` Prelude.rnf authenticationCode2

instance Core.ToHeaders ResyncMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ResyncMFADevice where
  toPath = Prelude.const "/"

instance Core.ToQuery ResyncMFADevice where
  toQuery ResyncMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ResyncMFADevice" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Core.=: userName,
        "SerialNumber" Core.=: serialNumber,
        "AuthenticationCode1" Core.=: authenticationCode1,
        "AuthenticationCode2" Core.=: authenticationCode2
      ]

-- | /See:/ 'newResyncMFADeviceResponse' smart constructor.
data ResyncMFADeviceResponse = ResyncMFADeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResyncMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResyncMFADeviceResponse ::
  ResyncMFADeviceResponse
newResyncMFADeviceResponse = ResyncMFADeviceResponse'

instance Prelude.NFData ResyncMFADeviceResponse where
  rnf _ = ()
