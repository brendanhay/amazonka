{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IAM.ResyncMFADevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Synchronizes the specified MFA device with its IAM resource object on
-- the AWS servers.
--
-- For more information about creating and working with virtual MFA
-- devices, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a virtual MFA device>
-- in the /IAM User Guide/.
module Network.AWS.IAM.ResyncMFADevice
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest ResyncMFADevice where
  type Rs ResyncMFADevice = ResyncMFADeviceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull ResyncMFADeviceResponse'

instance Prelude.Hashable ResyncMFADevice

instance Prelude.NFData ResyncMFADevice

instance Prelude.ToHeaders ResyncMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ResyncMFADevice where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResyncMFADevice where
  toQuery ResyncMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ResyncMFADevice" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "SerialNumber" Prelude.=: serialNumber,
        "AuthenticationCode1" Prelude.=: authenticationCode1,
        "AuthenticationCode2" Prelude.=: authenticationCode2
      ]

-- | /See:/ 'newResyncMFADeviceResponse' smart constructor.
data ResyncMFADeviceResponse = ResyncMFADeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResyncMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResyncMFADeviceResponse ::
  ResyncMFADeviceResponse
newResyncMFADeviceResponse = ResyncMFADeviceResponse'

instance Prelude.NFData ResyncMFADeviceResponse
