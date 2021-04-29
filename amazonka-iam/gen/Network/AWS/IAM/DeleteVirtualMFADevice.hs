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
-- Module      : Network.AWS.IAM.DeleteVirtualMFADevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a virtual MFA device.
--
-- You must deactivate a user\'s virtual MFA device before you can delete
-- it. For information about deactivating MFA devices, see
-- DeactivateMFADevice.
module Network.AWS.IAM.DeleteVirtualMFADevice
  ( -- * Creating a Request
    DeleteVirtualMFADevice (..),
    newDeleteVirtualMFADevice,

    -- * Request Lenses
    deleteVirtualMFADevice_serialNumber,

    -- * Destructuring the Response
    DeleteVirtualMFADeviceResponse (..),
    newDeleteVirtualMFADeviceResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteVirtualMFADevice' smart constructor.
data DeleteVirtualMFADevice = DeleteVirtualMFADevice'
  { -- | The serial number that uniquely identifies the MFA device. For virtual
    -- MFA devices, the serial number is the same as the ARN.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@:\/-
    serialNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualMFADevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serialNumber', 'deleteVirtualMFADevice_serialNumber' - The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the same as the ARN.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@:\/-
newDeleteVirtualMFADevice ::
  -- | 'serialNumber'
  Prelude.Text ->
  DeleteVirtualMFADevice
newDeleteVirtualMFADevice pSerialNumber_ =
  DeleteVirtualMFADevice'
    { serialNumber =
        pSerialNumber_
    }

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the same as the ARN.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@:\/-
deleteVirtualMFADevice_serialNumber :: Lens.Lens' DeleteVirtualMFADevice Prelude.Text
deleteVirtualMFADevice_serialNumber = Lens.lens (\DeleteVirtualMFADevice' {serialNumber} -> serialNumber) (\s@DeleteVirtualMFADevice' {} a -> s {serialNumber = a} :: DeleteVirtualMFADevice)

instance Prelude.AWSRequest DeleteVirtualMFADevice where
  type
    Rs DeleteVirtualMFADevice =
      DeleteVirtualMFADeviceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteVirtualMFADeviceResponse'

instance Prelude.Hashable DeleteVirtualMFADevice

instance Prelude.NFData DeleteVirtualMFADevice

instance Prelude.ToHeaders DeleteVirtualMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteVirtualMFADevice where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteVirtualMFADevice where
  toQuery DeleteVirtualMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteVirtualMFADevice" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "SerialNumber" Prelude.=: serialNumber
      ]

-- | /See:/ 'newDeleteVirtualMFADeviceResponse' smart constructor.
data DeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVirtualMFADeviceResponse ::
  DeleteVirtualMFADeviceResponse
newDeleteVirtualMFADeviceResponse =
  DeleteVirtualMFADeviceResponse'

instance
  Prelude.NFData
    DeleteVirtualMFADeviceResponse
