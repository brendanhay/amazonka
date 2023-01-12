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
-- Module      : Amazonka.IAM.DeleteVirtualMFADevice
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.IAM.DeleteVirtualMFADevice
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteVirtualMFADevice where
  type
    AWSResponse DeleteVirtualMFADevice =
      DeleteVirtualMFADeviceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteVirtualMFADeviceResponse'

instance Prelude.Hashable DeleteVirtualMFADevice where
  hashWithSalt _salt DeleteVirtualMFADevice' {..} =
    _salt `Prelude.hashWithSalt` serialNumber

instance Prelude.NFData DeleteVirtualMFADevice where
  rnf DeleteVirtualMFADevice' {..} =
    Prelude.rnf serialNumber

instance Data.ToHeaders DeleteVirtualMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVirtualMFADevice where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVirtualMFADevice where
  toQuery DeleteVirtualMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteVirtualMFADevice" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "SerialNumber" Data.=: serialNumber
      ]

-- | /See:/ 'newDeleteVirtualMFADeviceResponse' smart constructor.
data DeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
