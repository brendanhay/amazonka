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
-- Module      : Amazonka.AlexaBusiness.DeleteDeviceUsageData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When this action is called for a specified shared device, it allows
-- authorized users to delete the device\'s entire previous history of
-- voice input data and associated response data. This action can be called
-- once every 24 hours for a specific shared device.
module Amazonka.AlexaBusiness.DeleteDeviceUsageData
  ( -- * Creating a Request
    DeleteDeviceUsageData (..),
    newDeleteDeviceUsageData,

    -- * Request Lenses
    deleteDeviceUsageData_deviceArn,
    deleteDeviceUsageData_deviceUsageType,

    -- * Destructuring the Response
    DeleteDeviceUsageDataResponse (..),
    newDeleteDeviceUsageDataResponse,

    -- * Response Lenses
    deleteDeviceUsageDataResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDeviceUsageData' smart constructor.
data DeleteDeviceUsageData = DeleteDeviceUsageData'
  { -- | The ARN of the device.
    deviceArn :: Prelude.Text,
    -- | The type of usage data to delete.
    deviceUsageType :: DeviceUsageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeviceUsageData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'deleteDeviceUsageData_deviceArn' - The ARN of the device.
--
-- 'deviceUsageType', 'deleteDeviceUsageData_deviceUsageType' - The type of usage data to delete.
newDeleteDeviceUsageData ::
  -- | 'deviceArn'
  Prelude.Text ->
  -- | 'deviceUsageType'
  DeviceUsageType ->
  DeleteDeviceUsageData
newDeleteDeviceUsageData
  pDeviceArn_
  pDeviceUsageType_ =
    DeleteDeviceUsageData'
      { deviceArn = pDeviceArn_,
        deviceUsageType = pDeviceUsageType_
      }

-- | The ARN of the device.
deleteDeviceUsageData_deviceArn :: Lens.Lens' DeleteDeviceUsageData Prelude.Text
deleteDeviceUsageData_deviceArn = Lens.lens (\DeleteDeviceUsageData' {deviceArn} -> deviceArn) (\s@DeleteDeviceUsageData' {} a -> s {deviceArn = a} :: DeleteDeviceUsageData)

-- | The type of usage data to delete.
deleteDeviceUsageData_deviceUsageType :: Lens.Lens' DeleteDeviceUsageData DeviceUsageType
deleteDeviceUsageData_deviceUsageType = Lens.lens (\DeleteDeviceUsageData' {deviceUsageType} -> deviceUsageType) (\s@DeleteDeviceUsageData' {} a -> s {deviceUsageType = a} :: DeleteDeviceUsageData)

instance Core.AWSRequest DeleteDeviceUsageData where
  type
    AWSResponse DeleteDeviceUsageData =
      DeleteDeviceUsageDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDeviceUsageDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDeviceUsageData where
  hashWithSalt _salt DeleteDeviceUsageData' {..} =
    _salt
      `Prelude.hashWithSalt` deviceArn
      `Prelude.hashWithSalt` deviceUsageType

instance Prelude.NFData DeleteDeviceUsageData where
  rnf DeleteDeviceUsageData' {..} =
    Prelude.rnf deviceArn `Prelude.seq`
      Prelude.rnf deviceUsageType

instance Data.ToHeaders DeleteDeviceUsageData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.DeleteDeviceUsageData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDeviceUsageData where
  toJSON DeleteDeviceUsageData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DeviceArn" Data..= deviceArn),
            Prelude.Just
              ("DeviceUsageType" Data..= deviceUsageType)
          ]
      )

instance Data.ToPath DeleteDeviceUsageData where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDeviceUsageData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeviceUsageDataResponse' smart constructor.
data DeleteDeviceUsageDataResponse = DeleteDeviceUsageDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeviceUsageDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDeviceUsageDataResponse_httpStatus' - The response's http status code.
newDeleteDeviceUsageDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDeviceUsageDataResponse
newDeleteDeviceUsageDataResponse pHttpStatus_ =
  DeleteDeviceUsageDataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDeviceUsageDataResponse_httpStatus :: Lens.Lens' DeleteDeviceUsageDataResponse Prelude.Int
deleteDeviceUsageDataResponse_httpStatus = Lens.lens (\DeleteDeviceUsageDataResponse' {httpStatus} -> httpStatus) (\s@DeleteDeviceUsageDataResponse' {} a -> s {httpStatus = a} :: DeleteDeviceUsageDataResponse)

instance Prelude.NFData DeleteDeviceUsageDataResponse where
  rnf DeleteDeviceUsageDataResponse' {..} =
    Prelude.rnf httpStatus
