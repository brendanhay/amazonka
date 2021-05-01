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
-- Module      : Network.AWS.Greengrass.DeleteDeviceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device definition.
module Network.AWS.Greengrass.DeleteDeviceDefinition
  ( -- * Creating a Request
    DeleteDeviceDefinition (..),
    newDeleteDeviceDefinition,

    -- * Request Lenses
    deleteDeviceDefinition_deviceDefinitionId,

    -- * Destructuring the Response
    DeleteDeviceDefinitionResponse (..),
    newDeleteDeviceDefinitionResponse,

    -- * Response Lenses
    deleteDeviceDefinitionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDeviceDefinition' smart constructor.
data DeleteDeviceDefinition = DeleteDeviceDefinition'
  { -- | The ID of the device definition.
    deviceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeviceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceDefinitionId', 'deleteDeviceDefinition_deviceDefinitionId' - The ID of the device definition.
newDeleteDeviceDefinition ::
  -- | 'deviceDefinitionId'
  Prelude.Text ->
  DeleteDeviceDefinition
newDeleteDeviceDefinition pDeviceDefinitionId_ =
  DeleteDeviceDefinition'
    { deviceDefinitionId =
        pDeviceDefinitionId_
    }

-- | The ID of the device definition.
deleteDeviceDefinition_deviceDefinitionId :: Lens.Lens' DeleteDeviceDefinition Prelude.Text
deleteDeviceDefinition_deviceDefinitionId = Lens.lens (\DeleteDeviceDefinition' {deviceDefinitionId} -> deviceDefinitionId) (\s@DeleteDeviceDefinition' {} a -> s {deviceDefinitionId = a} :: DeleteDeviceDefinition)

instance Prelude.AWSRequest DeleteDeviceDefinition where
  type
    Rs DeleteDeviceDefinition =
      DeleteDeviceDefinitionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDeviceDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDeviceDefinition

instance Prelude.NFData DeleteDeviceDefinition

instance Prelude.ToHeaders DeleteDeviceDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteDeviceDefinition where
  toPath DeleteDeviceDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/devices/",
        Prelude.toBS deviceDefinitionId
      ]

instance Prelude.ToQuery DeleteDeviceDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeviceDefinitionResponse' smart constructor.
data DeleteDeviceDefinitionResponse = DeleteDeviceDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeviceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDeviceDefinitionResponse_httpStatus' - The response's http status code.
newDeleteDeviceDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDeviceDefinitionResponse
newDeleteDeviceDefinitionResponse pHttpStatus_ =
  DeleteDeviceDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDeviceDefinitionResponse_httpStatus :: Lens.Lens' DeleteDeviceDefinitionResponse Prelude.Int
deleteDeviceDefinitionResponse_httpStatus = Lens.lens (\DeleteDeviceDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteDeviceDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteDeviceDefinitionResponse)

instance
  Prelude.NFData
    DeleteDeviceDefinitionResponse
