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
-- Module      : Network.AWS.Greengrass.UpdateDeviceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a device definition.
module Network.AWS.Greengrass.UpdateDeviceDefinition
  ( -- * Creating a Request
    UpdateDeviceDefinition (..),
    newUpdateDeviceDefinition,

    -- * Request Lenses
    updateDeviceDefinition_name,
    updateDeviceDefinition_deviceDefinitionId,

    -- * Destructuring the Response
    UpdateDeviceDefinitionResponse (..),
    newUpdateDeviceDefinitionResponse,

    -- * Response Lenses
    updateDeviceDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDeviceDefinition' smart constructor.
data UpdateDeviceDefinition = UpdateDeviceDefinition'
  { -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDeviceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateDeviceDefinition_name' - The name of the definition.
--
-- 'deviceDefinitionId', 'updateDeviceDefinition_deviceDefinitionId' - The ID of the device definition.
newUpdateDeviceDefinition ::
  -- | 'deviceDefinitionId'
  Core.Text ->
  UpdateDeviceDefinition
newUpdateDeviceDefinition pDeviceDefinitionId_ =
  UpdateDeviceDefinition'
    { name = Core.Nothing,
      deviceDefinitionId = pDeviceDefinitionId_
    }

-- | The name of the definition.
updateDeviceDefinition_name :: Lens.Lens' UpdateDeviceDefinition (Core.Maybe Core.Text)
updateDeviceDefinition_name = Lens.lens (\UpdateDeviceDefinition' {name} -> name) (\s@UpdateDeviceDefinition' {} a -> s {name = a} :: UpdateDeviceDefinition)

-- | The ID of the device definition.
updateDeviceDefinition_deviceDefinitionId :: Lens.Lens' UpdateDeviceDefinition Core.Text
updateDeviceDefinition_deviceDefinitionId = Lens.lens (\UpdateDeviceDefinition' {deviceDefinitionId} -> deviceDefinitionId) (\s@UpdateDeviceDefinition' {} a -> s {deviceDefinitionId = a} :: UpdateDeviceDefinition)

instance Core.AWSRequest UpdateDeviceDefinition where
  type
    AWSResponse UpdateDeviceDefinition =
      UpdateDeviceDefinitionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeviceDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDeviceDefinition

instance Core.NFData UpdateDeviceDefinition

instance Core.ToHeaders UpdateDeviceDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDeviceDefinition where
  toJSON UpdateDeviceDefinition' {..} =
    Core.object
      (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.ToPath UpdateDeviceDefinition where
  toPath UpdateDeviceDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/devices/",
        Core.toBS deviceDefinitionId
      ]

instance Core.ToQuery UpdateDeviceDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDeviceDefinitionResponse' smart constructor.
data UpdateDeviceDefinitionResponse = UpdateDeviceDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDeviceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDeviceDefinitionResponse_httpStatus' - The response's http status code.
newUpdateDeviceDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDeviceDefinitionResponse
newUpdateDeviceDefinitionResponse pHttpStatus_ =
  UpdateDeviceDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDeviceDefinitionResponse_httpStatus :: Lens.Lens' UpdateDeviceDefinitionResponse Core.Int
updateDeviceDefinitionResponse_httpStatus = Lens.lens (\UpdateDeviceDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateDeviceDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateDeviceDefinitionResponse)

instance Core.NFData UpdateDeviceDefinitionResponse
