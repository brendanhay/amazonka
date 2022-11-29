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
-- Module      : Amazonka.Greengrass.UpdateDeviceDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a device definition.
module Amazonka.Greengrass.UpdateDeviceDefinition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDeviceDefinition' smart constructor.
data UpdateDeviceDefinition = UpdateDeviceDefinition'
  { -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateDeviceDefinition
newUpdateDeviceDefinition pDeviceDefinitionId_ =
  UpdateDeviceDefinition'
    { name = Prelude.Nothing,
      deviceDefinitionId = pDeviceDefinitionId_
    }

-- | The name of the definition.
updateDeviceDefinition_name :: Lens.Lens' UpdateDeviceDefinition (Prelude.Maybe Prelude.Text)
updateDeviceDefinition_name = Lens.lens (\UpdateDeviceDefinition' {name} -> name) (\s@UpdateDeviceDefinition' {} a -> s {name = a} :: UpdateDeviceDefinition)

-- | The ID of the device definition.
updateDeviceDefinition_deviceDefinitionId :: Lens.Lens' UpdateDeviceDefinition Prelude.Text
updateDeviceDefinition_deviceDefinitionId = Lens.lens (\UpdateDeviceDefinition' {deviceDefinitionId} -> deviceDefinitionId) (\s@UpdateDeviceDefinition' {} a -> s {deviceDefinitionId = a} :: UpdateDeviceDefinition)

instance Core.AWSRequest UpdateDeviceDefinition where
  type
    AWSResponse UpdateDeviceDefinition =
      UpdateDeviceDefinitionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeviceDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDeviceDefinition where
  hashWithSalt _salt UpdateDeviceDefinition' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` deviceDefinitionId

instance Prelude.NFData UpdateDeviceDefinition where
  rnf UpdateDeviceDefinition' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf deviceDefinitionId

instance Core.ToHeaders UpdateDeviceDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDeviceDefinition where
  toJSON UpdateDeviceDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Name" Core..=) Prelude.<$> name]
      )

instance Core.ToPath UpdateDeviceDefinition where
  toPath UpdateDeviceDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/devices/",
        Core.toBS deviceDefinitionId
      ]

instance Core.ToQuery UpdateDeviceDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDeviceDefinitionResponse' smart constructor.
data UpdateDeviceDefinitionResponse = UpdateDeviceDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateDeviceDefinitionResponse
newUpdateDeviceDefinitionResponse pHttpStatus_ =
  UpdateDeviceDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDeviceDefinitionResponse_httpStatus :: Lens.Lens' UpdateDeviceDefinitionResponse Prelude.Int
updateDeviceDefinitionResponse_httpStatus = Lens.lens (\UpdateDeviceDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateDeviceDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateDeviceDefinitionResponse)

instance
  Prelude.NFData
    UpdateDeviceDefinitionResponse
  where
  rnf UpdateDeviceDefinitionResponse' {..} =
    Prelude.rnf httpStatus
