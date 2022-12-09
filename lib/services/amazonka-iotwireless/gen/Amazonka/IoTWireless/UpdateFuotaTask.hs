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
-- Module      : Amazonka.IoTWireless.UpdateFuotaTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates properties of a FUOTA task.
module Amazonka.IoTWireless.UpdateFuotaTask
  ( -- * Creating a Request
    UpdateFuotaTask (..),
    newUpdateFuotaTask,

    -- * Request Lenses
    updateFuotaTask_description,
    updateFuotaTask_firmwareUpdateImage,
    updateFuotaTask_firmwareUpdateRole,
    updateFuotaTask_loRaWAN,
    updateFuotaTask_name,
    updateFuotaTask_id,

    -- * Destructuring the Response
    UpdateFuotaTaskResponse (..),
    newUpdateFuotaTaskResponse,

    -- * Response Lenses
    updateFuotaTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFuotaTask' smart constructor.
data UpdateFuotaTask = UpdateFuotaTask'
  { description :: Prelude.Maybe Prelude.Text,
    firmwareUpdateImage :: Prelude.Maybe Prelude.Text,
    firmwareUpdateRole :: Prelude.Maybe Prelude.Text,
    loRaWAN :: Prelude.Maybe LoRaWANFuotaTask,
    name :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateFuotaTask_description' - Undocumented member.
--
-- 'firmwareUpdateImage', 'updateFuotaTask_firmwareUpdateImage' - Undocumented member.
--
-- 'firmwareUpdateRole', 'updateFuotaTask_firmwareUpdateRole' - Undocumented member.
--
-- 'loRaWAN', 'updateFuotaTask_loRaWAN' - Undocumented member.
--
-- 'name', 'updateFuotaTask_name' - Undocumented member.
--
-- 'id', 'updateFuotaTask_id' - Undocumented member.
newUpdateFuotaTask ::
  -- | 'id'
  Prelude.Text ->
  UpdateFuotaTask
newUpdateFuotaTask pId_ =
  UpdateFuotaTask'
    { description = Prelude.Nothing,
      firmwareUpdateImage = Prelude.Nothing,
      firmwareUpdateRole = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      name = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
updateFuotaTask_description :: Lens.Lens' UpdateFuotaTask (Prelude.Maybe Prelude.Text)
updateFuotaTask_description = Lens.lens (\UpdateFuotaTask' {description} -> description) (\s@UpdateFuotaTask' {} a -> s {description = a} :: UpdateFuotaTask)

-- | Undocumented member.
updateFuotaTask_firmwareUpdateImage :: Lens.Lens' UpdateFuotaTask (Prelude.Maybe Prelude.Text)
updateFuotaTask_firmwareUpdateImage = Lens.lens (\UpdateFuotaTask' {firmwareUpdateImage} -> firmwareUpdateImage) (\s@UpdateFuotaTask' {} a -> s {firmwareUpdateImage = a} :: UpdateFuotaTask)

-- | Undocumented member.
updateFuotaTask_firmwareUpdateRole :: Lens.Lens' UpdateFuotaTask (Prelude.Maybe Prelude.Text)
updateFuotaTask_firmwareUpdateRole = Lens.lens (\UpdateFuotaTask' {firmwareUpdateRole} -> firmwareUpdateRole) (\s@UpdateFuotaTask' {} a -> s {firmwareUpdateRole = a} :: UpdateFuotaTask)

-- | Undocumented member.
updateFuotaTask_loRaWAN :: Lens.Lens' UpdateFuotaTask (Prelude.Maybe LoRaWANFuotaTask)
updateFuotaTask_loRaWAN = Lens.lens (\UpdateFuotaTask' {loRaWAN} -> loRaWAN) (\s@UpdateFuotaTask' {} a -> s {loRaWAN = a} :: UpdateFuotaTask)

-- | Undocumented member.
updateFuotaTask_name :: Lens.Lens' UpdateFuotaTask (Prelude.Maybe Prelude.Text)
updateFuotaTask_name = Lens.lens (\UpdateFuotaTask' {name} -> name) (\s@UpdateFuotaTask' {} a -> s {name = a} :: UpdateFuotaTask)

-- | Undocumented member.
updateFuotaTask_id :: Lens.Lens' UpdateFuotaTask Prelude.Text
updateFuotaTask_id = Lens.lens (\UpdateFuotaTask' {id} -> id) (\s@UpdateFuotaTask' {} a -> s {id = a} :: UpdateFuotaTask)

instance Core.AWSRequest UpdateFuotaTask where
  type
    AWSResponse UpdateFuotaTask =
      UpdateFuotaTaskResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFuotaTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFuotaTask where
  hashWithSalt _salt UpdateFuotaTask' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` firmwareUpdateImage
      `Prelude.hashWithSalt` firmwareUpdateRole
      `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateFuotaTask where
  rnf UpdateFuotaTask' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf firmwareUpdateImage
      `Prelude.seq` Prelude.rnf firmwareUpdateRole
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateFuotaTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateFuotaTask where
  toJSON UpdateFuotaTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("FirmwareUpdateImage" Data..=)
              Prelude.<$> firmwareUpdateImage,
            ("FirmwareUpdateRole" Data..=)
              Prelude.<$> firmwareUpdateRole,
            ("LoRaWAN" Data..=) Prelude.<$> loRaWAN,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateFuotaTask where
  toPath UpdateFuotaTask' {..} =
    Prelude.mconcat ["/fuota-tasks/", Data.toBS id]

instance Data.ToQuery UpdateFuotaTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFuotaTaskResponse' smart constructor.
data UpdateFuotaTaskResponse = UpdateFuotaTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFuotaTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFuotaTaskResponse_httpStatus' - The response's http status code.
newUpdateFuotaTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFuotaTaskResponse
newUpdateFuotaTaskResponse pHttpStatus_ =
  UpdateFuotaTaskResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateFuotaTaskResponse_httpStatus :: Lens.Lens' UpdateFuotaTaskResponse Prelude.Int
updateFuotaTaskResponse_httpStatus = Lens.lens (\UpdateFuotaTaskResponse' {httpStatus} -> httpStatus) (\s@UpdateFuotaTaskResponse' {} a -> s {httpStatus = a} :: UpdateFuotaTaskResponse)

instance Prelude.NFData UpdateFuotaTaskResponse where
  rnf UpdateFuotaTaskResponse' {..} =
    Prelude.rnf httpStatus
