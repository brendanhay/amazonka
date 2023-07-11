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
-- Module      : Amazonka.IoTWireless.GetFuotaTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a FUOTA task.
module Amazonka.IoTWireless.GetFuotaTask
  ( -- * Creating a Request
    GetFuotaTask (..),
    newGetFuotaTask,

    -- * Request Lenses
    getFuotaTask_id,

    -- * Destructuring the Response
    GetFuotaTaskResponse (..),
    newGetFuotaTaskResponse,

    -- * Response Lenses
    getFuotaTaskResponse_arn,
    getFuotaTaskResponse_createdAt,
    getFuotaTaskResponse_description,
    getFuotaTaskResponse_firmwareUpdateImage,
    getFuotaTaskResponse_firmwareUpdateRole,
    getFuotaTaskResponse_id,
    getFuotaTaskResponse_loRaWAN,
    getFuotaTaskResponse_name,
    getFuotaTaskResponse_status,
    getFuotaTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFuotaTask' smart constructor.
data GetFuotaTask = GetFuotaTask'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getFuotaTask_id' - Undocumented member.
newGetFuotaTask ::
  -- | 'id'
  Prelude.Text ->
  GetFuotaTask
newGetFuotaTask pId_ = GetFuotaTask' {id = pId_}

-- | Undocumented member.
getFuotaTask_id :: Lens.Lens' GetFuotaTask Prelude.Text
getFuotaTask_id = Lens.lens (\GetFuotaTask' {id} -> id) (\s@GetFuotaTask' {} a -> s {id = a} :: GetFuotaTask)

instance Core.AWSRequest GetFuotaTask where
  type AWSResponse GetFuotaTask = GetFuotaTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFuotaTaskResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "FirmwareUpdateImage")
            Prelude.<*> (x Data..?> "FirmwareUpdateRole")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "LoRaWAN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFuotaTask where
  hashWithSalt _salt GetFuotaTask' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetFuotaTask where
  rnf GetFuotaTask' {..} = Prelude.rnf id

instance Data.ToHeaders GetFuotaTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetFuotaTask where
  toPath GetFuotaTask' {..} =
    Prelude.mconcat ["/fuota-tasks/", Data.toBS id]

instance Data.ToQuery GetFuotaTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFuotaTaskResponse' smart constructor.
data GetFuotaTaskResponse = GetFuotaTaskResponse'
  { arn :: Prelude.Maybe Prelude.Text,
    createdAt :: Prelude.Maybe Data.POSIX,
    description :: Prelude.Maybe Prelude.Text,
    firmwareUpdateImage :: Prelude.Maybe Prelude.Text,
    firmwareUpdateRole :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Maybe Prelude.Text,
    loRaWAN :: Prelude.Maybe LoRaWANFuotaTaskGetInfo,
    name :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe FuotaTaskStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFuotaTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getFuotaTaskResponse_arn' - Undocumented member.
--
-- 'createdAt', 'getFuotaTaskResponse_createdAt' - Undocumented member.
--
-- 'description', 'getFuotaTaskResponse_description' - Undocumented member.
--
-- 'firmwareUpdateImage', 'getFuotaTaskResponse_firmwareUpdateImage' - Undocumented member.
--
-- 'firmwareUpdateRole', 'getFuotaTaskResponse_firmwareUpdateRole' - Undocumented member.
--
-- 'id', 'getFuotaTaskResponse_id' - Undocumented member.
--
-- 'loRaWAN', 'getFuotaTaskResponse_loRaWAN' - Undocumented member.
--
-- 'name', 'getFuotaTaskResponse_name' - Undocumented member.
--
-- 'status', 'getFuotaTaskResponse_status' - Undocumented member.
--
-- 'httpStatus', 'getFuotaTaskResponse_httpStatus' - The response's http status code.
newGetFuotaTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFuotaTaskResponse
newGetFuotaTaskResponse pHttpStatus_ =
  GetFuotaTaskResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      firmwareUpdateImage = Prelude.Nothing,
      firmwareUpdateRole = Prelude.Nothing,
      id = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getFuotaTaskResponse_arn :: Lens.Lens' GetFuotaTaskResponse (Prelude.Maybe Prelude.Text)
getFuotaTaskResponse_arn = Lens.lens (\GetFuotaTaskResponse' {arn} -> arn) (\s@GetFuotaTaskResponse' {} a -> s {arn = a} :: GetFuotaTaskResponse)

-- | Undocumented member.
getFuotaTaskResponse_createdAt :: Lens.Lens' GetFuotaTaskResponse (Prelude.Maybe Prelude.UTCTime)
getFuotaTaskResponse_createdAt = Lens.lens (\GetFuotaTaskResponse' {createdAt} -> createdAt) (\s@GetFuotaTaskResponse' {} a -> s {createdAt = a} :: GetFuotaTaskResponse) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
getFuotaTaskResponse_description :: Lens.Lens' GetFuotaTaskResponse (Prelude.Maybe Prelude.Text)
getFuotaTaskResponse_description = Lens.lens (\GetFuotaTaskResponse' {description} -> description) (\s@GetFuotaTaskResponse' {} a -> s {description = a} :: GetFuotaTaskResponse)

-- | Undocumented member.
getFuotaTaskResponse_firmwareUpdateImage :: Lens.Lens' GetFuotaTaskResponse (Prelude.Maybe Prelude.Text)
getFuotaTaskResponse_firmwareUpdateImage = Lens.lens (\GetFuotaTaskResponse' {firmwareUpdateImage} -> firmwareUpdateImage) (\s@GetFuotaTaskResponse' {} a -> s {firmwareUpdateImage = a} :: GetFuotaTaskResponse)

-- | Undocumented member.
getFuotaTaskResponse_firmwareUpdateRole :: Lens.Lens' GetFuotaTaskResponse (Prelude.Maybe Prelude.Text)
getFuotaTaskResponse_firmwareUpdateRole = Lens.lens (\GetFuotaTaskResponse' {firmwareUpdateRole} -> firmwareUpdateRole) (\s@GetFuotaTaskResponse' {} a -> s {firmwareUpdateRole = a} :: GetFuotaTaskResponse)

-- | Undocumented member.
getFuotaTaskResponse_id :: Lens.Lens' GetFuotaTaskResponse (Prelude.Maybe Prelude.Text)
getFuotaTaskResponse_id = Lens.lens (\GetFuotaTaskResponse' {id} -> id) (\s@GetFuotaTaskResponse' {} a -> s {id = a} :: GetFuotaTaskResponse)

-- | Undocumented member.
getFuotaTaskResponse_loRaWAN :: Lens.Lens' GetFuotaTaskResponse (Prelude.Maybe LoRaWANFuotaTaskGetInfo)
getFuotaTaskResponse_loRaWAN = Lens.lens (\GetFuotaTaskResponse' {loRaWAN} -> loRaWAN) (\s@GetFuotaTaskResponse' {} a -> s {loRaWAN = a} :: GetFuotaTaskResponse)

-- | Undocumented member.
getFuotaTaskResponse_name :: Lens.Lens' GetFuotaTaskResponse (Prelude.Maybe Prelude.Text)
getFuotaTaskResponse_name = Lens.lens (\GetFuotaTaskResponse' {name} -> name) (\s@GetFuotaTaskResponse' {} a -> s {name = a} :: GetFuotaTaskResponse)

-- | Undocumented member.
getFuotaTaskResponse_status :: Lens.Lens' GetFuotaTaskResponse (Prelude.Maybe FuotaTaskStatus)
getFuotaTaskResponse_status = Lens.lens (\GetFuotaTaskResponse' {status} -> status) (\s@GetFuotaTaskResponse' {} a -> s {status = a} :: GetFuotaTaskResponse)

-- | The response's http status code.
getFuotaTaskResponse_httpStatus :: Lens.Lens' GetFuotaTaskResponse Prelude.Int
getFuotaTaskResponse_httpStatus = Lens.lens (\GetFuotaTaskResponse' {httpStatus} -> httpStatus) (\s@GetFuotaTaskResponse' {} a -> s {httpStatus = a} :: GetFuotaTaskResponse)

instance Prelude.NFData GetFuotaTaskResponse where
  rnf GetFuotaTaskResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf firmwareUpdateImage
      `Prelude.seq` Prelude.rnf firmwareUpdateRole
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
