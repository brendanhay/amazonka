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
-- Module      : Amazonka.IoTRoboRunner.UpdateWorkerFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to update a worker fleet
module Amazonka.IoTRoboRunner.UpdateWorkerFleet
  ( -- * Creating a Request
    UpdateWorkerFleet (..),
    newUpdateWorkerFleet,

    -- * Request Lenses
    updateWorkerFleet_name,
    updateWorkerFleet_additionalFixedProperties,
    updateWorkerFleet_id,

    -- * Destructuring the Response
    UpdateWorkerFleetResponse (..),
    newUpdateWorkerFleetResponse,

    -- * Response Lenses
    updateWorkerFleetResponse_additionalFixedProperties,
    updateWorkerFleetResponse_httpStatus,
    updateWorkerFleetResponse_arn,
    updateWorkerFleetResponse_id,
    updateWorkerFleetResponse_name,
    updateWorkerFleetResponse_updatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorkerFleet' smart constructor.
data UpdateWorkerFleet = UpdateWorkerFleet'
  { name :: Prelude.Maybe Prelude.Text,
    additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkerFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateWorkerFleet_name' - Undocumented member.
--
-- 'additionalFixedProperties', 'updateWorkerFleet_additionalFixedProperties' - Undocumented member.
--
-- 'id', 'updateWorkerFleet_id' - Undocumented member.
newUpdateWorkerFleet ::
  -- | 'id'
  Prelude.Text ->
  UpdateWorkerFleet
newUpdateWorkerFleet pId_ =
  UpdateWorkerFleet'
    { name = Prelude.Nothing,
      additionalFixedProperties = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
updateWorkerFleet_name :: Lens.Lens' UpdateWorkerFleet (Prelude.Maybe Prelude.Text)
updateWorkerFleet_name = Lens.lens (\UpdateWorkerFleet' {name} -> name) (\s@UpdateWorkerFleet' {} a -> s {name = a} :: UpdateWorkerFleet)

-- | Undocumented member.
updateWorkerFleet_additionalFixedProperties :: Lens.Lens' UpdateWorkerFleet (Prelude.Maybe Prelude.Text)
updateWorkerFleet_additionalFixedProperties = Lens.lens (\UpdateWorkerFleet' {additionalFixedProperties} -> additionalFixedProperties) (\s@UpdateWorkerFleet' {} a -> s {additionalFixedProperties = a} :: UpdateWorkerFleet)

-- | Undocumented member.
updateWorkerFleet_id :: Lens.Lens' UpdateWorkerFleet Prelude.Text
updateWorkerFleet_id = Lens.lens (\UpdateWorkerFleet' {id} -> id) (\s@UpdateWorkerFleet' {} a -> s {id = a} :: UpdateWorkerFleet)

instance Core.AWSRequest UpdateWorkerFleet where
  type
    AWSResponse UpdateWorkerFleet =
      UpdateWorkerFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkerFleetResponse'
            Prelude.<$> (x Core..?> "additionalFixedProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "id")
            Prelude.<*> (x Core..:> "name")
            Prelude.<*> (x Core..:> "updatedAt")
      )

instance Prelude.Hashable UpdateWorkerFleet where
  hashWithSalt _salt UpdateWorkerFleet' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` additionalFixedProperties
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateWorkerFleet where
  rnf UpdateWorkerFleet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders UpdateWorkerFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateWorkerFleet where
  toJSON UpdateWorkerFleet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("additionalFixedProperties" Core..=)
              Prelude.<$> additionalFixedProperties,
            Prelude.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath UpdateWorkerFleet where
  toPath = Prelude.const "/updateWorkerFleet"

instance Core.ToQuery UpdateWorkerFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkerFleetResponse' smart constructor.
data UpdateWorkerFleetResponse = UpdateWorkerFleetResponse'
  { additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    name :: Prelude.Text,
    updatedAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkerFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalFixedProperties', 'updateWorkerFleetResponse_additionalFixedProperties' - Undocumented member.
--
-- 'httpStatus', 'updateWorkerFleetResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'updateWorkerFleetResponse_arn' - Undocumented member.
--
-- 'id', 'updateWorkerFleetResponse_id' - Undocumented member.
--
-- 'name', 'updateWorkerFleetResponse_name' - Undocumented member.
--
-- 'updatedAt', 'updateWorkerFleetResponse_updatedAt' - Undocumented member.
newUpdateWorkerFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  UpdateWorkerFleetResponse
newUpdateWorkerFleetResponse
  pHttpStatus_
  pArn_
  pId_
  pName_
  pUpdatedAt_ =
    UpdateWorkerFleetResponse'
      { additionalFixedProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        name = pName_,
        updatedAt = Core._Time Lens.# pUpdatedAt_
      }

-- | Undocumented member.
updateWorkerFleetResponse_additionalFixedProperties :: Lens.Lens' UpdateWorkerFleetResponse (Prelude.Maybe Prelude.Text)
updateWorkerFleetResponse_additionalFixedProperties = Lens.lens (\UpdateWorkerFleetResponse' {additionalFixedProperties} -> additionalFixedProperties) (\s@UpdateWorkerFleetResponse' {} a -> s {additionalFixedProperties = a} :: UpdateWorkerFleetResponse)

-- | The response's http status code.
updateWorkerFleetResponse_httpStatus :: Lens.Lens' UpdateWorkerFleetResponse Prelude.Int
updateWorkerFleetResponse_httpStatus = Lens.lens (\UpdateWorkerFleetResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkerFleetResponse' {} a -> s {httpStatus = a} :: UpdateWorkerFleetResponse)

-- | Undocumented member.
updateWorkerFleetResponse_arn :: Lens.Lens' UpdateWorkerFleetResponse Prelude.Text
updateWorkerFleetResponse_arn = Lens.lens (\UpdateWorkerFleetResponse' {arn} -> arn) (\s@UpdateWorkerFleetResponse' {} a -> s {arn = a} :: UpdateWorkerFleetResponse)

-- | Undocumented member.
updateWorkerFleetResponse_id :: Lens.Lens' UpdateWorkerFleetResponse Prelude.Text
updateWorkerFleetResponse_id = Lens.lens (\UpdateWorkerFleetResponse' {id} -> id) (\s@UpdateWorkerFleetResponse' {} a -> s {id = a} :: UpdateWorkerFleetResponse)

-- | Undocumented member.
updateWorkerFleetResponse_name :: Lens.Lens' UpdateWorkerFleetResponse Prelude.Text
updateWorkerFleetResponse_name = Lens.lens (\UpdateWorkerFleetResponse' {name} -> name) (\s@UpdateWorkerFleetResponse' {} a -> s {name = a} :: UpdateWorkerFleetResponse)

-- | Undocumented member.
updateWorkerFleetResponse_updatedAt :: Lens.Lens' UpdateWorkerFleetResponse Prelude.UTCTime
updateWorkerFleetResponse_updatedAt = Lens.lens (\UpdateWorkerFleetResponse' {updatedAt} -> updatedAt) (\s@UpdateWorkerFleetResponse' {} a -> s {updatedAt = a} :: UpdateWorkerFleetResponse) Prelude.. Core._Time

instance Prelude.NFData UpdateWorkerFleetResponse where
  rnf UpdateWorkerFleetResponse' {..} =
    Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf updatedAt
