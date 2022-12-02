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
-- Module      : Amazonka.IoTRoboRunner.UpdateWorker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to update a worker
module Amazonka.IoTRoboRunner.UpdateWorker
  ( -- * Creating a Request
    UpdateWorker (..),
    newUpdateWorker,

    -- * Request Lenses
    updateWorker_name,
    updateWorker_additionalTransientProperties,
    updateWorker_orientation,
    updateWorker_vendorProperties,
    updateWorker_position,
    updateWorker_additionalFixedProperties,
    updateWorker_id,

    -- * Destructuring the Response
    UpdateWorkerResponse (..),
    newUpdateWorkerResponse,

    -- * Response Lenses
    updateWorkerResponse_additionalTransientProperties,
    updateWorkerResponse_orientation,
    updateWorkerResponse_vendorProperties,
    updateWorkerResponse_position,
    updateWorkerResponse_additionalFixedProperties,
    updateWorkerResponse_httpStatus,
    updateWorkerResponse_arn,
    updateWorkerResponse_id,
    updateWorkerResponse_fleet,
    updateWorkerResponse_updatedAt,
    updateWorkerResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorker' smart constructor.
data UpdateWorker = UpdateWorker'
  { name :: Prelude.Maybe Prelude.Text,
    additionalTransientProperties :: Prelude.Maybe Prelude.Text,
    orientation :: Prelude.Maybe Orientation,
    vendorProperties :: Prelude.Maybe VendorProperties,
    position :: Prelude.Maybe PositionCoordinates,
    additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateWorker_name' - Undocumented member.
--
-- 'additionalTransientProperties', 'updateWorker_additionalTransientProperties' - Undocumented member.
--
-- 'orientation', 'updateWorker_orientation' - Undocumented member.
--
-- 'vendorProperties', 'updateWorker_vendorProperties' - Undocumented member.
--
-- 'position', 'updateWorker_position' - Undocumented member.
--
-- 'additionalFixedProperties', 'updateWorker_additionalFixedProperties' - Undocumented member.
--
-- 'id', 'updateWorker_id' - Undocumented member.
newUpdateWorker ::
  -- | 'id'
  Prelude.Text ->
  UpdateWorker
newUpdateWorker pId_ =
  UpdateWorker'
    { name = Prelude.Nothing,
      additionalTransientProperties = Prelude.Nothing,
      orientation = Prelude.Nothing,
      vendorProperties = Prelude.Nothing,
      position = Prelude.Nothing,
      additionalFixedProperties = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
updateWorker_name :: Lens.Lens' UpdateWorker (Prelude.Maybe Prelude.Text)
updateWorker_name = Lens.lens (\UpdateWorker' {name} -> name) (\s@UpdateWorker' {} a -> s {name = a} :: UpdateWorker)

-- | Undocumented member.
updateWorker_additionalTransientProperties :: Lens.Lens' UpdateWorker (Prelude.Maybe Prelude.Text)
updateWorker_additionalTransientProperties = Lens.lens (\UpdateWorker' {additionalTransientProperties} -> additionalTransientProperties) (\s@UpdateWorker' {} a -> s {additionalTransientProperties = a} :: UpdateWorker)

-- | Undocumented member.
updateWorker_orientation :: Lens.Lens' UpdateWorker (Prelude.Maybe Orientation)
updateWorker_orientation = Lens.lens (\UpdateWorker' {orientation} -> orientation) (\s@UpdateWorker' {} a -> s {orientation = a} :: UpdateWorker)

-- | Undocumented member.
updateWorker_vendorProperties :: Lens.Lens' UpdateWorker (Prelude.Maybe VendorProperties)
updateWorker_vendorProperties = Lens.lens (\UpdateWorker' {vendorProperties} -> vendorProperties) (\s@UpdateWorker' {} a -> s {vendorProperties = a} :: UpdateWorker)

-- | Undocumented member.
updateWorker_position :: Lens.Lens' UpdateWorker (Prelude.Maybe PositionCoordinates)
updateWorker_position = Lens.lens (\UpdateWorker' {position} -> position) (\s@UpdateWorker' {} a -> s {position = a} :: UpdateWorker)

-- | Undocumented member.
updateWorker_additionalFixedProperties :: Lens.Lens' UpdateWorker (Prelude.Maybe Prelude.Text)
updateWorker_additionalFixedProperties = Lens.lens (\UpdateWorker' {additionalFixedProperties} -> additionalFixedProperties) (\s@UpdateWorker' {} a -> s {additionalFixedProperties = a} :: UpdateWorker)

-- | Undocumented member.
updateWorker_id :: Lens.Lens' UpdateWorker Prelude.Text
updateWorker_id = Lens.lens (\UpdateWorker' {id} -> id) (\s@UpdateWorker' {} a -> s {id = a} :: UpdateWorker)

instance Core.AWSRequest UpdateWorker where
  type AWSResponse UpdateWorker = UpdateWorkerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkerResponse'
            Prelude.<$> (x Data..?> "additionalTransientProperties")
            Prelude.<*> (x Data..?> "orientation")
            Prelude.<*> (x Data..?> "vendorProperties")
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (x Data..?> "additionalFixedProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "fleet")
            Prelude.<*> (x Data..:> "updatedAt")
            Prelude.<*> (x Data..:> "name")
      )

instance Prelude.Hashable UpdateWorker where
  hashWithSalt _salt UpdateWorker' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` additionalTransientProperties
      `Prelude.hashWithSalt` orientation
      `Prelude.hashWithSalt` vendorProperties
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` additionalFixedProperties
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateWorker where
  rnf UpdateWorker' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf additionalTransientProperties
      `Prelude.seq` Prelude.rnf orientation
      `Prelude.seq` Prelude.rnf vendorProperties
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateWorker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorker where
  toJSON UpdateWorker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("additionalTransientProperties" Data..=)
              Prelude.<$> additionalTransientProperties,
            ("orientation" Data..=) Prelude.<$> orientation,
            ("vendorProperties" Data..=)
              Prelude.<$> vendorProperties,
            ("position" Data..=) Prelude.<$> position,
            ("additionalFixedProperties" Data..=)
              Prelude.<$> additionalFixedProperties,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath UpdateWorker where
  toPath = Prelude.const "/updateWorker"

instance Data.ToQuery UpdateWorker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkerResponse' smart constructor.
data UpdateWorkerResponse = UpdateWorkerResponse'
  { additionalTransientProperties :: Prelude.Maybe Prelude.Text,
    orientation :: Prelude.Maybe Orientation,
    vendorProperties :: Prelude.Maybe VendorProperties,
    position :: Prelude.Maybe PositionCoordinates,
    additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    fleet :: Prelude.Text,
    updatedAt :: Data.POSIX,
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalTransientProperties', 'updateWorkerResponse_additionalTransientProperties' - Undocumented member.
--
-- 'orientation', 'updateWorkerResponse_orientation' - Undocumented member.
--
-- 'vendorProperties', 'updateWorkerResponse_vendorProperties' - Undocumented member.
--
-- 'position', 'updateWorkerResponse_position' - Undocumented member.
--
-- 'additionalFixedProperties', 'updateWorkerResponse_additionalFixedProperties' - Undocumented member.
--
-- 'httpStatus', 'updateWorkerResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'updateWorkerResponse_arn' - Undocumented member.
--
-- 'id', 'updateWorkerResponse_id' - Undocumented member.
--
-- 'fleet', 'updateWorkerResponse_fleet' - Undocumented member.
--
-- 'updatedAt', 'updateWorkerResponse_updatedAt' - Undocumented member.
--
-- 'name', 'updateWorkerResponse_name' - Undocumented member.
newUpdateWorkerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'fleet'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  UpdateWorkerResponse
newUpdateWorkerResponse
  pHttpStatus_
  pArn_
  pId_
  pFleet_
  pUpdatedAt_
  pName_ =
    UpdateWorkerResponse'
      { additionalTransientProperties =
          Prelude.Nothing,
        orientation = Prelude.Nothing,
        vendorProperties = Prelude.Nothing,
        position = Prelude.Nothing,
        additionalFixedProperties = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        fleet = pFleet_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        name = pName_
      }

-- | Undocumented member.
updateWorkerResponse_additionalTransientProperties :: Lens.Lens' UpdateWorkerResponse (Prelude.Maybe Prelude.Text)
updateWorkerResponse_additionalTransientProperties = Lens.lens (\UpdateWorkerResponse' {additionalTransientProperties} -> additionalTransientProperties) (\s@UpdateWorkerResponse' {} a -> s {additionalTransientProperties = a} :: UpdateWorkerResponse)

-- | Undocumented member.
updateWorkerResponse_orientation :: Lens.Lens' UpdateWorkerResponse (Prelude.Maybe Orientation)
updateWorkerResponse_orientation = Lens.lens (\UpdateWorkerResponse' {orientation} -> orientation) (\s@UpdateWorkerResponse' {} a -> s {orientation = a} :: UpdateWorkerResponse)

-- | Undocumented member.
updateWorkerResponse_vendorProperties :: Lens.Lens' UpdateWorkerResponse (Prelude.Maybe VendorProperties)
updateWorkerResponse_vendorProperties = Lens.lens (\UpdateWorkerResponse' {vendorProperties} -> vendorProperties) (\s@UpdateWorkerResponse' {} a -> s {vendorProperties = a} :: UpdateWorkerResponse)

-- | Undocumented member.
updateWorkerResponse_position :: Lens.Lens' UpdateWorkerResponse (Prelude.Maybe PositionCoordinates)
updateWorkerResponse_position = Lens.lens (\UpdateWorkerResponse' {position} -> position) (\s@UpdateWorkerResponse' {} a -> s {position = a} :: UpdateWorkerResponse)

-- | Undocumented member.
updateWorkerResponse_additionalFixedProperties :: Lens.Lens' UpdateWorkerResponse (Prelude.Maybe Prelude.Text)
updateWorkerResponse_additionalFixedProperties = Lens.lens (\UpdateWorkerResponse' {additionalFixedProperties} -> additionalFixedProperties) (\s@UpdateWorkerResponse' {} a -> s {additionalFixedProperties = a} :: UpdateWorkerResponse)

-- | The response's http status code.
updateWorkerResponse_httpStatus :: Lens.Lens' UpdateWorkerResponse Prelude.Int
updateWorkerResponse_httpStatus = Lens.lens (\UpdateWorkerResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkerResponse' {} a -> s {httpStatus = a} :: UpdateWorkerResponse)

-- | Undocumented member.
updateWorkerResponse_arn :: Lens.Lens' UpdateWorkerResponse Prelude.Text
updateWorkerResponse_arn = Lens.lens (\UpdateWorkerResponse' {arn} -> arn) (\s@UpdateWorkerResponse' {} a -> s {arn = a} :: UpdateWorkerResponse)

-- | Undocumented member.
updateWorkerResponse_id :: Lens.Lens' UpdateWorkerResponse Prelude.Text
updateWorkerResponse_id = Lens.lens (\UpdateWorkerResponse' {id} -> id) (\s@UpdateWorkerResponse' {} a -> s {id = a} :: UpdateWorkerResponse)

-- | Undocumented member.
updateWorkerResponse_fleet :: Lens.Lens' UpdateWorkerResponse Prelude.Text
updateWorkerResponse_fleet = Lens.lens (\UpdateWorkerResponse' {fleet} -> fleet) (\s@UpdateWorkerResponse' {} a -> s {fleet = a} :: UpdateWorkerResponse)

-- | Undocumented member.
updateWorkerResponse_updatedAt :: Lens.Lens' UpdateWorkerResponse Prelude.UTCTime
updateWorkerResponse_updatedAt = Lens.lens (\UpdateWorkerResponse' {updatedAt} -> updatedAt) (\s@UpdateWorkerResponse' {} a -> s {updatedAt = a} :: UpdateWorkerResponse) Prelude.. Data._Time

-- | Undocumented member.
updateWorkerResponse_name :: Lens.Lens' UpdateWorkerResponse Prelude.Text
updateWorkerResponse_name = Lens.lens (\UpdateWorkerResponse' {name} -> name) (\s@UpdateWorkerResponse' {} a -> s {name = a} :: UpdateWorkerResponse)

instance Prelude.NFData UpdateWorkerResponse where
  rnf UpdateWorkerResponse' {..} =
    Prelude.rnf additionalTransientProperties
      `Prelude.seq` Prelude.rnf orientation
      `Prelude.seq` Prelude.rnf vendorProperties
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf fleet
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf name
