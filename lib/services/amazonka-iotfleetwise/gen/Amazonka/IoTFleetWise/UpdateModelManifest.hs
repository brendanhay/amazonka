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
-- Module      : Amazonka.IoTFleetWise.UpdateModelManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a vehicle model (model manifest). If created vehicles are
-- associated with a vehicle model, it can\'t be updated.
module Amazonka.IoTFleetWise.UpdateModelManifest
  ( -- * Creating a Request
    UpdateModelManifest (..),
    newUpdateModelManifest,

    -- * Request Lenses
    updateModelManifest_description,
    updateModelManifest_nodesToAdd,
    updateModelManifest_nodesToRemove,
    updateModelManifest_status,
    updateModelManifest_name,

    -- * Destructuring the Response
    UpdateModelManifestResponse (..),
    newUpdateModelManifestResponse,

    -- * Response Lenses
    updateModelManifestResponse_httpStatus,
    updateModelManifestResponse_name,
    updateModelManifestResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateModelManifest' smart constructor.
data UpdateModelManifest = UpdateModelManifest'
  { -- | A brief description of the vehicle model.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of @fullyQualifiedName@ of nodes, which are a general abstraction
    -- of signals, to add to the vehicle model.
    nodesToAdd :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of @fullyQualifiedName@ of nodes, which are a general abstraction
    -- of signals, to remove from the vehicle model.
    nodesToRemove :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The state of the vehicle model. If the status is @ACTIVE@, the vehicle
    -- model can\'t be edited. If the status is @DRAFT@, you can edit the
    -- vehicle model.
    status :: Prelude.Maybe ManifestStatus,
    -- | The name of the vehicle model to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateModelManifest_description' - A brief description of the vehicle model.
--
-- 'nodesToAdd', 'updateModelManifest_nodesToAdd' - A list of @fullyQualifiedName@ of nodes, which are a general abstraction
-- of signals, to add to the vehicle model.
--
-- 'nodesToRemove', 'updateModelManifest_nodesToRemove' - A list of @fullyQualifiedName@ of nodes, which are a general abstraction
-- of signals, to remove from the vehicle model.
--
-- 'status', 'updateModelManifest_status' - The state of the vehicle model. If the status is @ACTIVE@, the vehicle
-- model can\'t be edited. If the status is @DRAFT@, you can edit the
-- vehicle model.
--
-- 'name', 'updateModelManifest_name' - The name of the vehicle model to update.
newUpdateModelManifest ::
  -- | 'name'
  Prelude.Text ->
  UpdateModelManifest
newUpdateModelManifest pName_ =
  UpdateModelManifest'
    { description = Prelude.Nothing,
      nodesToAdd = Prelude.Nothing,
      nodesToRemove = Prelude.Nothing,
      status = Prelude.Nothing,
      name = pName_
    }

-- | A brief description of the vehicle model.
updateModelManifest_description :: Lens.Lens' UpdateModelManifest (Prelude.Maybe Prelude.Text)
updateModelManifest_description = Lens.lens (\UpdateModelManifest' {description} -> description) (\s@UpdateModelManifest' {} a -> s {description = a} :: UpdateModelManifest)

-- | A list of @fullyQualifiedName@ of nodes, which are a general abstraction
-- of signals, to add to the vehicle model.
updateModelManifest_nodesToAdd :: Lens.Lens' UpdateModelManifest (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateModelManifest_nodesToAdd = Lens.lens (\UpdateModelManifest' {nodesToAdd} -> nodesToAdd) (\s@UpdateModelManifest' {} a -> s {nodesToAdd = a} :: UpdateModelManifest) Prelude.. Lens.mapping Lens.coerced

-- | A list of @fullyQualifiedName@ of nodes, which are a general abstraction
-- of signals, to remove from the vehicle model.
updateModelManifest_nodesToRemove :: Lens.Lens' UpdateModelManifest (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateModelManifest_nodesToRemove = Lens.lens (\UpdateModelManifest' {nodesToRemove} -> nodesToRemove) (\s@UpdateModelManifest' {} a -> s {nodesToRemove = a} :: UpdateModelManifest) Prelude.. Lens.mapping Lens.coerced

-- | The state of the vehicle model. If the status is @ACTIVE@, the vehicle
-- model can\'t be edited. If the status is @DRAFT@, you can edit the
-- vehicle model.
updateModelManifest_status :: Lens.Lens' UpdateModelManifest (Prelude.Maybe ManifestStatus)
updateModelManifest_status = Lens.lens (\UpdateModelManifest' {status} -> status) (\s@UpdateModelManifest' {} a -> s {status = a} :: UpdateModelManifest)

-- | The name of the vehicle model to update.
updateModelManifest_name :: Lens.Lens' UpdateModelManifest Prelude.Text
updateModelManifest_name = Lens.lens (\UpdateModelManifest' {name} -> name) (\s@UpdateModelManifest' {} a -> s {name = a} :: UpdateModelManifest)

instance Core.AWSRequest UpdateModelManifest where
  type
    AWSResponse UpdateModelManifest =
      UpdateModelManifestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateModelManifestResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable UpdateModelManifest where
  hashWithSalt _salt UpdateModelManifest' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` nodesToAdd
      `Prelude.hashWithSalt` nodesToRemove
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateModelManifest where
  rnf UpdateModelManifest' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf nodesToAdd
      `Prelude.seq` Prelude.rnf nodesToRemove
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateModelManifest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.UpdateModelManifest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateModelManifest where
  toJSON UpdateModelManifest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("nodesToAdd" Data..=) Prelude.<$> nodesToAdd,
            ("nodesToRemove" Data..=) Prelude.<$> nodesToRemove,
            ("status" Data..=) Prelude.<$> status,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath UpdateModelManifest where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateModelManifest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateModelManifestResponse' smart constructor.
data UpdateModelManifestResponse = UpdateModelManifestResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the updated vehicle model.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the updated vehicle model.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelManifestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateModelManifestResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateModelManifestResponse_name' - The name of the updated vehicle model.
--
-- 'arn', 'updateModelManifestResponse_arn' - The Amazon Resource Name (ARN) of the updated vehicle model.
newUpdateModelManifestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  UpdateModelManifestResponse
newUpdateModelManifestResponse
  pHttpStatus_
  pName_
  pArn_ =
    UpdateModelManifestResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
updateModelManifestResponse_httpStatus :: Lens.Lens' UpdateModelManifestResponse Prelude.Int
updateModelManifestResponse_httpStatus = Lens.lens (\UpdateModelManifestResponse' {httpStatus} -> httpStatus) (\s@UpdateModelManifestResponse' {} a -> s {httpStatus = a} :: UpdateModelManifestResponse)

-- | The name of the updated vehicle model.
updateModelManifestResponse_name :: Lens.Lens' UpdateModelManifestResponse Prelude.Text
updateModelManifestResponse_name = Lens.lens (\UpdateModelManifestResponse' {name} -> name) (\s@UpdateModelManifestResponse' {} a -> s {name = a} :: UpdateModelManifestResponse)

-- | The Amazon Resource Name (ARN) of the updated vehicle model.
updateModelManifestResponse_arn :: Lens.Lens' UpdateModelManifestResponse Prelude.Text
updateModelManifestResponse_arn = Lens.lens (\UpdateModelManifestResponse' {arn} -> arn) (\s@UpdateModelManifestResponse' {} a -> s {arn = a} :: UpdateModelManifestResponse)

instance Prelude.NFData UpdateModelManifestResponse where
  rnf UpdateModelManifestResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
