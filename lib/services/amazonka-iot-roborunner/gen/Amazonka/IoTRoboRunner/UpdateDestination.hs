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
-- Module      : Amazonka.IoTRoboRunner.UpdateDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to update a destination
module Amazonka.IoTRoboRunner.UpdateDestination
  ( -- * Creating a Request
    UpdateDestination (..),
    newUpdateDestination,

    -- * Request Lenses
    updateDestination_additionalFixedProperties,
    updateDestination_name,
    updateDestination_state,
    updateDestination_id,

    -- * Destructuring the Response
    UpdateDestinationResponse (..),
    newUpdateDestinationResponse,

    -- * Response Lenses
    updateDestinationResponse_additionalFixedProperties,
    updateDestinationResponse_httpStatus,
    updateDestinationResponse_arn,
    updateDestinationResponse_id,
    updateDestinationResponse_name,
    updateDestinationResponse_updatedAt,
    updateDestinationResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDestination' smart constructor.
data UpdateDestination = UpdateDestination'
  { additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe DestinationState,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalFixedProperties', 'updateDestination_additionalFixedProperties' - Undocumented member.
--
-- 'name', 'updateDestination_name' - Undocumented member.
--
-- 'state', 'updateDestination_state' - Undocumented member.
--
-- 'id', 'updateDestination_id' - Undocumented member.
newUpdateDestination ::
  -- | 'id'
  Prelude.Text ->
  UpdateDestination
newUpdateDestination pId_ =
  UpdateDestination'
    { additionalFixedProperties =
        Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
updateDestination_additionalFixedProperties :: Lens.Lens' UpdateDestination (Prelude.Maybe Prelude.Text)
updateDestination_additionalFixedProperties = Lens.lens (\UpdateDestination' {additionalFixedProperties} -> additionalFixedProperties) (\s@UpdateDestination' {} a -> s {additionalFixedProperties = a} :: UpdateDestination)

-- | Undocumented member.
updateDestination_name :: Lens.Lens' UpdateDestination (Prelude.Maybe Prelude.Text)
updateDestination_name = Lens.lens (\UpdateDestination' {name} -> name) (\s@UpdateDestination' {} a -> s {name = a} :: UpdateDestination)

-- | Undocumented member.
updateDestination_state :: Lens.Lens' UpdateDestination (Prelude.Maybe DestinationState)
updateDestination_state = Lens.lens (\UpdateDestination' {state} -> state) (\s@UpdateDestination' {} a -> s {state = a} :: UpdateDestination)

-- | Undocumented member.
updateDestination_id :: Lens.Lens' UpdateDestination Prelude.Text
updateDestination_id = Lens.lens (\UpdateDestination' {id} -> id) (\s@UpdateDestination' {} a -> s {id = a} :: UpdateDestination)

instance Core.AWSRequest UpdateDestination where
  type
    AWSResponse UpdateDestination =
      UpdateDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDestinationResponse'
            Prelude.<$> (x Data..?> "additionalFixedProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "updatedAt")
            Prelude.<*> (x Data..:> "state")
      )

instance Prelude.Hashable UpdateDestination where
  hashWithSalt _salt UpdateDestination' {..} =
    _salt
      `Prelude.hashWithSalt` additionalFixedProperties
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateDestination where
  rnf UpdateDestination' {..} =
    Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDestination where
  toJSON UpdateDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalFixedProperties" Data..=)
              Prelude.<$> additionalFixedProperties,
            ("name" Data..=) Prelude.<$> name,
            ("state" Data..=) Prelude.<$> state,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath UpdateDestination where
  toPath = Prelude.const "/updateDestination"

instance Data.ToQuery UpdateDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDestinationResponse' smart constructor.
data UpdateDestinationResponse = UpdateDestinationResponse'
  { additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    name :: Prelude.Text,
    updatedAt :: Data.POSIX,
    state :: DestinationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalFixedProperties', 'updateDestinationResponse_additionalFixedProperties' - Undocumented member.
--
-- 'httpStatus', 'updateDestinationResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'updateDestinationResponse_arn' - Undocumented member.
--
-- 'id', 'updateDestinationResponse_id' - Undocumented member.
--
-- 'name', 'updateDestinationResponse_name' - Undocumented member.
--
-- 'updatedAt', 'updateDestinationResponse_updatedAt' - Undocumented member.
--
-- 'state', 'updateDestinationResponse_state' - Undocumented member.
newUpdateDestinationResponse ::
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
  -- | 'state'
  DestinationState ->
  UpdateDestinationResponse
newUpdateDestinationResponse
  pHttpStatus_
  pArn_
  pId_
  pName_
  pUpdatedAt_
  pState_ =
    UpdateDestinationResponse'
      { additionalFixedProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        name = pName_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        state = pState_
      }

-- | Undocumented member.
updateDestinationResponse_additionalFixedProperties :: Lens.Lens' UpdateDestinationResponse (Prelude.Maybe Prelude.Text)
updateDestinationResponse_additionalFixedProperties = Lens.lens (\UpdateDestinationResponse' {additionalFixedProperties} -> additionalFixedProperties) (\s@UpdateDestinationResponse' {} a -> s {additionalFixedProperties = a} :: UpdateDestinationResponse)

-- | The response's http status code.
updateDestinationResponse_httpStatus :: Lens.Lens' UpdateDestinationResponse Prelude.Int
updateDestinationResponse_httpStatus = Lens.lens (\UpdateDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdateDestinationResponse' {} a -> s {httpStatus = a} :: UpdateDestinationResponse)

-- | Undocumented member.
updateDestinationResponse_arn :: Lens.Lens' UpdateDestinationResponse Prelude.Text
updateDestinationResponse_arn = Lens.lens (\UpdateDestinationResponse' {arn} -> arn) (\s@UpdateDestinationResponse' {} a -> s {arn = a} :: UpdateDestinationResponse)

-- | Undocumented member.
updateDestinationResponse_id :: Lens.Lens' UpdateDestinationResponse Prelude.Text
updateDestinationResponse_id = Lens.lens (\UpdateDestinationResponse' {id} -> id) (\s@UpdateDestinationResponse' {} a -> s {id = a} :: UpdateDestinationResponse)

-- | Undocumented member.
updateDestinationResponse_name :: Lens.Lens' UpdateDestinationResponse Prelude.Text
updateDestinationResponse_name = Lens.lens (\UpdateDestinationResponse' {name} -> name) (\s@UpdateDestinationResponse' {} a -> s {name = a} :: UpdateDestinationResponse)

-- | Undocumented member.
updateDestinationResponse_updatedAt :: Lens.Lens' UpdateDestinationResponse Prelude.UTCTime
updateDestinationResponse_updatedAt = Lens.lens (\UpdateDestinationResponse' {updatedAt} -> updatedAt) (\s@UpdateDestinationResponse' {} a -> s {updatedAt = a} :: UpdateDestinationResponse) Prelude.. Data._Time

-- | Undocumented member.
updateDestinationResponse_state :: Lens.Lens' UpdateDestinationResponse DestinationState
updateDestinationResponse_state = Lens.lens (\UpdateDestinationResponse' {state} -> state) (\s@UpdateDestinationResponse' {} a -> s {state = a} :: UpdateDestinationResponse)

instance Prelude.NFData UpdateDestinationResponse where
  rnf UpdateDestinationResponse' {..} =
    Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf state
