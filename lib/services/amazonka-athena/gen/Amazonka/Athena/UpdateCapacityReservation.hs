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
-- Module      : Amazonka.Athena.UpdateCapacityReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the number of requested data processing units for the capacity
-- reservation with the specified name.
module Amazonka.Athena.UpdateCapacityReservation
  ( -- * Creating a Request
    UpdateCapacityReservation (..),
    newUpdateCapacityReservation,

    -- * Request Lenses
    updateCapacityReservation_targetDpus,
    updateCapacityReservation_name,

    -- * Destructuring the Response
    UpdateCapacityReservationResponse (..),
    newUpdateCapacityReservationResponse,

    -- * Response Lenses
    updateCapacityReservationResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCapacityReservation' smart constructor.
data UpdateCapacityReservation = UpdateCapacityReservation'
  { -- | The new number of requested data processing units.
    targetDpus :: Prelude.Natural,
    -- | The name of the capacity reservation.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDpus', 'updateCapacityReservation_targetDpus' - The new number of requested data processing units.
--
-- 'name', 'updateCapacityReservation_name' - The name of the capacity reservation.
newUpdateCapacityReservation ::
  -- | 'targetDpus'
  Prelude.Natural ->
  -- | 'name'
  Prelude.Text ->
  UpdateCapacityReservation
newUpdateCapacityReservation pTargetDpus_ pName_ =
  UpdateCapacityReservation'
    { targetDpus =
        pTargetDpus_,
      name = pName_
    }

-- | The new number of requested data processing units.
updateCapacityReservation_targetDpus :: Lens.Lens' UpdateCapacityReservation Prelude.Natural
updateCapacityReservation_targetDpus = Lens.lens (\UpdateCapacityReservation' {targetDpus} -> targetDpus) (\s@UpdateCapacityReservation' {} a -> s {targetDpus = a} :: UpdateCapacityReservation)

-- | The name of the capacity reservation.
updateCapacityReservation_name :: Lens.Lens' UpdateCapacityReservation Prelude.Text
updateCapacityReservation_name = Lens.lens (\UpdateCapacityReservation' {name} -> name) (\s@UpdateCapacityReservation' {} a -> s {name = a} :: UpdateCapacityReservation)

instance Core.AWSRequest UpdateCapacityReservation where
  type
    AWSResponse UpdateCapacityReservation =
      UpdateCapacityReservationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCapacityReservationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCapacityReservation where
  hashWithSalt _salt UpdateCapacityReservation' {..} =
    _salt
      `Prelude.hashWithSalt` targetDpus
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateCapacityReservation where
  rnf UpdateCapacityReservation' {..} =
    Prelude.rnf targetDpus
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateCapacityReservation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.UpdateCapacityReservation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCapacityReservation where
  toJSON UpdateCapacityReservation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TargetDpus" Data..= targetDpus),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateCapacityReservation where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCapacityReservation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCapacityReservationResponse' smart constructor.
data UpdateCapacityReservationResponse = UpdateCapacityReservationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCapacityReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCapacityReservationResponse_httpStatus' - The response's http status code.
newUpdateCapacityReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCapacityReservationResponse
newUpdateCapacityReservationResponse pHttpStatus_ =
  UpdateCapacityReservationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateCapacityReservationResponse_httpStatus :: Lens.Lens' UpdateCapacityReservationResponse Prelude.Int
updateCapacityReservationResponse_httpStatus = Lens.lens (\UpdateCapacityReservationResponse' {httpStatus} -> httpStatus) (\s@UpdateCapacityReservationResponse' {} a -> s {httpStatus = a} :: UpdateCapacityReservationResponse)

instance
  Prelude.NFData
    UpdateCapacityReservationResponse
  where
  rnf UpdateCapacityReservationResponse' {..} =
    Prelude.rnf httpStatus
