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
-- Module      : Amazonka.Athena.GetCapacityReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the capacity reservation with the specified
-- name.
module Amazonka.Athena.GetCapacityReservation
  ( -- * Creating a Request
    GetCapacityReservation (..),
    newGetCapacityReservation,

    -- * Request Lenses
    getCapacityReservation_name,

    -- * Destructuring the Response
    GetCapacityReservationResponse (..),
    newGetCapacityReservationResponse,

    -- * Response Lenses
    getCapacityReservationResponse_httpStatus,
    getCapacityReservationResponse_capacityReservation,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCapacityReservation' smart constructor.
data GetCapacityReservation = GetCapacityReservation'
  { -- | The name of the capacity reservation.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getCapacityReservation_name' - The name of the capacity reservation.
newGetCapacityReservation ::
  -- | 'name'
  Prelude.Text ->
  GetCapacityReservation
newGetCapacityReservation pName_ =
  GetCapacityReservation' {name = pName_}

-- | The name of the capacity reservation.
getCapacityReservation_name :: Lens.Lens' GetCapacityReservation Prelude.Text
getCapacityReservation_name = Lens.lens (\GetCapacityReservation' {name} -> name) (\s@GetCapacityReservation' {} a -> s {name = a} :: GetCapacityReservation)

instance Core.AWSRequest GetCapacityReservation where
  type
    AWSResponse GetCapacityReservation =
      GetCapacityReservationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCapacityReservationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CapacityReservation")
      )

instance Prelude.Hashable GetCapacityReservation where
  hashWithSalt _salt GetCapacityReservation' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetCapacityReservation where
  rnf GetCapacityReservation' {..} = Prelude.rnf name

instance Data.ToHeaders GetCapacityReservation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetCapacityReservation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCapacityReservation where
  toJSON GetCapacityReservation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath GetCapacityReservation where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCapacityReservation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCapacityReservationResponse' smart constructor.
data GetCapacityReservationResponse = GetCapacityReservationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The requested capacity reservation structure.
    capacityReservation :: CapacityReservation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCapacityReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCapacityReservationResponse_httpStatus' - The response's http status code.
--
-- 'capacityReservation', 'getCapacityReservationResponse_capacityReservation' - The requested capacity reservation structure.
newGetCapacityReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'capacityReservation'
  CapacityReservation ->
  GetCapacityReservationResponse
newGetCapacityReservationResponse
  pHttpStatus_
  pCapacityReservation_ =
    GetCapacityReservationResponse'
      { httpStatus =
          pHttpStatus_,
        capacityReservation = pCapacityReservation_
      }

-- | The response's http status code.
getCapacityReservationResponse_httpStatus :: Lens.Lens' GetCapacityReservationResponse Prelude.Int
getCapacityReservationResponse_httpStatus = Lens.lens (\GetCapacityReservationResponse' {httpStatus} -> httpStatus) (\s@GetCapacityReservationResponse' {} a -> s {httpStatus = a} :: GetCapacityReservationResponse)

-- | The requested capacity reservation structure.
getCapacityReservationResponse_capacityReservation :: Lens.Lens' GetCapacityReservationResponse CapacityReservation
getCapacityReservationResponse_capacityReservation = Lens.lens (\GetCapacityReservationResponse' {capacityReservation} -> capacityReservation) (\s@GetCapacityReservationResponse' {} a -> s {capacityReservation = a} :: GetCapacityReservationResponse)

instance
  Prelude.NFData
    GetCapacityReservationResponse
  where
  rnf GetCapacityReservationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf capacityReservation
