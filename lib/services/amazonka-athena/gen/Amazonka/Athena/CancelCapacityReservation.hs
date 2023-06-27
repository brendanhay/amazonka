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
-- Module      : Amazonka.Athena.CancelCapacityReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the capacity reservation with the specified name. Cancelled
-- reservations remain in your account and will be deleted 45 days after
-- cancellation. During the 45 days, you cannot re-purpose or reuse a
-- reservation that has been cancelled, but you can refer to its tags and
-- view it for historical reference.
module Amazonka.Athena.CancelCapacityReservation
  ( -- * Creating a Request
    CancelCapacityReservation (..),
    newCancelCapacityReservation,

    -- * Request Lenses
    cancelCapacityReservation_name,

    -- * Destructuring the Response
    CancelCapacityReservationResponse (..),
    newCancelCapacityReservationResponse,

    -- * Response Lenses
    cancelCapacityReservationResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelCapacityReservation' smart constructor.
data CancelCapacityReservation = CancelCapacityReservation'
  { -- | The name of the capacity reservation to cancel.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'cancelCapacityReservation_name' - The name of the capacity reservation to cancel.
newCancelCapacityReservation ::
  -- | 'name'
  Prelude.Text ->
  CancelCapacityReservation
newCancelCapacityReservation pName_ =
  CancelCapacityReservation' {name = pName_}

-- | The name of the capacity reservation to cancel.
cancelCapacityReservation_name :: Lens.Lens' CancelCapacityReservation Prelude.Text
cancelCapacityReservation_name = Lens.lens (\CancelCapacityReservation' {name} -> name) (\s@CancelCapacityReservation' {} a -> s {name = a} :: CancelCapacityReservation)

instance Core.AWSRequest CancelCapacityReservation where
  type
    AWSResponse CancelCapacityReservation =
      CancelCapacityReservationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelCapacityReservationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelCapacityReservation where
  hashWithSalt _salt CancelCapacityReservation' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData CancelCapacityReservation where
  rnf CancelCapacityReservation' {..} = Prelude.rnf name

instance Data.ToHeaders CancelCapacityReservation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.CancelCapacityReservation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelCapacityReservation where
  toJSON CancelCapacityReservation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath CancelCapacityReservation where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelCapacityReservation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelCapacityReservationResponse' smart constructor.
data CancelCapacityReservationResponse = CancelCapacityReservationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelCapacityReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelCapacityReservationResponse_httpStatus' - The response's http status code.
newCancelCapacityReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelCapacityReservationResponse
newCancelCapacityReservationResponse pHttpStatus_ =
  CancelCapacityReservationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelCapacityReservationResponse_httpStatus :: Lens.Lens' CancelCapacityReservationResponse Prelude.Int
cancelCapacityReservationResponse_httpStatus = Lens.lens (\CancelCapacityReservationResponse' {httpStatus} -> httpStatus) (\s@CancelCapacityReservationResponse' {} a -> s {httpStatus = a} :: CancelCapacityReservationResponse)

instance
  Prelude.NFData
    CancelCapacityReservationResponse
  where
  rnf CancelCapacityReservationResponse' {..} =
    Prelude.rnf httpStatus
