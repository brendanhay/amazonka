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
-- Module      : Amazonka.Athena.DeleteCapacityReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cancelled capacity reservation. A reservation must be
-- cancelled before it can be deleted. A deleted reservation is immediately
-- removed from your account and can no longer be referenced, including by
-- its ARN. A deleted reservation cannot be called by
-- @GetCapacityReservation@, and deleted reservations do not appear in the
-- output of @ListCapacityReservations@.
module Amazonka.Athena.DeleteCapacityReservation
  ( -- * Creating a Request
    DeleteCapacityReservation (..),
    newDeleteCapacityReservation,

    -- * Request Lenses
    deleteCapacityReservation_name,

    -- * Destructuring the Response
    DeleteCapacityReservationResponse (..),
    newDeleteCapacityReservationResponse,

    -- * Response Lenses
    deleteCapacityReservationResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCapacityReservation' smart constructor.
data DeleteCapacityReservation = DeleteCapacityReservation'
  { -- | The name of the capacity reservation to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteCapacityReservation_name' - The name of the capacity reservation to delete.
newDeleteCapacityReservation ::
  -- | 'name'
  Prelude.Text ->
  DeleteCapacityReservation
newDeleteCapacityReservation pName_ =
  DeleteCapacityReservation' {name = pName_}

-- | The name of the capacity reservation to delete.
deleteCapacityReservation_name :: Lens.Lens' DeleteCapacityReservation Prelude.Text
deleteCapacityReservation_name = Lens.lens (\DeleteCapacityReservation' {name} -> name) (\s@DeleteCapacityReservation' {} a -> s {name = a} :: DeleteCapacityReservation)

instance Core.AWSRequest DeleteCapacityReservation where
  type
    AWSResponse DeleteCapacityReservation =
      DeleteCapacityReservationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCapacityReservationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCapacityReservation where
  hashWithSalt _salt DeleteCapacityReservation' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteCapacityReservation where
  rnf DeleteCapacityReservation' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteCapacityReservation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.DeleteCapacityReservation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCapacityReservation where
  toJSON DeleteCapacityReservation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteCapacityReservation where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCapacityReservation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCapacityReservationResponse' smart constructor.
data DeleteCapacityReservationResponse = DeleteCapacityReservationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCapacityReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCapacityReservationResponse_httpStatus' - The response's http status code.
newDeleteCapacityReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCapacityReservationResponse
newDeleteCapacityReservationResponse pHttpStatus_ =
  DeleteCapacityReservationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCapacityReservationResponse_httpStatus :: Lens.Lens' DeleteCapacityReservationResponse Prelude.Int
deleteCapacityReservationResponse_httpStatus = Lens.lens (\DeleteCapacityReservationResponse' {httpStatus} -> httpStatus) (\s@DeleteCapacityReservationResponse' {} a -> s {httpStatus = a} :: DeleteCapacityReservationResponse)

instance
  Prelude.NFData
    DeleteCapacityReservationResponse
  where
  rnf DeleteCapacityReservationResponse' {..} =
    Prelude.rnf httpStatus
