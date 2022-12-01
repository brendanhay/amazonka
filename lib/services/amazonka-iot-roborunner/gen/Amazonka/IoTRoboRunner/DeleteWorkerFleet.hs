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
-- Module      : Amazonka.IoTRoboRunner.DeleteWorkerFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to delete a worker fleet
module Amazonka.IoTRoboRunner.DeleteWorkerFleet
  ( -- * Creating a Request
    DeleteWorkerFleet (..),
    newDeleteWorkerFleet,

    -- * Request Lenses
    deleteWorkerFleet_id,

    -- * Destructuring the Response
    DeleteWorkerFleetResponse (..),
    newDeleteWorkerFleetResponse,

    -- * Response Lenses
    deleteWorkerFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWorkerFleet' smart constructor.
data DeleteWorkerFleet = DeleteWorkerFleet'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkerFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteWorkerFleet_id' - Undocumented member.
newDeleteWorkerFleet ::
  -- | 'id'
  Prelude.Text ->
  DeleteWorkerFleet
newDeleteWorkerFleet pId_ =
  DeleteWorkerFleet' {id = pId_}

-- | Undocumented member.
deleteWorkerFleet_id :: Lens.Lens' DeleteWorkerFleet Prelude.Text
deleteWorkerFleet_id = Lens.lens (\DeleteWorkerFleet' {id} -> id) (\s@DeleteWorkerFleet' {} a -> s {id = a} :: DeleteWorkerFleet)

instance Core.AWSRequest DeleteWorkerFleet where
  type
    AWSResponse DeleteWorkerFleet =
      DeleteWorkerFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkerFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkerFleet where
  hashWithSalt _salt DeleteWorkerFleet' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteWorkerFleet where
  rnf DeleteWorkerFleet' {..} = Prelude.rnf id

instance Core.ToHeaders DeleteWorkerFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteWorkerFleet where
  toJSON DeleteWorkerFleet' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance Core.ToPath DeleteWorkerFleet where
  toPath = Prelude.const "/deleteWorkerFleet"

instance Core.ToQuery DeleteWorkerFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkerFleetResponse' smart constructor.
data DeleteWorkerFleetResponse = DeleteWorkerFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkerFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkerFleetResponse_httpStatus' - The response's http status code.
newDeleteWorkerFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorkerFleetResponse
newDeleteWorkerFleetResponse pHttpStatus_ =
  DeleteWorkerFleetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteWorkerFleetResponse_httpStatus :: Lens.Lens' DeleteWorkerFleetResponse Prelude.Int
deleteWorkerFleetResponse_httpStatus = Lens.lens (\DeleteWorkerFleetResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkerFleetResponse' {} a -> s {httpStatus = a} :: DeleteWorkerFleetResponse)

instance Prelude.NFData DeleteWorkerFleetResponse where
  rnf DeleteWorkerFleetResponse' {..} =
    Prelude.rnf httpStatus
