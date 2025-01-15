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
-- Module      : Amazonka.DataBrew.DeleteSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified DataBrew schedule.
module Amazonka.DataBrew.DeleteSchedule
  ( -- * Creating a Request
    DeleteSchedule (..),
    newDeleteSchedule,

    -- * Request Lenses
    deleteSchedule_name,

    -- * Destructuring the Response
    DeleteScheduleResponse (..),
    newDeleteScheduleResponse,

    -- * Response Lenses
    deleteScheduleResponse_httpStatus,
    deleteScheduleResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSchedule' smart constructor.
data DeleteSchedule = DeleteSchedule'
  { -- | The name of the schedule to be deleted.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteSchedule_name' - The name of the schedule to be deleted.
newDeleteSchedule ::
  -- | 'name'
  Prelude.Text ->
  DeleteSchedule
newDeleteSchedule pName_ =
  DeleteSchedule' {name = pName_}

-- | The name of the schedule to be deleted.
deleteSchedule_name :: Lens.Lens' DeleteSchedule Prelude.Text
deleteSchedule_name = Lens.lens (\DeleteSchedule' {name} -> name) (\s@DeleteSchedule' {} a -> s {name = a} :: DeleteSchedule)

instance Core.AWSRequest DeleteSchedule where
  type
    AWSResponse DeleteSchedule =
      DeleteScheduleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable DeleteSchedule where
  hashWithSalt _salt DeleteSchedule' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteSchedule where
  rnf DeleteSchedule' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSchedule where
  toPath DeleteSchedule' {..} =
    Prelude.mconcat ["/schedules/", Data.toBS name]

instance Data.ToQuery DeleteSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteScheduleResponse' smart constructor.
data DeleteScheduleResponse = DeleteScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the schedule that was deleted.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteScheduleResponse_httpStatus' - The response's http status code.
--
-- 'name', 'deleteScheduleResponse_name' - The name of the schedule that was deleted.
newDeleteScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  DeleteScheduleResponse
newDeleteScheduleResponse pHttpStatus_ pName_ =
  DeleteScheduleResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
deleteScheduleResponse_httpStatus :: Lens.Lens' DeleteScheduleResponse Prelude.Int
deleteScheduleResponse_httpStatus = Lens.lens (\DeleteScheduleResponse' {httpStatus} -> httpStatus) (\s@DeleteScheduleResponse' {} a -> s {httpStatus = a} :: DeleteScheduleResponse)

-- | The name of the schedule that was deleted.
deleteScheduleResponse_name :: Lens.Lens' DeleteScheduleResponse Prelude.Text
deleteScheduleResponse_name = Lens.lens (\DeleteScheduleResponse' {name} -> name) (\s@DeleteScheduleResponse' {} a -> s {name = a} :: DeleteScheduleResponse)

instance Prelude.NFData DeleteScheduleResponse where
  rnf DeleteScheduleResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf name
