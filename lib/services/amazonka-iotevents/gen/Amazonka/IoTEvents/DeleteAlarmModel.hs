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
-- Module      : Amazonka.IoTEvents.DeleteAlarmModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alarm model. Any alarm instances that were created based on
-- this alarm model are also deleted. This action can\'t be undone.
module Amazonka.IoTEvents.DeleteAlarmModel
  ( -- * Creating a Request
    DeleteAlarmModel (..),
    newDeleteAlarmModel,

    -- * Request Lenses
    deleteAlarmModel_alarmModelName,

    -- * Destructuring the Response
    DeleteAlarmModelResponse (..),
    newDeleteAlarmModelResponse,

    -- * Response Lenses
    deleteAlarmModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAlarmModel' smart constructor.
data DeleteAlarmModel = DeleteAlarmModel'
  { -- | The name of the alarm model.
    alarmModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlarmModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmModelName', 'deleteAlarmModel_alarmModelName' - The name of the alarm model.
newDeleteAlarmModel ::
  -- | 'alarmModelName'
  Prelude.Text ->
  DeleteAlarmModel
newDeleteAlarmModel pAlarmModelName_ =
  DeleteAlarmModel'
    { alarmModelName =
        pAlarmModelName_
    }

-- | The name of the alarm model.
deleteAlarmModel_alarmModelName :: Lens.Lens' DeleteAlarmModel Prelude.Text
deleteAlarmModel_alarmModelName = Lens.lens (\DeleteAlarmModel' {alarmModelName} -> alarmModelName) (\s@DeleteAlarmModel' {} a -> s {alarmModelName = a} :: DeleteAlarmModel)

instance Core.AWSRequest DeleteAlarmModel where
  type
    AWSResponse DeleteAlarmModel =
      DeleteAlarmModelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAlarmModelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAlarmModel where
  hashWithSalt _salt DeleteAlarmModel' {..} =
    _salt `Prelude.hashWithSalt` alarmModelName

instance Prelude.NFData DeleteAlarmModel where
  rnf DeleteAlarmModel' {..} =
    Prelude.rnf alarmModelName

instance Core.ToHeaders DeleteAlarmModel where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteAlarmModel where
  toPath DeleteAlarmModel' {..} =
    Prelude.mconcat
      ["/alarm-models/", Core.toBS alarmModelName]

instance Core.ToQuery DeleteAlarmModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAlarmModelResponse' smart constructor.
data DeleteAlarmModelResponse = DeleteAlarmModelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlarmModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAlarmModelResponse_httpStatus' - The response's http status code.
newDeleteAlarmModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAlarmModelResponse
newDeleteAlarmModelResponse pHttpStatus_ =
  DeleteAlarmModelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAlarmModelResponse_httpStatus :: Lens.Lens' DeleteAlarmModelResponse Prelude.Int
deleteAlarmModelResponse_httpStatus = Lens.lens (\DeleteAlarmModelResponse' {httpStatus} -> httpStatus) (\s@DeleteAlarmModelResponse' {} a -> s {httpStatus = a} :: DeleteAlarmModelResponse)

instance Prelude.NFData DeleteAlarmModelResponse where
  rnf DeleteAlarmModelResponse' {..} =
    Prelude.rnf httpStatus
