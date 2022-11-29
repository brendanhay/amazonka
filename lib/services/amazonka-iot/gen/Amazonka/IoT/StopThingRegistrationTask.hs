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
-- Module      : Amazonka.IoT.StopThingRegistrationTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bulk thing provisioning task.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions StopThingRegistrationTask>
-- action.
module Amazonka.IoT.StopThingRegistrationTask
  ( -- * Creating a Request
    StopThingRegistrationTask (..),
    newStopThingRegistrationTask,

    -- * Request Lenses
    stopThingRegistrationTask_taskId,

    -- * Destructuring the Response
    StopThingRegistrationTaskResponse (..),
    newStopThingRegistrationTaskResponse,

    -- * Response Lenses
    stopThingRegistrationTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopThingRegistrationTask' smart constructor.
data StopThingRegistrationTask = StopThingRegistrationTask'
  { -- | The bulk thing provisioning task ID.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopThingRegistrationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'stopThingRegistrationTask_taskId' - The bulk thing provisioning task ID.
newStopThingRegistrationTask ::
  -- | 'taskId'
  Prelude.Text ->
  StopThingRegistrationTask
newStopThingRegistrationTask pTaskId_ =
  StopThingRegistrationTask' {taskId = pTaskId_}

-- | The bulk thing provisioning task ID.
stopThingRegistrationTask_taskId :: Lens.Lens' StopThingRegistrationTask Prelude.Text
stopThingRegistrationTask_taskId = Lens.lens (\StopThingRegistrationTask' {taskId} -> taskId) (\s@StopThingRegistrationTask' {} a -> s {taskId = a} :: StopThingRegistrationTask)

instance Core.AWSRequest StopThingRegistrationTask where
  type
    AWSResponse StopThingRegistrationTask =
      StopThingRegistrationTaskResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopThingRegistrationTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopThingRegistrationTask where
  hashWithSalt _salt StopThingRegistrationTask' {..} =
    _salt `Prelude.hashWithSalt` taskId

instance Prelude.NFData StopThingRegistrationTask where
  rnf StopThingRegistrationTask' {..} =
    Prelude.rnf taskId

instance Core.ToHeaders StopThingRegistrationTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON StopThingRegistrationTask where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath StopThingRegistrationTask where
  toPath StopThingRegistrationTask' {..} =
    Prelude.mconcat
      [ "/thing-registration-tasks/",
        Core.toBS taskId,
        "/cancel"
      ]

instance Core.ToQuery StopThingRegistrationTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopThingRegistrationTaskResponse' smart constructor.
data StopThingRegistrationTaskResponse = StopThingRegistrationTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopThingRegistrationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopThingRegistrationTaskResponse_httpStatus' - The response's http status code.
newStopThingRegistrationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopThingRegistrationTaskResponse
newStopThingRegistrationTaskResponse pHttpStatus_ =
  StopThingRegistrationTaskResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopThingRegistrationTaskResponse_httpStatus :: Lens.Lens' StopThingRegistrationTaskResponse Prelude.Int
stopThingRegistrationTaskResponse_httpStatus = Lens.lens (\StopThingRegistrationTaskResponse' {httpStatus} -> httpStatus) (\s@StopThingRegistrationTaskResponse' {} a -> s {httpStatus = a} :: StopThingRegistrationTaskResponse)

instance
  Prelude.NFData
    StopThingRegistrationTaskResponse
  where
  rnf StopThingRegistrationTaskResponse' {..} =
    Prelude.rnf httpStatus
