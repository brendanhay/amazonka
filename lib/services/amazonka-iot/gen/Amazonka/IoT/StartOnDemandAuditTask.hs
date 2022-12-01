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
-- Module      : Amazonka.IoT.StartOnDemandAuditTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand Device Defender audit.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions StartOnDemandAuditTask>
-- action.
module Amazonka.IoT.StartOnDemandAuditTask
  ( -- * Creating a Request
    StartOnDemandAuditTask (..),
    newStartOnDemandAuditTask,

    -- * Request Lenses
    startOnDemandAuditTask_targetCheckNames,

    -- * Destructuring the Response
    StartOnDemandAuditTaskResponse (..),
    newStartOnDemandAuditTaskResponse,

    -- * Response Lenses
    startOnDemandAuditTaskResponse_taskId,
    startOnDemandAuditTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartOnDemandAuditTask' smart constructor.
data StartOnDemandAuditTask = StartOnDemandAuditTask'
  { -- | Which checks are performed during the audit. The checks you specify must
    -- be enabled for your account or an exception occurs. Use
    -- @DescribeAccountAuditConfiguration@ to see the list of all checks,
    -- including those that are enabled or @UpdateAccountAuditConfiguration@ to
    -- select which checks are enabled.
    targetCheckNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartOnDemandAuditTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetCheckNames', 'startOnDemandAuditTask_targetCheckNames' - Which checks are performed during the audit. The checks you specify must
-- be enabled for your account or an exception occurs. Use
-- @DescribeAccountAuditConfiguration@ to see the list of all checks,
-- including those that are enabled or @UpdateAccountAuditConfiguration@ to
-- select which checks are enabled.
newStartOnDemandAuditTask ::
  StartOnDemandAuditTask
newStartOnDemandAuditTask =
  StartOnDemandAuditTask'
    { targetCheckNames =
        Prelude.mempty
    }

-- | Which checks are performed during the audit. The checks you specify must
-- be enabled for your account or an exception occurs. Use
-- @DescribeAccountAuditConfiguration@ to see the list of all checks,
-- including those that are enabled or @UpdateAccountAuditConfiguration@ to
-- select which checks are enabled.
startOnDemandAuditTask_targetCheckNames :: Lens.Lens' StartOnDemandAuditTask [Prelude.Text]
startOnDemandAuditTask_targetCheckNames = Lens.lens (\StartOnDemandAuditTask' {targetCheckNames} -> targetCheckNames) (\s@StartOnDemandAuditTask' {} a -> s {targetCheckNames = a} :: StartOnDemandAuditTask) Prelude.. Lens.coerced

instance Core.AWSRequest StartOnDemandAuditTask where
  type
    AWSResponse StartOnDemandAuditTask =
      StartOnDemandAuditTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartOnDemandAuditTaskResponse'
            Prelude.<$> (x Core..?> "taskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartOnDemandAuditTask where
  hashWithSalt _salt StartOnDemandAuditTask' {..} =
    _salt `Prelude.hashWithSalt` targetCheckNames

instance Prelude.NFData StartOnDemandAuditTask where
  rnf StartOnDemandAuditTask' {..} =
    Prelude.rnf targetCheckNames

instance Core.ToHeaders StartOnDemandAuditTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON StartOnDemandAuditTask where
  toJSON StartOnDemandAuditTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("targetCheckNames" Core..= targetCheckNames)
          ]
      )

instance Core.ToPath StartOnDemandAuditTask where
  toPath = Prelude.const "/audit/tasks"

instance Core.ToQuery StartOnDemandAuditTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartOnDemandAuditTaskResponse' smart constructor.
data StartOnDemandAuditTaskResponse = StartOnDemandAuditTaskResponse'
  { -- | The ID of the on-demand audit you started.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartOnDemandAuditTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'startOnDemandAuditTaskResponse_taskId' - The ID of the on-demand audit you started.
--
-- 'httpStatus', 'startOnDemandAuditTaskResponse_httpStatus' - The response's http status code.
newStartOnDemandAuditTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartOnDemandAuditTaskResponse
newStartOnDemandAuditTaskResponse pHttpStatus_ =
  StartOnDemandAuditTaskResponse'
    { taskId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the on-demand audit you started.
startOnDemandAuditTaskResponse_taskId :: Lens.Lens' StartOnDemandAuditTaskResponse (Prelude.Maybe Prelude.Text)
startOnDemandAuditTaskResponse_taskId = Lens.lens (\StartOnDemandAuditTaskResponse' {taskId} -> taskId) (\s@StartOnDemandAuditTaskResponse' {} a -> s {taskId = a} :: StartOnDemandAuditTaskResponse)

-- | The response's http status code.
startOnDemandAuditTaskResponse_httpStatus :: Lens.Lens' StartOnDemandAuditTaskResponse Prelude.Int
startOnDemandAuditTaskResponse_httpStatus = Lens.lens (\StartOnDemandAuditTaskResponse' {httpStatus} -> httpStatus) (\s@StartOnDemandAuditTaskResponse' {} a -> s {httpStatus = a} :: StartOnDemandAuditTaskResponse)

instance
  Prelude.NFData
    StartOnDemandAuditTaskResponse
  where
  rnf StartOnDemandAuditTaskResponse' {..} =
    Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf httpStatus
