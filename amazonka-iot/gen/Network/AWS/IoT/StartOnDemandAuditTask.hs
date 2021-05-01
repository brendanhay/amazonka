{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.StartOnDemandAuditTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand Device Defender audit.
module Network.AWS.IoT.StartOnDemandAuditTask
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartOnDemandAuditTask' smart constructor.
data StartOnDemandAuditTask = StartOnDemandAuditTask'
  { -- | Which checks are performed during the audit. The checks you specify must
    -- be enabled for your account or an exception occurs. Use
    -- @DescribeAccountAuditConfiguration@ to see the list of all checks,
    -- including those that are enabled or @UpdateAccountAuditConfiguration@ to
    -- select which checks are enabled.
    targetCheckNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
startOnDemandAuditTask_targetCheckNames = Lens.lens (\StartOnDemandAuditTask' {targetCheckNames} -> targetCheckNames) (\s@StartOnDemandAuditTask' {} a -> s {targetCheckNames = a} :: StartOnDemandAuditTask) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest StartOnDemandAuditTask where
  type
    Rs StartOnDemandAuditTask =
      StartOnDemandAuditTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartOnDemandAuditTaskResponse'
            Prelude.<$> (x Prelude..?> "taskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartOnDemandAuditTask

instance Prelude.NFData StartOnDemandAuditTask

instance Prelude.ToHeaders StartOnDemandAuditTask where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON StartOnDemandAuditTask where
  toJSON StartOnDemandAuditTask' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("targetCheckNames" Prelude..= targetCheckNames)
          ]
      )

instance Prelude.ToPath StartOnDemandAuditTask where
  toPath = Prelude.const "/audit/tasks"

instance Prelude.ToQuery StartOnDemandAuditTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartOnDemandAuditTaskResponse' smart constructor.
data StartOnDemandAuditTaskResponse = StartOnDemandAuditTaskResponse'
  { -- | The ID of the on-demand audit you started.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
