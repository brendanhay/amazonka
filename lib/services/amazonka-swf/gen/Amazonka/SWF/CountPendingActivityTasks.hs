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
-- Module      : Amazonka.SWF.CountPendingActivityTasks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated number of activity tasks in the specified task
-- list. The count returned is an approximation and isn\'t guaranteed to be
-- exact. If you specify a task list that no activity task was ever
-- scheduled in then @0@ is returned.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   Constrain the @taskList.name@ parameter by using a @Condition@
--     element with the @swf:taskList.name@ key to allow the action to
--     access only certain task lists.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Amazonka.SWF.CountPendingActivityTasks
  ( -- * Creating a Request
    CountPendingActivityTasks (..),
    newCountPendingActivityTasks,

    -- * Request Lenses
    countPendingActivityTasks_domain,
    countPendingActivityTasks_taskList,

    -- * Destructuring the Response
    PendingTaskCount (..),
    newPendingTaskCount,

    -- * Response Lenses
    pendingTaskCount_truncated,
    pendingTaskCount_count,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newCountPendingActivityTasks' smart constructor.
data CountPendingActivityTasks = CountPendingActivityTasks'
  { -- | The name of the domain that contains the task list.
    domain :: Prelude.Text,
    -- | The name of the task list.
    taskList :: TaskList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CountPendingActivityTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'countPendingActivityTasks_domain' - The name of the domain that contains the task list.
--
-- 'taskList', 'countPendingActivityTasks_taskList' - The name of the task list.
newCountPendingActivityTasks ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'taskList'
  TaskList ->
  CountPendingActivityTasks
newCountPendingActivityTasks pDomain_ pTaskList_ =
  CountPendingActivityTasks'
    { domain = pDomain_,
      taskList = pTaskList_
    }

-- | The name of the domain that contains the task list.
countPendingActivityTasks_domain :: Lens.Lens' CountPendingActivityTasks Prelude.Text
countPendingActivityTasks_domain = Lens.lens (\CountPendingActivityTasks' {domain} -> domain) (\s@CountPendingActivityTasks' {} a -> s {domain = a} :: CountPendingActivityTasks)

-- | The name of the task list.
countPendingActivityTasks_taskList :: Lens.Lens' CountPendingActivityTasks TaskList
countPendingActivityTasks_taskList = Lens.lens (\CountPendingActivityTasks' {taskList} -> taskList) (\s@CountPendingActivityTasks' {} a -> s {taskList = a} :: CountPendingActivityTasks)

instance Core.AWSRequest CountPendingActivityTasks where
  type
    AWSResponse CountPendingActivityTasks =
      PendingTaskCount
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CountPendingActivityTasks where
  hashWithSalt _salt CountPendingActivityTasks' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` taskList

instance Prelude.NFData CountPendingActivityTasks where
  rnf CountPendingActivityTasks' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf taskList

instance Core.ToHeaders CountPendingActivityTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.CountPendingActivityTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CountPendingActivityTasks where
  toJSON CountPendingActivityTasks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domain" Core..= domain),
            Prelude.Just ("taskList" Core..= taskList)
          ]
      )

instance Core.ToPath CountPendingActivityTasks where
  toPath = Prelude.const "/"

instance Core.ToQuery CountPendingActivityTasks where
  toQuery = Prelude.const Prelude.mempty
