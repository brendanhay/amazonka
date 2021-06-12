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
-- Module      : Network.AWS.SWF.CountPendingDecisionTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated number of decision tasks in the specified task
-- list. The count returned is an approximation and isn\'t guaranteed to be
-- exact. If you specify a task list that no decision task was ever
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
module Network.AWS.SWF.CountPendingDecisionTasks
  ( -- * Creating a Request
    CountPendingDecisionTasks (..),
    newCountPendingDecisionTasks,

    -- * Request Lenses
    countPendingDecisionTasks_domain,
    countPendingDecisionTasks_taskList,

    -- * Destructuring the Response
    PendingTaskCount (..),
    newPendingTaskCount,

    -- * Response Lenses
    pendingTaskCount_truncated,
    pendingTaskCount_count,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newCountPendingDecisionTasks' smart constructor.
data CountPendingDecisionTasks = CountPendingDecisionTasks'
  { -- | The name of the domain that contains the task list.
    domain :: Core.Text,
    -- | The name of the task list.
    taskList :: TaskList
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CountPendingDecisionTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'countPendingDecisionTasks_domain' - The name of the domain that contains the task list.
--
-- 'taskList', 'countPendingDecisionTasks_taskList' - The name of the task list.
newCountPendingDecisionTasks ::
  -- | 'domain'
  Core.Text ->
  -- | 'taskList'
  TaskList ->
  CountPendingDecisionTasks
newCountPendingDecisionTasks pDomain_ pTaskList_ =
  CountPendingDecisionTasks'
    { domain = pDomain_,
      taskList = pTaskList_
    }

-- | The name of the domain that contains the task list.
countPendingDecisionTasks_domain :: Lens.Lens' CountPendingDecisionTasks Core.Text
countPendingDecisionTasks_domain = Lens.lens (\CountPendingDecisionTasks' {domain} -> domain) (\s@CountPendingDecisionTasks' {} a -> s {domain = a} :: CountPendingDecisionTasks)

-- | The name of the task list.
countPendingDecisionTasks_taskList :: Lens.Lens' CountPendingDecisionTasks TaskList
countPendingDecisionTasks_taskList = Lens.lens (\CountPendingDecisionTasks' {taskList} -> taskList) (\s@CountPendingDecisionTasks' {} a -> s {taskList = a} :: CountPendingDecisionTasks)

instance Core.AWSRequest CountPendingDecisionTasks where
  type
    AWSResponse CountPendingDecisionTasks =
      PendingTaskCount
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CountPendingDecisionTasks

instance Core.NFData CountPendingDecisionTasks

instance Core.ToHeaders CountPendingDecisionTasks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.CountPendingDecisionTasks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CountPendingDecisionTasks where
  toJSON CountPendingDecisionTasks' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("taskList" Core..= taskList)
          ]
      )

instance Core.ToPath CountPendingDecisionTasks where
  toPath = Core.const "/"

instance Core.ToQuery CountPendingDecisionTasks where
  toQuery = Core.const Core.mempty
