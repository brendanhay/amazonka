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
-- Module      : Amazonka.SWF.CountPendingDecisionTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.SWF.CountPendingDecisionTasks
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newCountPendingDecisionTasks' smart constructor.
data CountPendingDecisionTasks = CountPendingDecisionTasks'
  { -- | The name of the domain that contains the task list.
    domain :: Prelude.Text,
    -- | The name of the task list.
    taskList :: TaskList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'taskList'
  TaskList ->
  CountPendingDecisionTasks
newCountPendingDecisionTasks pDomain_ pTaskList_ =
  CountPendingDecisionTasks'
    { domain = pDomain_,
      taskList = pTaskList_
    }

-- | The name of the domain that contains the task list.
countPendingDecisionTasks_domain :: Lens.Lens' CountPendingDecisionTasks Prelude.Text
countPendingDecisionTasks_domain = Lens.lens (\CountPendingDecisionTasks' {domain} -> domain) (\s@CountPendingDecisionTasks' {} a -> s {domain = a} :: CountPendingDecisionTasks)

-- | The name of the task list.
countPendingDecisionTasks_taskList :: Lens.Lens' CountPendingDecisionTasks TaskList
countPendingDecisionTasks_taskList = Lens.lens (\CountPendingDecisionTasks' {taskList} -> taskList) (\s@CountPendingDecisionTasks' {} a -> s {taskList = a} :: CountPendingDecisionTasks)

instance Core.AWSRequest CountPendingDecisionTasks where
  type
    AWSResponse CountPendingDecisionTasks =
      PendingTaskCount
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CountPendingDecisionTasks where
  hashWithSalt _salt CountPendingDecisionTasks' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` taskList

instance Prelude.NFData CountPendingDecisionTasks where
  rnf CountPendingDecisionTasks' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf taskList

instance Data.ToHeaders CountPendingDecisionTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.CountPendingDecisionTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CountPendingDecisionTasks where
  toJSON CountPendingDecisionTasks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domain" Data..= domain),
            Prelude.Just ("taskList" Data..= taskList)
          ]
      )

instance Data.ToPath CountPendingDecisionTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery CountPendingDecisionTasks where
  toQuery = Prelude.const Prelude.mempty
