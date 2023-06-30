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
-- Module      : Amazonka.SWF.CountClosedWorkflowExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of closed workflow executions within the given domain
-- that meet the specified filtering criteria.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
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
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--
--     -   @tagFilter.tag@: String constraint. The key is
--         @swf:tagFilter.tag@.
--
--     -   @typeFilter.name@: String constraint. The key is
--         @swf:typeFilter.name@.
--
--     -   @typeFilter.version@: String constraint. The key is
--         @swf:typeFilter.version@.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Amazonka.SWF.CountClosedWorkflowExecutions
  ( -- * Creating a Request
    CountClosedWorkflowExecutions (..),
    newCountClosedWorkflowExecutions,

    -- * Request Lenses
    countClosedWorkflowExecutions_closeStatusFilter,
    countClosedWorkflowExecutions_closeTimeFilter,
    countClosedWorkflowExecutions_executionFilter,
    countClosedWorkflowExecutions_startTimeFilter,
    countClosedWorkflowExecutions_tagFilter,
    countClosedWorkflowExecutions_typeFilter,
    countClosedWorkflowExecutions_domain,

    -- * Destructuring the Response
    WorkflowExecutionCount (..),
    newWorkflowExecutionCount,

    -- * Response Lenses
    workflowExecutionCount_truncated,
    workflowExecutionCount_count,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newCountClosedWorkflowExecutions' smart constructor.
data CountClosedWorkflowExecutions = CountClosedWorkflowExecutions'
  { -- | If specified, only workflow executions that match this close status are
    -- counted. This filter has an affect only if @executionStatus@ is
    -- specified as @CLOSED@.
    --
    -- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
    -- mutually exclusive. You can specify at most one of these in a request.
    closeStatusFilter :: Prelude.Maybe CloseStatusFilter,
    -- | If specified, only workflow executions that meet the close time criteria
    -- of the filter are counted.
    --
    -- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
    -- specify one of these in a request but not both.
    closeTimeFilter :: Prelude.Maybe ExecutionTimeFilter,
    -- | If specified, only workflow executions matching the @WorkflowId@ in the
    -- filter are counted.
    --
    -- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
    -- mutually exclusive. You can specify at most one of these in a request.
    executionFilter :: Prelude.Maybe WorkflowExecutionFilter,
    -- | If specified, only workflow executions that meet the start time criteria
    -- of the filter are counted.
    --
    -- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
    -- specify one of these in a request but not both.
    startTimeFilter :: Prelude.Maybe ExecutionTimeFilter,
    -- | If specified, only executions that have a tag that matches the filter
    -- are counted.
    --
    -- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
    -- mutually exclusive. You can specify at most one of these in a request.
    tagFilter :: Prelude.Maybe TagFilter,
    -- | If specified, indicates the type of the workflow executions to be
    -- counted.
    --
    -- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
    -- mutually exclusive. You can specify at most one of these in a request.
    typeFilter :: Prelude.Maybe WorkflowTypeFilter,
    -- | The name of the domain containing the workflow executions to count.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CountClosedWorkflowExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'closeStatusFilter', 'countClosedWorkflowExecutions_closeStatusFilter' - If specified, only workflow executions that match this close status are
-- counted. This filter has an affect only if @executionStatus@ is
-- specified as @CLOSED@.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
--
-- 'closeTimeFilter', 'countClosedWorkflowExecutions_closeTimeFilter' - If specified, only workflow executions that meet the close time criteria
-- of the filter are counted.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
--
-- 'executionFilter', 'countClosedWorkflowExecutions_executionFilter' - If specified, only workflow executions matching the @WorkflowId@ in the
-- filter are counted.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
--
-- 'startTimeFilter', 'countClosedWorkflowExecutions_startTimeFilter' - If specified, only workflow executions that meet the start time criteria
-- of the filter are counted.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
--
-- 'tagFilter', 'countClosedWorkflowExecutions_tagFilter' - If specified, only executions that have a tag that matches the filter
-- are counted.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
--
-- 'typeFilter', 'countClosedWorkflowExecutions_typeFilter' - If specified, indicates the type of the workflow executions to be
-- counted.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
--
-- 'domain', 'countClosedWorkflowExecutions_domain' - The name of the domain containing the workflow executions to count.
newCountClosedWorkflowExecutions ::
  -- | 'domain'
  Prelude.Text ->
  CountClosedWorkflowExecutions
newCountClosedWorkflowExecutions pDomain_ =
  CountClosedWorkflowExecutions'
    { closeStatusFilter =
        Prelude.Nothing,
      closeTimeFilter = Prelude.Nothing,
      executionFilter = Prelude.Nothing,
      startTimeFilter = Prelude.Nothing,
      tagFilter = Prelude.Nothing,
      typeFilter = Prelude.Nothing,
      domain = pDomain_
    }

-- | If specified, only workflow executions that match this close status are
-- counted. This filter has an affect only if @executionStatus@ is
-- specified as @CLOSED@.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
countClosedWorkflowExecutions_closeStatusFilter :: Lens.Lens' CountClosedWorkflowExecutions (Prelude.Maybe CloseStatusFilter)
countClosedWorkflowExecutions_closeStatusFilter = Lens.lens (\CountClosedWorkflowExecutions' {closeStatusFilter} -> closeStatusFilter) (\s@CountClosedWorkflowExecutions' {} a -> s {closeStatusFilter = a} :: CountClosedWorkflowExecutions)

-- | If specified, only workflow executions that meet the close time criteria
-- of the filter are counted.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
countClosedWorkflowExecutions_closeTimeFilter :: Lens.Lens' CountClosedWorkflowExecutions (Prelude.Maybe ExecutionTimeFilter)
countClosedWorkflowExecutions_closeTimeFilter = Lens.lens (\CountClosedWorkflowExecutions' {closeTimeFilter} -> closeTimeFilter) (\s@CountClosedWorkflowExecutions' {} a -> s {closeTimeFilter = a} :: CountClosedWorkflowExecutions)

-- | If specified, only workflow executions matching the @WorkflowId@ in the
-- filter are counted.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
countClosedWorkflowExecutions_executionFilter :: Lens.Lens' CountClosedWorkflowExecutions (Prelude.Maybe WorkflowExecutionFilter)
countClosedWorkflowExecutions_executionFilter = Lens.lens (\CountClosedWorkflowExecutions' {executionFilter} -> executionFilter) (\s@CountClosedWorkflowExecutions' {} a -> s {executionFilter = a} :: CountClosedWorkflowExecutions)

-- | If specified, only workflow executions that meet the start time criteria
-- of the filter are counted.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
countClosedWorkflowExecutions_startTimeFilter :: Lens.Lens' CountClosedWorkflowExecutions (Prelude.Maybe ExecutionTimeFilter)
countClosedWorkflowExecutions_startTimeFilter = Lens.lens (\CountClosedWorkflowExecutions' {startTimeFilter} -> startTimeFilter) (\s@CountClosedWorkflowExecutions' {} a -> s {startTimeFilter = a} :: CountClosedWorkflowExecutions)

-- | If specified, only executions that have a tag that matches the filter
-- are counted.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
countClosedWorkflowExecutions_tagFilter :: Lens.Lens' CountClosedWorkflowExecutions (Prelude.Maybe TagFilter)
countClosedWorkflowExecutions_tagFilter = Lens.lens (\CountClosedWorkflowExecutions' {tagFilter} -> tagFilter) (\s@CountClosedWorkflowExecutions' {} a -> s {tagFilter = a} :: CountClosedWorkflowExecutions)

-- | If specified, indicates the type of the workflow executions to be
-- counted.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
countClosedWorkflowExecutions_typeFilter :: Lens.Lens' CountClosedWorkflowExecutions (Prelude.Maybe WorkflowTypeFilter)
countClosedWorkflowExecutions_typeFilter = Lens.lens (\CountClosedWorkflowExecutions' {typeFilter} -> typeFilter) (\s@CountClosedWorkflowExecutions' {} a -> s {typeFilter = a} :: CountClosedWorkflowExecutions)

-- | The name of the domain containing the workflow executions to count.
countClosedWorkflowExecutions_domain :: Lens.Lens' CountClosedWorkflowExecutions Prelude.Text
countClosedWorkflowExecutions_domain = Lens.lens (\CountClosedWorkflowExecutions' {domain} -> domain) (\s@CountClosedWorkflowExecutions' {} a -> s {domain = a} :: CountClosedWorkflowExecutions)

instance
  Core.AWSRequest
    CountClosedWorkflowExecutions
  where
  type
    AWSResponse CountClosedWorkflowExecutions =
      WorkflowExecutionCount
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    CountClosedWorkflowExecutions
  where
  hashWithSalt _salt CountClosedWorkflowExecutions' {..} =
    _salt
      `Prelude.hashWithSalt` closeStatusFilter
      `Prelude.hashWithSalt` closeTimeFilter
      `Prelude.hashWithSalt` executionFilter
      `Prelude.hashWithSalt` startTimeFilter
      `Prelude.hashWithSalt` tagFilter
      `Prelude.hashWithSalt` typeFilter
      `Prelude.hashWithSalt` domain

instance Prelude.NFData CountClosedWorkflowExecutions where
  rnf CountClosedWorkflowExecutions' {..} =
    Prelude.rnf closeStatusFilter
      `Prelude.seq` Prelude.rnf closeTimeFilter
      `Prelude.seq` Prelude.rnf executionFilter
      `Prelude.seq` Prelude.rnf startTimeFilter
      `Prelude.seq` Prelude.rnf tagFilter
      `Prelude.seq` Prelude.rnf typeFilter
      `Prelude.seq` Prelude.rnf domain

instance Data.ToHeaders CountClosedWorkflowExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.CountClosedWorkflowExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CountClosedWorkflowExecutions where
  toJSON CountClosedWorkflowExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("closeStatusFilter" Data..=)
              Prelude.<$> closeStatusFilter,
            ("closeTimeFilter" Data..=)
              Prelude.<$> closeTimeFilter,
            ("executionFilter" Data..=)
              Prelude.<$> executionFilter,
            ("startTimeFilter" Data..=)
              Prelude.<$> startTimeFilter,
            ("tagFilter" Data..=) Prelude.<$> tagFilter,
            ("typeFilter" Data..=) Prelude.<$> typeFilter,
            Prelude.Just ("domain" Data..= domain)
          ]
      )

instance Data.ToPath CountClosedWorkflowExecutions where
  toPath = Prelude.const "/"

instance Data.ToQuery CountClosedWorkflowExecutions where
  toQuery = Prelude.const Prelude.mempty
