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
-- Module      : Amazonka.SWF.CountOpenWorkflowExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of open workflow executions within the given domain
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
module Amazonka.SWF.CountOpenWorkflowExecutions
  ( -- * Creating a Request
    CountOpenWorkflowExecutions (..),
    newCountOpenWorkflowExecutions,

    -- * Request Lenses
    countOpenWorkflowExecutions_executionFilter,
    countOpenWorkflowExecutions_tagFilter,
    countOpenWorkflowExecutions_typeFilter,
    countOpenWorkflowExecutions_domain,
    countOpenWorkflowExecutions_startTimeFilter,

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

-- | /See:/ 'newCountOpenWorkflowExecutions' smart constructor.
data CountOpenWorkflowExecutions = CountOpenWorkflowExecutions'
  { -- | If specified, only workflow executions matching the @WorkflowId@ in the
    -- filter are counted.
    --
    -- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
    -- You can specify at most one of these in a request.
    executionFilter :: Prelude.Maybe WorkflowExecutionFilter,
    -- | If specified, only executions that have a tag that matches the filter
    -- are counted.
    --
    -- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
    -- You can specify at most one of these in a request.
    tagFilter :: Prelude.Maybe TagFilter,
    -- | Specifies the type of the workflow executions to be counted.
    --
    -- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
    -- You can specify at most one of these in a request.
    typeFilter :: Prelude.Maybe WorkflowTypeFilter,
    -- | The name of the domain containing the workflow executions to count.
    domain :: Prelude.Text,
    -- | Specifies the start time criteria that workflow executions must meet in
    -- order to be counted.
    startTimeFilter :: ExecutionTimeFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CountOpenWorkflowExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionFilter', 'countOpenWorkflowExecutions_executionFilter' - If specified, only workflow executions matching the @WorkflowId@ in the
-- filter are counted.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
--
-- 'tagFilter', 'countOpenWorkflowExecutions_tagFilter' - If specified, only executions that have a tag that matches the filter
-- are counted.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
--
-- 'typeFilter', 'countOpenWorkflowExecutions_typeFilter' - Specifies the type of the workflow executions to be counted.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
--
-- 'domain', 'countOpenWorkflowExecutions_domain' - The name of the domain containing the workflow executions to count.
--
-- 'startTimeFilter', 'countOpenWorkflowExecutions_startTimeFilter' - Specifies the start time criteria that workflow executions must meet in
-- order to be counted.
newCountOpenWorkflowExecutions ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'startTimeFilter'
  ExecutionTimeFilter ->
  CountOpenWorkflowExecutions
newCountOpenWorkflowExecutions
  pDomain_
  pStartTimeFilter_ =
    CountOpenWorkflowExecutions'
      { executionFilter =
          Prelude.Nothing,
        tagFilter = Prelude.Nothing,
        typeFilter = Prelude.Nothing,
        domain = pDomain_,
        startTimeFilter = pStartTimeFilter_
      }

-- | If specified, only workflow executions matching the @WorkflowId@ in the
-- filter are counted.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
countOpenWorkflowExecutions_executionFilter :: Lens.Lens' CountOpenWorkflowExecutions (Prelude.Maybe WorkflowExecutionFilter)
countOpenWorkflowExecutions_executionFilter = Lens.lens (\CountOpenWorkflowExecutions' {executionFilter} -> executionFilter) (\s@CountOpenWorkflowExecutions' {} a -> s {executionFilter = a} :: CountOpenWorkflowExecutions)

-- | If specified, only executions that have a tag that matches the filter
-- are counted.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
countOpenWorkflowExecutions_tagFilter :: Lens.Lens' CountOpenWorkflowExecutions (Prelude.Maybe TagFilter)
countOpenWorkflowExecutions_tagFilter = Lens.lens (\CountOpenWorkflowExecutions' {tagFilter} -> tagFilter) (\s@CountOpenWorkflowExecutions' {} a -> s {tagFilter = a} :: CountOpenWorkflowExecutions)

-- | Specifies the type of the workflow executions to be counted.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
countOpenWorkflowExecutions_typeFilter :: Lens.Lens' CountOpenWorkflowExecutions (Prelude.Maybe WorkflowTypeFilter)
countOpenWorkflowExecutions_typeFilter = Lens.lens (\CountOpenWorkflowExecutions' {typeFilter} -> typeFilter) (\s@CountOpenWorkflowExecutions' {} a -> s {typeFilter = a} :: CountOpenWorkflowExecutions)

-- | The name of the domain containing the workflow executions to count.
countOpenWorkflowExecutions_domain :: Lens.Lens' CountOpenWorkflowExecutions Prelude.Text
countOpenWorkflowExecutions_domain = Lens.lens (\CountOpenWorkflowExecutions' {domain} -> domain) (\s@CountOpenWorkflowExecutions' {} a -> s {domain = a} :: CountOpenWorkflowExecutions)

-- | Specifies the start time criteria that workflow executions must meet in
-- order to be counted.
countOpenWorkflowExecutions_startTimeFilter :: Lens.Lens' CountOpenWorkflowExecutions ExecutionTimeFilter
countOpenWorkflowExecutions_startTimeFilter = Lens.lens (\CountOpenWorkflowExecutions' {startTimeFilter} -> startTimeFilter) (\s@CountOpenWorkflowExecutions' {} a -> s {startTimeFilter = a} :: CountOpenWorkflowExecutions)

instance Core.AWSRequest CountOpenWorkflowExecutions where
  type
    AWSResponse CountOpenWorkflowExecutions =
      WorkflowExecutionCount
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CountOpenWorkflowExecutions where
  hashWithSalt _salt CountOpenWorkflowExecutions' {..} =
    _salt `Prelude.hashWithSalt` executionFilter
      `Prelude.hashWithSalt` tagFilter
      `Prelude.hashWithSalt` typeFilter
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` startTimeFilter

instance Prelude.NFData CountOpenWorkflowExecutions where
  rnf CountOpenWorkflowExecutions' {..} =
    Prelude.rnf executionFilter
      `Prelude.seq` Prelude.rnf tagFilter
      `Prelude.seq` Prelude.rnf typeFilter
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf startTimeFilter

instance Data.ToHeaders CountOpenWorkflowExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.CountOpenWorkflowExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CountOpenWorkflowExecutions where
  toJSON CountOpenWorkflowExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("executionFilter" Data..=)
              Prelude.<$> executionFilter,
            ("tagFilter" Data..=) Prelude.<$> tagFilter,
            ("typeFilter" Data..=) Prelude.<$> typeFilter,
            Prelude.Just ("domain" Data..= domain),
            Prelude.Just
              ("startTimeFilter" Data..= startTimeFilter)
          ]
      )

instance Data.ToPath CountOpenWorkflowExecutions where
  toPath = Prelude.const "/"

instance Data.ToQuery CountOpenWorkflowExecutions where
  toQuery = Prelude.const Prelude.mempty
