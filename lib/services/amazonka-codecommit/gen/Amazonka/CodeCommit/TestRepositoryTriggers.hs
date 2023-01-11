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
-- Module      : Amazonka.CodeCommit.TestRepositoryTriggers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the functionality of repository triggers by sending information to
-- the trigger target. If real data is available in the repository, the
-- test sends data from the last commit. If no data is available, sample
-- data is generated.
module Amazonka.CodeCommit.TestRepositoryTriggers
  ( -- * Creating a Request
    TestRepositoryTriggers (..),
    newTestRepositoryTriggers,

    -- * Request Lenses
    testRepositoryTriggers_repositoryName,
    testRepositoryTriggers_triggers,

    -- * Destructuring the Response
    TestRepositoryTriggersResponse (..),
    newTestRepositoryTriggersResponse,

    -- * Response Lenses
    testRepositoryTriggersResponse_failedExecutions,
    testRepositoryTriggersResponse_successfulExecutions,
    testRepositoryTriggersResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a test repository triggers operation.
--
-- /See:/ 'newTestRepositoryTriggers' smart constructor.
data TestRepositoryTriggers = TestRepositoryTriggers'
  { -- | The name of the repository in which to test the triggers.
    repositoryName :: Prelude.Text,
    -- | The list of triggers to test.
    triggers :: [RepositoryTrigger]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestRepositoryTriggers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'testRepositoryTriggers_repositoryName' - The name of the repository in which to test the triggers.
--
-- 'triggers', 'testRepositoryTriggers_triggers' - The list of triggers to test.
newTestRepositoryTriggers ::
  -- | 'repositoryName'
  Prelude.Text ->
  TestRepositoryTriggers
newTestRepositoryTriggers pRepositoryName_ =
  TestRepositoryTriggers'
    { repositoryName =
        pRepositoryName_,
      triggers = Prelude.mempty
    }

-- | The name of the repository in which to test the triggers.
testRepositoryTriggers_repositoryName :: Lens.Lens' TestRepositoryTriggers Prelude.Text
testRepositoryTriggers_repositoryName = Lens.lens (\TestRepositoryTriggers' {repositoryName} -> repositoryName) (\s@TestRepositoryTriggers' {} a -> s {repositoryName = a} :: TestRepositoryTriggers)

-- | The list of triggers to test.
testRepositoryTriggers_triggers :: Lens.Lens' TestRepositoryTriggers [RepositoryTrigger]
testRepositoryTriggers_triggers = Lens.lens (\TestRepositoryTriggers' {triggers} -> triggers) (\s@TestRepositoryTriggers' {} a -> s {triggers = a} :: TestRepositoryTriggers) Prelude.. Lens.coerced

instance Core.AWSRequest TestRepositoryTriggers where
  type
    AWSResponse TestRepositoryTriggers =
      TestRepositoryTriggersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestRepositoryTriggersResponse'
            Prelude.<$> ( x Data..?> "failedExecutions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "successfulExecutions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestRepositoryTriggers where
  hashWithSalt _salt TestRepositoryTriggers' {..} =
    _salt `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` triggers

instance Prelude.NFData TestRepositoryTriggers where
  rnf TestRepositoryTriggers' {..} =
    Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf triggers

instance Data.ToHeaders TestRepositoryTriggers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.TestRepositoryTriggers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TestRepositoryTriggers where
  toJSON TestRepositoryTriggers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("triggers" Data..= triggers)
          ]
      )

instance Data.ToPath TestRepositoryTriggers where
  toPath = Prelude.const "/"

instance Data.ToQuery TestRepositoryTriggers where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a test repository triggers operation.
--
-- /See:/ 'newTestRepositoryTriggersResponse' smart constructor.
data TestRepositoryTriggersResponse = TestRepositoryTriggersResponse'
  { -- | The list of triggers that were not tested. This list provides the names
    -- of the triggers that could not be tested, separated by commas.
    failedExecutions :: Prelude.Maybe [RepositoryTriggerExecutionFailure],
    -- | The list of triggers that were successfully tested. This list provides
    -- the names of the triggers that were successfully tested, separated by
    -- commas.
    successfulExecutions :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestRepositoryTriggersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedExecutions', 'testRepositoryTriggersResponse_failedExecutions' - The list of triggers that were not tested. This list provides the names
-- of the triggers that could not be tested, separated by commas.
--
-- 'successfulExecutions', 'testRepositoryTriggersResponse_successfulExecutions' - The list of triggers that were successfully tested. This list provides
-- the names of the triggers that were successfully tested, separated by
-- commas.
--
-- 'httpStatus', 'testRepositoryTriggersResponse_httpStatus' - The response's http status code.
newTestRepositoryTriggersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestRepositoryTriggersResponse
newTestRepositoryTriggersResponse pHttpStatus_ =
  TestRepositoryTriggersResponse'
    { failedExecutions =
        Prelude.Nothing,
      successfulExecutions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of triggers that were not tested. This list provides the names
-- of the triggers that could not be tested, separated by commas.
testRepositoryTriggersResponse_failedExecutions :: Lens.Lens' TestRepositoryTriggersResponse (Prelude.Maybe [RepositoryTriggerExecutionFailure])
testRepositoryTriggersResponse_failedExecutions = Lens.lens (\TestRepositoryTriggersResponse' {failedExecutions} -> failedExecutions) (\s@TestRepositoryTriggersResponse' {} a -> s {failedExecutions = a} :: TestRepositoryTriggersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of triggers that were successfully tested. This list provides
-- the names of the triggers that were successfully tested, separated by
-- commas.
testRepositoryTriggersResponse_successfulExecutions :: Lens.Lens' TestRepositoryTriggersResponse (Prelude.Maybe [Prelude.Text])
testRepositoryTriggersResponse_successfulExecutions = Lens.lens (\TestRepositoryTriggersResponse' {successfulExecutions} -> successfulExecutions) (\s@TestRepositoryTriggersResponse' {} a -> s {successfulExecutions = a} :: TestRepositoryTriggersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
testRepositoryTriggersResponse_httpStatus :: Lens.Lens' TestRepositoryTriggersResponse Prelude.Int
testRepositoryTriggersResponse_httpStatus = Lens.lens (\TestRepositoryTriggersResponse' {httpStatus} -> httpStatus) (\s@TestRepositoryTriggersResponse' {} a -> s {httpStatus = a} :: TestRepositoryTriggersResponse)

instance
  Prelude.NFData
    TestRepositoryTriggersResponse
  where
  rnf TestRepositoryTriggersResponse' {..} =
    Prelude.rnf failedExecutions
      `Prelude.seq` Prelude.rnf successfulExecutions
      `Prelude.seq` Prelude.rnf httpStatus
