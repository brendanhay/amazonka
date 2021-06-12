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
-- Module      : Network.AWS.CodeCommit.TestRepositoryTriggers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the functionality of repository triggers by sending information to
-- the trigger target. If real data is available in the repository, the
-- test sends data from the last commit. If no data is available, sample
-- data is generated.
module Network.AWS.CodeCommit.TestRepositoryTriggers
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
    testRepositoryTriggersResponse_successfulExecutions,
    testRepositoryTriggersResponse_failedExecutions,
    testRepositoryTriggersResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a test repository triggers operation.
--
-- /See:/ 'newTestRepositoryTriggers' smart constructor.
data TestRepositoryTriggers = TestRepositoryTriggers'
  { -- | The name of the repository in which to test the triggers.
    repositoryName :: Core.Text,
    -- | The list of triggers to test.
    triggers :: [RepositoryTrigger]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  TestRepositoryTriggers
newTestRepositoryTriggers pRepositoryName_ =
  TestRepositoryTriggers'
    { repositoryName =
        pRepositoryName_,
      triggers = Core.mempty
    }

-- | The name of the repository in which to test the triggers.
testRepositoryTriggers_repositoryName :: Lens.Lens' TestRepositoryTriggers Core.Text
testRepositoryTriggers_repositoryName = Lens.lens (\TestRepositoryTriggers' {repositoryName} -> repositoryName) (\s@TestRepositoryTriggers' {} a -> s {repositoryName = a} :: TestRepositoryTriggers)

-- | The list of triggers to test.
testRepositoryTriggers_triggers :: Lens.Lens' TestRepositoryTriggers [RepositoryTrigger]
testRepositoryTriggers_triggers = Lens.lens (\TestRepositoryTriggers' {triggers} -> triggers) (\s@TestRepositoryTriggers' {} a -> s {triggers = a} :: TestRepositoryTriggers) Core.. Lens._Coerce

instance Core.AWSRequest TestRepositoryTriggers where
  type
    AWSResponse TestRepositoryTriggers =
      TestRepositoryTriggersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestRepositoryTriggersResponse'
            Core.<$> ( x Core..?> "successfulExecutions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "failedExecutions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TestRepositoryTriggers

instance Core.NFData TestRepositoryTriggers

instance Core.ToHeaders TestRepositoryTriggers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.TestRepositoryTriggers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TestRepositoryTriggers where
  toJSON TestRepositoryTriggers' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("triggers" Core..= triggers)
          ]
      )

instance Core.ToPath TestRepositoryTriggers where
  toPath = Core.const "/"

instance Core.ToQuery TestRepositoryTriggers where
  toQuery = Core.const Core.mempty

-- | Represents the output of a test repository triggers operation.
--
-- /See:/ 'newTestRepositoryTriggersResponse' smart constructor.
data TestRepositoryTriggersResponse = TestRepositoryTriggersResponse'
  { -- | The list of triggers that were successfully tested. This list provides
    -- the names of the triggers that were successfully tested, separated by
    -- commas.
    successfulExecutions :: Core.Maybe [Core.Text],
    -- | The list of triggers that were not tested. This list provides the names
    -- of the triggers that could not be tested, separated by commas.
    failedExecutions :: Core.Maybe [RepositoryTriggerExecutionFailure],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TestRepositoryTriggersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successfulExecutions', 'testRepositoryTriggersResponse_successfulExecutions' - The list of triggers that were successfully tested. This list provides
-- the names of the triggers that were successfully tested, separated by
-- commas.
--
-- 'failedExecutions', 'testRepositoryTriggersResponse_failedExecutions' - The list of triggers that were not tested. This list provides the names
-- of the triggers that could not be tested, separated by commas.
--
-- 'httpStatus', 'testRepositoryTriggersResponse_httpStatus' - The response's http status code.
newTestRepositoryTriggersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TestRepositoryTriggersResponse
newTestRepositoryTriggersResponse pHttpStatus_ =
  TestRepositoryTriggersResponse'
    { successfulExecutions =
        Core.Nothing,
      failedExecutions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of triggers that were successfully tested. This list provides
-- the names of the triggers that were successfully tested, separated by
-- commas.
testRepositoryTriggersResponse_successfulExecutions :: Lens.Lens' TestRepositoryTriggersResponse (Core.Maybe [Core.Text])
testRepositoryTriggersResponse_successfulExecutions = Lens.lens (\TestRepositoryTriggersResponse' {successfulExecutions} -> successfulExecutions) (\s@TestRepositoryTriggersResponse' {} a -> s {successfulExecutions = a} :: TestRepositoryTriggersResponse) Core.. Lens.mapping Lens._Coerce

-- | The list of triggers that were not tested. This list provides the names
-- of the triggers that could not be tested, separated by commas.
testRepositoryTriggersResponse_failedExecutions :: Lens.Lens' TestRepositoryTriggersResponse (Core.Maybe [RepositoryTriggerExecutionFailure])
testRepositoryTriggersResponse_failedExecutions = Lens.lens (\TestRepositoryTriggersResponse' {failedExecutions} -> failedExecutions) (\s@TestRepositoryTriggersResponse' {} a -> s {failedExecutions = a} :: TestRepositoryTriggersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
testRepositoryTriggersResponse_httpStatus :: Lens.Lens' TestRepositoryTriggersResponse Core.Int
testRepositoryTriggersResponse_httpStatus = Lens.lens (\TestRepositoryTriggersResponse' {httpStatus} -> httpStatus) (\s@TestRepositoryTriggersResponse' {} a -> s {httpStatus = a} :: TestRepositoryTriggersResponse)

instance Core.NFData TestRepositoryTriggersResponse
