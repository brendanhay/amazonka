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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a test repository triggers operation.
--
-- /See:/ 'newTestRepositoryTriggers' smart constructor.
data TestRepositoryTriggers = TestRepositoryTriggers'
  { -- | The name of the repository in which to test the triggers.
    repositoryName :: Prelude.Text,
    -- | The list of triggers to test.
    triggers :: [RepositoryTrigger]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
testRepositoryTriggers_triggers = Lens.lens (\TestRepositoryTriggers' {triggers} -> triggers) (\s@TestRepositoryTriggers' {} a -> s {triggers = a} :: TestRepositoryTriggers) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TestRepositoryTriggers where
  type
    Rs TestRepositoryTriggers =
      TestRepositoryTriggersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestRepositoryTriggersResponse'
            Prelude.<$> ( x Prelude..?> "successfulExecutions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..?> "failedExecutions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestRepositoryTriggers

instance Prelude.NFData TestRepositoryTriggers

instance Prelude.ToHeaders TestRepositoryTriggers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.TestRepositoryTriggers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON TestRepositoryTriggers where
  toJSON TestRepositoryTriggers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just ("triggers" Prelude..= triggers)
          ]
      )

instance Prelude.ToPath TestRepositoryTriggers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery TestRepositoryTriggers where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a test repository triggers operation.
--
-- /See:/ 'newTestRepositoryTriggersResponse' smart constructor.
data TestRepositoryTriggersResponse = TestRepositoryTriggersResponse'
  { -- | The list of triggers that were successfully tested. This list provides
    -- the names of the triggers that were successfully tested, separated by
    -- commas.
    successfulExecutions :: Prelude.Maybe [Prelude.Text],
    -- | The list of triggers that were not tested. This list provides the names
    -- of the triggers that could not be tested, separated by commas.
    failedExecutions :: Prelude.Maybe [RepositoryTriggerExecutionFailure],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  TestRepositoryTriggersResponse
newTestRepositoryTriggersResponse pHttpStatus_ =
  TestRepositoryTriggersResponse'
    { successfulExecutions =
        Prelude.Nothing,
      failedExecutions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of triggers that were successfully tested. This list provides
-- the names of the triggers that were successfully tested, separated by
-- commas.
testRepositoryTriggersResponse_successfulExecutions :: Lens.Lens' TestRepositoryTriggersResponse (Prelude.Maybe [Prelude.Text])
testRepositoryTriggersResponse_successfulExecutions = Lens.lens (\TestRepositoryTriggersResponse' {successfulExecutions} -> successfulExecutions) (\s@TestRepositoryTriggersResponse' {} a -> s {successfulExecutions = a} :: TestRepositoryTriggersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The list of triggers that were not tested. This list provides the names
-- of the triggers that could not be tested, separated by commas.
testRepositoryTriggersResponse_failedExecutions :: Lens.Lens' TestRepositoryTriggersResponse (Prelude.Maybe [RepositoryTriggerExecutionFailure])
testRepositoryTriggersResponse_failedExecutions = Lens.lens (\TestRepositoryTriggersResponse' {failedExecutions} -> failedExecutions) (\s@TestRepositoryTriggersResponse' {} a -> s {failedExecutions = a} :: TestRepositoryTriggersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
testRepositoryTriggersResponse_httpStatus :: Lens.Lens' TestRepositoryTriggersResponse Prelude.Int
testRepositoryTriggersResponse_httpStatus = Lens.lens (\TestRepositoryTriggersResponse' {httpStatus} -> httpStatus) (\s@TestRepositoryTriggersResponse' {} a -> s {httpStatus = a} :: TestRepositoryTriggersResponse)

instance
  Prelude.NFData
    TestRepositoryTriggersResponse
