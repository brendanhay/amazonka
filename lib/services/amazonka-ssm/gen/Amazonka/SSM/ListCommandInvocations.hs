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
-- Module      : Amazonka.SSM.ListCommandInvocations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An invocation is copy of a command sent to a specific managed node. A
-- command can apply to one or more managed nodes. A command invocation
-- applies to one managed node. For example, if a user runs @SendCommand@
-- against three managed nodes, then a command invocation is created for
-- each requested managed node ID. @ListCommandInvocations@ provide status
-- about command execution.
--
-- This operation returns paginated results.
module Amazonka.SSM.ListCommandInvocations
  ( -- * Creating a Request
    ListCommandInvocations (..),
    newListCommandInvocations,

    -- * Request Lenses
    listCommandInvocations_nextToken,
    listCommandInvocations_filters,
    listCommandInvocations_details,
    listCommandInvocations_commandId,
    listCommandInvocations_instanceId,
    listCommandInvocations_maxResults,

    -- * Destructuring the Response
    ListCommandInvocationsResponse (..),
    newListCommandInvocationsResponse,

    -- * Response Lenses
    listCommandInvocationsResponse_nextToken,
    listCommandInvocationsResponse_commandInvocations,
    listCommandInvocationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListCommandInvocations' smart constructor.
data ListCommandInvocations = ListCommandInvocations'
  { -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) One or more filters. Use a filter to return a more specific
    -- list of results.
    filters :: Prelude.Maybe (Prelude.NonEmpty CommandFilter),
    -- | (Optional) If set this returns the response of the command executions
    -- and any command output. The default value is @false@.
    details :: Prelude.Maybe Prelude.Bool,
    -- | (Optional) The invocations for a specific command ID.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The command execution details for a specific managed node ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The maximum number of items to return for this call. The call
    -- also returns a token that you can specify in a subsequent call to get
    -- the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCommandInvocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCommandInvocations_nextToken' - (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
--
-- 'filters', 'listCommandInvocations_filters' - (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
--
-- 'details', 'listCommandInvocations_details' - (Optional) If set this returns the response of the command executions
-- and any command output. The default value is @false@.
--
-- 'commandId', 'listCommandInvocations_commandId' - (Optional) The invocations for a specific command ID.
--
-- 'instanceId', 'listCommandInvocations_instanceId' - (Optional) The command execution details for a specific managed node ID.
--
-- 'maxResults', 'listCommandInvocations_maxResults' - (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
newListCommandInvocations ::
  ListCommandInvocations
newListCommandInvocations =
  ListCommandInvocations'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      details = Prelude.Nothing,
      commandId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommandInvocations_nextToken :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Text)
listCommandInvocations_nextToken = Lens.lens (\ListCommandInvocations' {nextToken} -> nextToken) (\s@ListCommandInvocations' {} a -> s {nextToken = a} :: ListCommandInvocations)

-- | (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
listCommandInvocations_filters :: Lens.Lens' ListCommandInvocations (Prelude.Maybe (Prelude.NonEmpty CommandFilter))
listCommandInvocations_filters = Lens.lens (\ListCommandInvocations' {filters} -> filters) (\s@ListCommandInvocations' {} a -> s {filters = a} :: ListCommandInvocations) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) If set this returns the response of the command executions
-- and any command output. The default value is @false@.
listCommandInvocations_details :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Bool)
listCommandInvocations_details = Lens.lens (\ListCommandInvocations' {details} -> details) (\s@ListCommandInvocations' {} a -> s {details = a} :: ListCommandInvocations)

-- | (Optional) The invocations for a specific command ID.
listCommandInvocations_commandId :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Text)
listCommandInvocations_commandId = Lens.lens (\ListCommandInvocations' {commandId} -> commandId) (\s@ListCommandInvocations' {} a -> s {commandId = a} :: ListCommandInvocations)

-- | (Optional) The command execution details for a specific managed node ID.
listCommandInvocations_instanceId :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Text)
listCommandInvocations_instanceId = Lens.lens (\ListCommandInvocations' {instanceId} -> instanceId) (\s@ListCommandInvocations' {} a -> s {instanceId = a} :: ListCommandInvocations)

-- | (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
listCommandInvocations_maxResults :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Natural)
listCommandInvocations_maxResults = Lens.lens (\ListCommandInvocations' {maxResults} -> maxResults) (\s@ListCommandInvocations' {} a -> s {maxResults = a} :: ListCommandInvocations)

instance Core.AWSPager ListCommandInvocations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCommandInvocationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCommandInvocationsResponse_commandInvocations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCommandInvocations_nextToken
          Lens..~ rs
          Lens.^? listCommandInvocationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCommandInvocations where
  type
    AWSResponse ListCommandInvocations =
      ListCommandInvocationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCommandInvocationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "CommandInvocations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCommandInvocations where
  hashWithSalt _salt ListCommandInvocations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` commandId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCommandInvocations where
  rnf ListCommandInvocations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf commandId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListCommandInvocations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListCommandInvocations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCommandInvocations where
  toJSON ListCommandInvocations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("Details" Core..=) Prelude.<$> details,
            ("CommandId" Core..=) Prelude.<$> commandId,
            ("InstanceId" Core..=) Prelude.<$> instanceId,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListCommandInvocations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCommandInvocations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCommandInvocationsResponse' smart constructor.
data ListCommandInvocationsResponse = ListCommandInvocationsResponse'
  { -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) A list of all invocations.
    commandInvocations :: Prelude.Maybe [CommandInvocation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCommandInvocationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCommandInvocationsResponse_nextToken' - (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
--
-- 'commandInvocations', 'listCommandInvocationsResponse_commandInvocations' - (Optional) A list of all invocations.
--
-- 'httpStatus', 'listCommandInvocationsResponse_httpStatus' - The response's http status code.
newListCommandInvocationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCommandInvocationsResponse
newListCommandInvocationsResponse pHttpStatus_ =
  ListCommandInvocationsResponse'
    { nextToken =
        Prelude.Nothing,
      commandInvocations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommandInvocationsResponse_nextToken :: Lens.Lens' ListCommandInvocationsResponse (Prelude.Maybe Prelude.Text)
listCommandInvocationsResponse_nextToken = Lens.lens (\ListCommandInvocationsResponse' {nextToken} -> nextToken) (\s@ListCommandInvocationsResponse' {} a -> s {nextToken = a} :: ListCommandInvocationsResponse)

-- | (Optional) A list of all invocations.
listCommandInvocationsResponse_commandInvocations :: Lens.Lens' ListCommandInvocationsResponse (Prelude.Maybe [CommandInvocation])
listCommandInvocationsResponse_commandInvocations = Lens.lens (\ListCommandInvocationsResponse' {commandInvocations} -> commandInvocations) (\s@ListCommandInvocationsResponse' {} a -> s {commandInvocations = a} :: ListCommandInvocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCommandInvocationsResponse_httpStatus :: Lens.Lens' ListCommandInvocationsResponse Prelude.Int
listCommandInvocationsResponse_httpStatus = Lens.lens (\ListCommandInvocationsResponse' {httpStatus} -> httpStatus) (\s@ListCommandInvocationsResponse' {} a -> s {httpStatus = a} :: ListCommandInvocationsResponse)

instance
  Prelude.NFData
    ListCommandInvocationsResponse
  where
  rnf ListCommandInvocationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf commandInvocations
      `Prelude.seq` Prelude.rnf httpStatus
