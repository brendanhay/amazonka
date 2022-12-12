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
    listCommandInvocations_commandId,
    listCommandInvocations_details,
    listCommandInvocations_filters,
    listCommandInvocations_instanceId,
    listCommandInvocations_maxResults,
    listCommandInvocations_nextToken,

    -- * Destructuring the Response
    ListCommandInvocationsResponse (..),
    newListCommandInvocationsResponse,

    -- * Response Lenses
    listCommandInvocationsResponse_commandInvocations,
    listCommandInvocationsResponse_nextToken,
    listCommandInvocationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListCommandInvocations' smart constructor.
data ListCommandInvocations = ListCommandInvocations'
  { -- | (Optional) The invocations for a specific command ID.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) If set this returns the response of the command executions
    -- and any command output. The default value is @false@.
    details :: Prelude.Maybe Prelude.Bool,
    -- | (Optional) One or more filters. Use a filter to return a more specific
    -- list of results.
    filters :: Prelude.Maybe (Prelude.NonEmpty CommandFilter),
    -- | (Optional) The command execution details for a specific managed node ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The maximum number of items to return for this call. The call
    -- also returns a token that you can specify in a subsequent call to get
    -- the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'commandId', 'listCommandInvocations_commandId' - (Optional) The invocations for a specific command ID.
--
-- 'details', 'listCommandInvocations_details' - (Optional) If set this returns the response of the command executions
-- and any command output. The default value is @false@.
--
-- 'filters', 'listCommandInvocations_filters' - (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
--
-- 'instanceId', 'listCommandInvocations_instanceId' - (Optional) The command execution details for a specific managed node ID.
--
-- 'maxResults', 'listCommandInvocations_maxResults' - (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
--
-- 'nextToken', 'listCommandInvocations_nextToken' - (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
newListCommandInvocations ::
  ListCommandInvocations
newListCommandInvocations =
  ListCommandInvocations'
    { commandId =
        Prelude.Nothing,
      details = Prelude.Nothing,
      filters = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | (Optional) The invocations for a specific command ID.
listCommandInvocations_commandId :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Text)
listCommandInvocations_commandId = Lens.lens (\ListCommandInvocations' {commandId} -> commandId) (\s@ListCommandInvocations' {} a -> s {commandId = a} :: ListCommandInvocations)

-- | (Optional) If set this returns the response of the command executions
-- and any command output. The default value is @false@.
listCommandInvocations_details :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Bool)
listCommandInvocations_details = Lens.lens (\ListCommandInvocations' {details} -> details) (\s@ListCommandInvocations' {} a -> s {details = a} :: ListCommandInvocations)

-- | (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
listCommandInvocations_filters :: Lens.Lens' ListCommandInvocations (Prelude.Maybe (Prelude.NonEmpty CommandFilter))
listCommandInvocations_filters = Lens.lens (\ListCommandInvocations' {filters} -> filters) (\s@ListCommandInvocations' {} a -> s {filters = a} :: ListCommandInvocations) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) The command execution details for a specific managed node ID.
listCommandInvocations_instanceId :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Text)
listCommandInvocations_instanceId = Lens.lens (\ListCommandInvocations' {instanceId} -> instanceId) (\s@ListCommandInvocations' {} a -> s {instanceId = a} :: ListCommandInvocations)

-- | (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
listCommandInvocations_maxResults :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Natural)
listCommandInvocations_maxResults = Lens.lens (\ListCommandInvocations' {maxResults} -> maxResults) (\s@ListCommandInvocations' {} a -> s {maxResults = a} :: ListCommandInvocations)

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommandInvocations_nextToken :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Text)
listCommandInvocations_nextToken = Lens.lens (\ListCommandInvocations' {nextToken} -> nextToken) (\s@ListCommandInvocations' {} a -> s {nextToken = a} :: ListCommandInvocations)

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
            Prelude.<$> ( x Data..?> "CommandInvocations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCommandInvocations where
  hashWithSalt _salt ListCommandInvocations' {..} =
    _salt `Prelude.hashWithSalt` commandId
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCommandInvocations where
  rnf ListCommandInvocations' {..} =
    Prelude.rnf commandId
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCommandInvocations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.ListCommandInvocations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCommandInvocations where
  toJSON ListCommandInvocations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CommandId" Data..=) Prelude.<$> commandId,
            ("Details" Data..=) Prelude.<$> details,
            ("Filters" Data..=) Prelude.<$> filters,
            ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListCommandInvocations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCommandInvocations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCommandInvocationsResponse' smart constructor.
data ListCommandInvocationsResponse = ListCommandInvocationsResponse'
  { -- | (Optional) A list of all invocations.
    commandInvocations :: Prelude.Maybe [CommandInvocation],
    -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'commandInvocations', 'listCommandInvocationsResponse_commandInvocations' - (Optional) A list of all invocations.
--
-- 'nextToken', 'listCommandInvocationsResponse_nextToken' - (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
--
-- 'httpStatus', 'listCommandInvocationsResponse_httpStatus' - The response's http status code.
newListCommandInvocationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCommandInvocationsResponse
newListCommandInvocationsResponse pHttpStatus_ =
  ListCommandInvocationsResponse'
    { commandInvocations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Optional) A list of all invocations.
listCommandInvocationsResponse_commandInvocations :: Lens.Lens' ListCommandInvocationsResponse (Prelude.Maybe [CommandInvocation])
listCommandInvocationsResponse_commandInvocations = Lens.lens (\ListCommandInvocationsResponse' {commandInvocations} -> commandInvocations) (\s@ListCommandInvocationsResponse' {} a -> s {commandInvocations = a} :: ListCommandInvocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommandInvocationsResponse_nextToken :: Lens.Lens' ListCommandInvocationsResponse (Prelude.Maybe Prelude.Text)
listCommandInvocationsResponse_nextToken = Lens.lens (\ListCommandInvocationsResponse' {nextToken} -> nextToken) (\s@ListCommandInvocationsResponse' {} a -> s {nextToken = a} :: ListCommandInvocationsResponse)

-- | The response's http status code.
listCommandInvocationsResponse_httpStatus :: Lens.Lens' ListCommandInvocationsResponse Prelude.Int
listCommandInvocationsResponse_httpStatus = Lens.lens (\ListCommandInvocationsResponse' {httpStatus} -> httpStatus) (\s@ListCommandInvocationsResponse' {} a -> s {httpStatus = a} :: ListCommandInvocationsResponse)

instance
  Prelude.NFData
    ListCommandInvocationsResponse
  where
  rnf ListCommandInvocationsResponse' {..} =
    Prelude.rnf commandInvocations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
