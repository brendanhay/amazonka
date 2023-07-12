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
-- Module      : Amazonka.SSM.ListCommands
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the commands requested by users of the Amazon Web Services
-- account.
--
-- This operation returns paginated results.
module Amazonka.SSM.ListCommands
  ( -- * Creating a Request
    ListCommands (..),
    newListCommands,

    -- * Request Lenses
    listCommands_commandId,
    listCommands_filters,
    listCommands_instanceId,
    listCommands_maxResults,
    listCommands_nextToken,

    -- * Destructuring the Response
    ListCommandsResponse (..),
    newListCommandsResponse,

    -- * Response Lenses
    listCommandsResponse_commands,
    listCommandsResponse_nextToken,
    listCommandsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListCommands' smart constructor.
data ListCommands = ListCommands'
  { -- | (Optional) If provided, lists only the specified command.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) One or more filters. Use a filter to return a more specific
    -- list of results.
    filters :: Prelude.Maybe (Prelude.NonEmpty CommandFilter),
    -- | (Optional) Lists commands issued against this managed node ID.
    --
    -- You can\'t specify a managed node ID in the same command that you
    -- specify @Status@ = @Pending@. This is because the command hasn\'t
    -- reached the managed node yet.
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
-- Create a value of 'ListCommands' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commandId', 'listCommands_commandId' - (Optional) If provided, lists only the specified command.
--
-- 'filters', 'listCommands_filters' - (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
--
-- 'instanceId', 'listCommands_instanceId' - (Optional) Lists commands issued against this managed node ID.
--
-- You can\'t specify a managed node ID in the same command that you
-- specify @Status@ = @Pending@. This is because the command hasn\'t
-- reached the managed node yet.
--
-- 'maxResults', 'listCommands_maxResults' - (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
--
-- 'nextToken', 'listCommands_nextToken' - (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
newListCommands ::
  ListCommands
newListCommands =
  ListCommands'
    { commandId = Prelude.Nothing,
      filters = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | (Optional) If provided, lists only the specified command.
listCommands_commandId :: Lens.Lens' ListCommands (Prelude.Maybe Prelude.Text)
listCommands_commandId = Lens.lens (\ListCommands' {commandId} -> commandId) (\s@ListCommands' {} a -> s {commandId = a} :: ListCommands)

-- | (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
listCommands_filters :: Lens.Lens' ListCommands (Prelude.Maybe (Prelude.NonEmpty CommandFilter))
listCommands_filters = Lens.lens (\ListCommands' {filters} -> filters) (\s@ListCommands' {} a -> s {filters = a} :: ListCommands) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) Lists commands issued against this managed node ID.
--
-- You can\'t specify a managed node ID in the same command that you
-- specify @Status@ = @Pending@. This is because the command hasn\'t
-- reached the managed node yet.
listCommands_instanceId :: Lens.Lens' ListCommands (Prelude.Maybe Prelude.Text)
listCommands_instanceId = Lens.lens (\ListCommands' {instanceId} -> instanceId) (\s@ListCommands' {} a -> s {instanceId = a} :: ListCommands)

-- | (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
listCommands_maxResults :: Lens.Lens' ListCommands (Prelude.Maybe Prelude.Natural)
listCommands_maxResults = Lens.lens (\ListCommands' {maxResults} -> maxResults) (\s@ListCommands' {} a -> s {maxResults = a} :: ListCommands)

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommands_nextToken :: Lens.Lens' ListCommands (Prelude.Maybe Prelude.Text)
listCommands_nextToken = Lens.lens (\ListCommands' {nextToken} -> nextToken) (\s@ListCommands' {} a -> s {nextToken = a} :: ListCommands)

instance Core.AWSPager ListCommands where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCommandsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCommandsResponse_commands
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCommands_nextToken
          Lens..~ rs
          Lens.^? listCommandsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCommands where
  type AWSResponse ListCommands = ListCommandsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCommandsResponse'
            Prelude.<$> (x Data..?> "Commands" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCommands where
  hashWithSalt _salt ListCommands' {..} =
    _salt
      `Prelude.hashWithSalt` commandId
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCommands where
  rnf ListCommands' {..} =
    Prelude.rnf commandId
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCommands where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.ListCommands" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCommands where
  toJSON ListCommands' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CommandId" Data..=) Prelude.<$> commandId,
            ("Filters" Data..=) Prelude.<$> filters,
            ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListCommands where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCommands where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCommandsResponse' smart constructor.
data ListCommandsResponse = ListCommandsResponse'
  { -- | (Optional) The list of commands requested by the user.
    commands :: Prelude.Maybe [Command],
    -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCommandsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commands', 'listCommandsResponse_commands' - (Optional) The list of commands requested by the user.
--
-- 'nextToken', 'listCommandsResponse_nextToken' - (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
--
-- 'httpStatus', 'listCommandsResponse_httpStatus' - The response's http status code.
newListCommandsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCommandsResponse
newListCommandsResponse pHttpStatus_ =
  ListCommandsResponse'
    { commands = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Optional) The list of commands requested by the user.
listCommandsResponse_commands :: Lens.Lens' ListCommandsResponse (Prelude.Maybe [Command])
listCommandsResponse_commands = Lens.lens (\ListCommandsResponse' {commands} -> commands) (\s@ListCommandsResponse' {} a -> s {commands = a} :: ListCommandsResponse) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommandsResponse_nextToken :: Lens.Lens' ListCommandsResponse (Prelude.Maybe Prelude.Text)
listCommandsResponse_nextToken = Lens.lens (\ListCommandsResponse' {nextToken} -> nextToken) (\s@ListCommandsResponse' {} a -> s {nextToken = a} :: ListCommandsResponse)

-- | The response's http status code.
listCommandsResponse_httpStatus :: Lens.Lens' ListCommandsResponse Prelude.Int
listCommandsResponse_httpStatus = Lens.lens (\ListCommandsResponse' {httpStatus} -> httpStatus) (\s@ListCommandsResponse' {} a -> s {httpStatus = a} :: ListCommandsResponse)

instance Prelude.NFData ListCommandsResponse where
  rnf ListCommandsResponse' {..} =
    Prelude.rnf commands
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
