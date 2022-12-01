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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    listCommands_nextToken,
    listCommands_filters,
    listCommands_commandId,
    listCommands_instanceId,
    listCommands_maxResults,

    -- * Destructuring the Response
    ListCommandsResponse (..),
    newListCommandsResponse,

    -- * Response Lenses
    listCommandsResponse_nextToken,
    listCommandsResponse_commands,
    listCommandsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListCommands' smart constructor.
data ListCommands = ListCommands'
  { -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) One or more filters. Use a filter to return a more specific
    -- list of results.
    filters :: Prelude.Maybe (Prelude.NonEmpty CommandFilter),
    -- | (Optional) If provided, lists only the specified command.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Lists commands issued against this managed node ID.
    --
    -- You can\'t specify a managed node ID in the same command that you
    -- specify @Status@ = @Pending@. This is because the command hasn\'t
    -- reached the managed node yet.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The maximum number of items to return for this call. The call
    -- also returns a token that you can specify in a subsequent call to get
    -- the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listCommands_nextToken' - (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
--
-- 'filters', 'listCommands_filters' - (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
--
-- 'commandId', 'listCommands_commandId' - (Optional) If provided, lists only the specified command.
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
newListCommands ::
  ListCommands
newListCommands =
  ListCommands'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      commandId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommands_nextToken :: Lens.Lens' ListCommands (Prelude.Maybe Prelude.Text)
listCommands_nextToken = Lens.lens (\ListCommands' {nextToken} -> nextToken) (\s@ListCommands' {} a -> s {nextToken = a} :: ListCommands)

-- | (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
listCommands_filters :: Lens.Lens' ListCommands (Prelude.Maybe (Prelude.NonEmpty CommandFilter))
listCommands_filters = Lens.lens (\ListCommands' {filters} -> filters) (\s@ListCommands' {} a -> s {filters = a} :: ListCommands) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) If provided, lists only the specified command.
listCommands_commandId :: Lens.Lens' ListCommands (Prelude.Maybe Prelude.Text)
listCommands_commandId = Lens.lens (\ListCommands' {commandId} -> commandId) (\s@ListCommands' {} a -> s {commandId = a} :: ListCommands)

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

instance Core.AWSPager ListCommands where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCommandsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCommandsResponse_commands Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCommands_nextToken
          Lens..~ rs
          Lens.^? listCommandsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListCommands where
  type AWSResponse ListCommands = ListCommandsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCommandsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Commands" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCommands where
  hashWithSalt _salt ListCommands' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` commandId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCommands where
  rnf ListCommands' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf commandId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListCommands where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.ListCommands" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCommands where
  toJSON ListCommands' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("CommandId" Core..=) Prelude.<$> commandId,
            ("InstanceId" Core..=) Prelude.<$> instanceId,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListCommands where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCommands where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCommandsResponse' smart constructor.
data ListCommandsResponse = ListCommandsResponse'
  { -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The list of commands requested by the user.
    commands :: Prelude.Maybe [Command],
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
-- 'nextToken', 'listCommandsResponse_nextToken' - (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
--
-- 'commands', 'listCommandsResponse_commands' - (Optional) The list of commands requested by the user.
--
-- 'httpStatus', 'listCommandsResponse_httpStatus' - The response's http status code.
newListCommandsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCommandsResponse
newListCommandsResponse pHttpStatus_ =
  ListCommandsResponse'
    { nextToken = Prelude.Nothing,
      commands = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommandsResponse_nextToken :: Lens.Lens' ListCommandsResponse (Prelude.Maybe Prelude.Text)
listCommandsResponse_nextToken = Lens.lens (\ListCommandsResponse' {nextToken} -> nextToken) (\s@ListCommandsResponse' {} a -> s {nextToken = a} :: ListCommandsResponse)

-- | (Optional) The list of commands requested by the user.
listCommandsResponse_commands :: Lens.Lens' ListCommandsResponse (Prelude.Maybe [Command])
listCommandsResponse_commands = Lens.lens (\ListCommandsResponse' {commands} -> commands) (\s@ListCommandsResponse' {} a -> s {commands = a} :: ListCommandsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCommandsResponse_httpStatus :: Lens.Lens' ListCommandsResponse Prelude.Int
listCommandsResponse_httpStatus = Lens.lens (\ListCommandsResponse' {httpStatus} -> httpStatus) (\s@ListCommandsResponse' {} a -> s {httpStatus = a} :: ListCommandsResponse)

instance Prelude.NFData ListCommandsResponse where
  rnf ListCommandsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf commands
      `Prelude.seq` Prelude.rnf httpStatus
