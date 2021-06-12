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
-- Module      : Network.AWS.SSM.ListCommands
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the commands requested by users of the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListCommands
  ( -- * Creating a Request
    ListCommands (..),
    newListCommands,

    -- * Request Lenses
    listCommands_nextToken,
    listCommands_instanceId,
    listCommands_maxResults,
    listCommands_commandId,
    listCommands_filters,

    -- * Destructuring the Response
    ListCommandsResponse (..),
    newListCommandsResponse,

    -- * Response Lenses
    listCommandsResponse_nextToken,
    listCommandsResponse_commands,
    listCommandsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListCommands' smart constructor.
data ListCommands = ListCommands'
  { -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | (Optional) Lists commands issued against this instance ID.
    --
    -- You can\'t specify an instance ID in the same command that you specify
    -- @Status@ = @Pending@. This is because the command has not reached the
    -- instance yet.
    instanceId :: Core.Maybe Core.Text,
    -- | (Optional) The maximum number of items to return for this call. The call
    -- also returns a token that you can specify in a subsequent call to get
    -- the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | (Optional) If provided, lists only the specified command.
    commandId :: Core.Maybe Core.Text,
    -- | (Optional) One or more filters. Use a filter to return a more specific
    -- list of results.
    filters :: Core.Maybe (Core.NonEmpty CommandFilter)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'instanceId', 'listCommands_instanceId' - (Optional) Lists commands issued against this instance ID.
--
-- You can\'t specify an instance ID in the same command that you specify
-- @Status@ = @Pending@. This is because the command has not reached the
-- instance yet.
--
-- 'maxResults', 'listCommands_maxResults' - (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
--
-- 'commandId', 'listCommands_commandId' - (Optional) If provided, lists only the specified command.
--
-- 'filters', 'listCommands_filters' - (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
newListCommands ::
  ListCommands
newListCommands =
  ListCommands'
    { nextToken = Core.Nothing,
      instanceId = Core.Nothing,
      maxResults = Core.Nothing,
      commandId = Core.Nothing,
      filters = Core.Nothing
    }

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommands_nextToken :: Lens.Lens' ListCommands (Core.Maybe Core.Text)
listCommands_nextToken = Lens.lens (\ListCommands' {nextToken} -> nextToken) (\s@ListCommands' {} a -> s {nextToken = a} :: ListCommands)

-- | (Optional) Lists commands issued against this instance ID.
--
-- You can\'t specify an instance ID in the same command that you specify
-- @Status@ = @Pending@. This is because the command has not reached the
-- instance yet.
listCommands_instanceId :: Lens.Lens' ListCommands (Core.Maybe Core.Text)
listCommands_instanceId = Lens.lens (\ListCommands' {instanceId} -> instanceId) (\s@ListCommands' {} a -> s {instanceId = a} :: ListCommands)

-- | (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
listCommands_maxResults :: Lens.Lens' ListCommands (Core.Maybe Core.Natural)
listCommands_maxResults = Lens.lens (\ListCommands' {maxResults} -> maxResults) (\s@ListCommands' {} a -> s {maxResults = a} :: ListCommands)

-- | (Optional) If provided, lists only the specified command.
listCommands_commandId :: Lens.Lens' ListCommands (Core.Maybe Core.Text)
listCommands_commandId = Lens.lens (\ListCommands' {commandId} -> commandId) (\s@ListCommands' {} a -> s {commandId = a} :: ListCommands)

-- | (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
listCommands_filters :: Lens.Lens' ListCommands (Core.Maybe (Core.NonEmpty CommandFilter))
listCommands_filters = Lens.lens (\ListCommands' {filters} -> filters) (\s@ListCommands' {} a -> s {filters = a} :: ListCommands) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListCommands where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCommandsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listCommandsResponse_commands Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCommands_nextToken
          Lens..~ rs
          Lens.^? listCommandsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListCommands where
  type AWSResponse ListCommands = ListCommandsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCommandsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Commands" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCommands

instance Core.NFData ListCommands

instance Core.ToHeaders ListCommands where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.ListCommands" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCommands where
  toJSON ListCommands' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("InstanceId" Core..=) Core.<$> instanceId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CommandId" Core..=) Core.<$> commandId,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath ListCommands where
  toPath = Core.const "/"

instance Core.ToQuery ListCommands where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCommandsResponse' smart constructor.
data ListCommandsResponse = ListCommandsResponse'
  { -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | (Optional) The list of commands requested by the user.
    commands :: Core.Maybe [Command],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListCommandsResponse
newListCommandsResponse pHttpStatus_ =
  ListCommandsResponse'
    { nextToken = Core.Nothing,
      commands = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommandsResponse_nextToken :: Lens.Lens' ListCommandsResponse (Core.Maybe Core.Text)
listCommandsResponse_nextToken = Lens.lens (\ListCommandsResponse' {nextToken} -> nextToken) (\s@ListCommandsResponse' {} a -> s {nextToken = a} :: ListCommandsResponse)

-- | (Optional) The list of commands requested by the user.
listCommandsResponse_commands :: Lens.Lens' ListCommandsResponse (Core.Maybe [Command])
listCommandsResponse_commands = Lens.lens (\ListCommandsResponse' {commands} -> commands) (\s@ListCommandsResponse' {} a -> s {commands = a} :: ListCommandsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCommandsResponse_httpStatus :: Lens.Lens' ListCommandsResponse Core.Int
listCommandsResponse_httpStatus = Lens.lens (\ListCommandsResponse' {httpStatus} -> httpStatus) (\s@ListCommandsResponse' {} a -> s {httpStatus = a} :: ListCommandsResponse)

instance Core.NFData ListCommandsResponse
