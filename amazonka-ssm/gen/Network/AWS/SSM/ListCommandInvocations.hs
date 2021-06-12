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
-- Module      : Network.AWS.SSM.ListCommandInvocations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An invocation is copy of a command sent to a specific instance. A
-- command can apply to one or more instances. A command invocation applies
-- to one instance. For example, if a user runs SendCommand against three
-- instances, then a command invocation is created for each requested
-- instance ID. ListCommandInvocations provide status about command
-- execution.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListCommandInvocations
  ( -- * Creating a Request
    ListCommandInvocations (..),
    newListCommandInvocations,

    -- * Request Lenses
    listCommandInvocations_nextToken,
    listCommandInvocations_instanceId,
    listCommandInvocations_maxResults,
    listCommandInvocations_commandId,
    listCommandInvocations_details,
    listCommandInvocations_filters,

    -- * Destructuring the Response
    ListCommandInvocationsResponse (..),
    newListCommandInvocationsResponse,

    -- * Response Lenses
    listCommandInvocationsResponse_nextToken,
    listCommandInvocationsResponse_commandInvocations,
    listCommandInvocationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListCommandInvocations' smart constructor.
data ListCommandInvocations = ListCommandInvocations'
  { -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | (Optional) The command execution details for a specific instance ID.
    instanceId :: Core.Maybe Core.Text,
    -- | (Optional) The maximum number of items to return for this call. The call
    -- also returns a token that you can specify in a subsequent call to get
    -- the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | (Optional) The invocations for a specific command ID.
    commandId :: Core.Maybe Core.Text,
    -- | (Optional) If set this returns the response of the command executions
    -- and any command output. By default this is set to False.
    details :: Core.Maybe Core.Bool,
    -- | (Optional) One or more filters. Use a filter to return a more specific
    -- list of results.
    filters :: Core.Maybe (Core.NonEmpty CommandFilter)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'instanceId', 'listCommandInvocations_instanceId' - (Optional) The command execution details for a specific instance ID.
--
-- 'maxResults', 'listCommandInvocations_maxResults' - (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
--
-- 'commandId', 'listCommandInvocations_commandId' - (Optional) The invocations for a specific command ID.
--
-- 'details', 'listCommandInvocations_details' - (Optional) If set this returns the response of the command executions
-- and any command output. By default this is set to False.
--
-- 'filters', 'listCommandInvocations_filters' - (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
newListCommandInvocations ::
  ListCommandInvocations
newListCommandInvocations =
  ListCommandInvocations'
    { nextToken = Core.Nothing,
      instanceId = Core.Nothing,
      maxResults = Core.Nothing,
      commandId = Core.Nothing,
      details = Core.Nothing,
      filters = Core.Nothing
    }

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommandInvocations_nextToken :: Lens.Lens' ListCommandInvocations (Core.Maybe Core.Text)
listCommandInvocations_nextToken = Lens.lens (\ListCommandInvocations' {nextToken} -> nextToken) (\s@ListCommandInvocations' {} a -> s {nextToken = a} :: ListCommandInvocations)

-- | (Optional) The command execution details for a specific instance ID.
listCommandInvocations_instanceId :: Lens.Lens' ListCommandInvocations (Core.Maybe Core.Text)
listCommandInvocations_instanceId = Lens.lens (\ListCommandInvocations' {instanceId} -> instanceId) (\s@ListCommandInvocations' {} a -> s {instanceId = a} :: ListCommandInvocations)

-- | (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
listCommandInvocations_maxResults :: Lens.Lens' ListCommandInvocations (Core.Maybe Core.Natural)
listCommandInvocations_maxResults = Lens.lens (\ListCommandInvocations' {maxResults} -> maxResults) (\s@ListCommandInvocations' {} a -> s {maxResults = a} :: ListCommandInvocations)

-- | (Optional) The invocations for a specific command ID.
listCommandInvocations_commandId :: Lens.Lens' ListCommandInvocations (Core.Maybe Core.Text)
listCommandInvocations_commandId = Lens.lens (\ListCommandInvocations' {commandId} -> commandId) (\s@ListCommandInvocations' {} a -> s {commandId = a} :: ListCommandInvocations)

-- | (Optional) If set this returns the response of the command executions
-- and any command output. By default this is set to False.
listCommandInvocations_details :: Lens.Lens' ListCommandInvocations (Core.Maybe Core.Bool)
listCommandInvocations_details = Lens.lens (\ListCommandInvocations' {details} -> details) (\s@ListCommandInvocations' {} a -> s {details = a} :: ListCommandInvocations)

-- | (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
listCommandInvocations_filters :: Lens.Lens' ListCommandInvocations (Core.Maybe (Core.NonEmpty CommandFilter))
listCommandInvocations_filters = Lens.lens (\ListCommandInvocations' {filters} -> filters) (\s@ListCommandInvocations' {} a -> s {filters = a} :: ListCommandInvocations) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListCommandInvocations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCommandInvocationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listCommandInvocationsResponse_commandInvocations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCommandInvocations_nextToken
          Lens..~ rs
          Lens.^? listCommandInvocationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListCommandInvocations where
  type
    AWSResponse ListCommandInvocations =
      ListCommandInvocationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCommandInvocationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "CommandInvocations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCommandInvocations

instance Core.NFData ListCommandInvocations

instance Core.ToHeaders ListCommandInvocations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListCommandInvocations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCommandInvocations where
  toJSON ListCommandInvocations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("InstanceId" Core..=) Core.<$> instanceId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CommandId" Core..=) Core.<$> commandId,
            ("Details" Core..=) Core.<$> details,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath ListCommandInvocations where
  toPath = Core.const "/"

instance Core.ToQuery ListCommandInvocations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCommandInvocationsResponse' smart constructor.
data ListCommandInvocationsResponse = ListCommandInvocationsResponse'
  { -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | (Optional) A list of all invocations.
    commandInvocations :: Core.Maybe [CommandInvocation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListCommandInvocationsResponse
newListCommandInvocationsResponse pHttpStatus_ =
  ListCommandInvocationsResponse'
    { nextToken =
        Core.Nothing,
      commandInvocations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommandInvocationsResponse_nextToken :: Lens.Lens' ListCommandInvocationsResponse (Core.Maybe Core.Text)
listCommandInvocationsResponse_nextToken = Lens.lens (\ListCommandInvocationsResponse' {nextToken} -> nextToken) (\s@ListCommandInvocationsResponse' {} a -> s {nextToken = a} :: ListCommandInvocationsResponse)

-- | (Optional) A list of all invocations.
listCommandInvocationsResponse_commandInvocations :: Lens.Lens' ListCommandInvocationsResponse (Core.Maybe [CommandInvocation])
listCommandInvocationsResponse_commandInvocations = Lens.lens (\ListCommandInvocationsResponse' {commandInvocations} -> commandInvocations) (\s@ListCommandInvocationsResponse' {} a -> s {commandInvocations = a} :: ListCommandInvocationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCommandInvocationsResponse_httpStatus :: Lens.Lens' ListCommandInvocationsResponse Core.Int
listCommandInvocationsResponse_httpStatus = Lens.lens (\ListCommandInvocationsResponse' {httpStatus} -> httpStatus) (\s@ListCommandInvocationsResponse' {} a -> s {httpStatus = a} :: ListCommandInvocationsResponse)

instance Core.NFData ListCommandInvocationsResponse
