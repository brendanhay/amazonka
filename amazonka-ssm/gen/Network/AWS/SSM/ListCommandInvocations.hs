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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListCommandInvocations' smart constructor.
data ListCommandInvocations = ListCommandInvocations'
  { -- | (Optional) The token for the next set of items to return. (You received
    -- this token from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The command execution details for a specific instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The maximum number of items to return for this call. The call
    -- also returns a token that you can specify in a subsequent call to get
    -- the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) The invocations for a specific command ID.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) If set this returns the response of the command executions
    -- and any command output. By default this is set to False.
    details :: Prelude.Maybe Prelude.Bool,
    -- | (Optional) One or more filters. Use a filter to return a more specific
    -- list of results.
    filters :: Prelude.Maybe (Prelude.NonEmpty CommandFilter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      commandId = Prelude.Nothing,
      details = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | (Optional) The token for the next set of items to return. (You received
-- this token from a previous call.)
listCommandInvocations_nextToken :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Text)
listCommandInvocations_nextToken = Lens.lens (\ListCommandInvocations' {nextToken} -> nextToken) (\s@ListCommandInvocations' {} a -> s {nextToken = a} :: ListCommandInvocations)

-- | (Optional) The command execution details for a specific instance ID.
listCommandInvocations_instanceId :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Text)
listCommandInvocations_instanceId = Lens.lens (\ListCommandInvocations' {instanceId} -> instanceId) (\s@ListCommandInvocations' {} a -> s {instanceId = a} :: ListCommandInvocations)

-- | (Optional) The maximum number of items to return for this call. The call
-- also returns a token that you can specify in a subsequent call to get
-- the next set of results.
listCommandInvocations_maxResults :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Natural)
listCommandInvocations_maxResults = Lens.lens (\ListCommandInvocations' {maxResults} -> maxResults) (\s@ListCommandInvocations' {} a -> s {maxResults = a} :: ListCommandInvocations)

-- | (Optional) The invocations for a specific command ID.
listCommandInvocations_commandId :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Text)
listCommandInvocations_commandId = Lens.lens (\ListCommandInvocations' {commandId} -> commandId) (\s@ListCommandInvocations' {} a -> s {commandId = a} :: ListCommandInvocations)

-- | (Optional) If set this returns the response of the command executions
-- and any command output. By default this is set to False.
listCommandInvocations_details :: Lens.Lens' ListCommandInvocations (Prelude.Maybe Prelude.Bool)
listCommandInvocations_details = Lens.lens (\ListCommandInvocations' {details} -> details) (\s@ListCommandInvocations' {} a -> s {details = a} :: ListCommandInvocations)

-- | (Optional) One or more filters. Use a filter to return a more specific
-- list of results.
listCommandInvocations_filters :: Lens.Lens' ListCommandInvocations (Prelude.Maybe (Prelude.NonEmpty CommandFilter))
listCommandInvocations_filters = Lens.lens (\ListCommandInvocations' {filters} -> filters) (\s@ListCommandInvocations' {} a -> s {filters = a} :: ListCommandInvocations) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager ListCommandInvocations where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listCommandInvocationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listCommandInvocationsResponse_commandInvocations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listCommandInvocations_nextToken
          Lens..~ rs
          Lens.^? listCommandInvocationsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListCommandInvocations where
  type
    Rs ListCommandInvocations =
      ListCommandInvocationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCommandInvocationsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "CommandInvocations"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCommandInvocations

instance Prelude.NFData ListCommandInvocations

instance Prelude.ToHeaders ListCommandInvocations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.ListCommandInvocations" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListCommandInvocations where
  toJSON ListCommandInvocations' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("InstanceId" Prelude..=) Prelude.<$> instanceId,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("CommandId" Prelude..=) Prelude.<$> commandId,
            ("Details" Prelude..=) Prelude.<$> details,
            ("Filters" Prelude..=) Prelude.<$> filters
          ]
      )

instance Prelude.ToPath ListCommandInvocations where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListCommandInvocations where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listCommandInvocationsResponse_commandInvocations = Lens.lens (\ListCommandInvocationsResponse' {commandInvocations} -> commandInvocations) (\s@ListCommandInvocationsResponse' {} a -> s {commandInvocations = a} :: ListCommandInvocationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listCommandInvocationsResponse_httpStatus :: Lens.Lens' ListCommandInvocationsResponse Prelude.Int
listCommandInvocationsResponse_httpStatus = Lens.lens (\ListCommandInvocationsResponse' {httpStatus} -> httpStatus) (\s@ListCommandInvocationsResponse' {} a -> s {httpStatus = a} :: ListCommandInvocationsResponse)

instance
  Prelude.NFData
    ListCommandInvocationsResponse
