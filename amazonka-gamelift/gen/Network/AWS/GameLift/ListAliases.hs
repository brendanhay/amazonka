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
-- Module      : Network.AWS.GameLift.ListAliases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all aliases for this AWS account. You can filter the result
-- set by alias name and\/or routing strategy type. Use the pagination
-- parameters to retrieve results in sequential pages.
--
-- Returned aliases are not listed in any particular order.
--
-- -   CreateAlias
--
-- -   ListAliases
--
-- -   DescribeAlias
--
-- -   UpdateAlias
--
-- -   DeleteAlias
--
-- -   ResolveAlias
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListAliases
  ( -- * Creating a Request
    ListAliases (..),
    newListAliases,

    -- * Request Lenses
    listAliases_nextToken,
    listAliases_routingStrategyType,
    listAliases_name,
    listAliases_limit,

    -- * Destructuring the Response
    ListAliasesResponse (..),
    newListAliasesResponse,

    -- * Response Lenses
    listAliasesResponse_nextToken,
    listAliasesResponse_aliases,
    listAliasesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Core.Text,
    -- | The routing type to filter results on. Use this parameter to retrieve
    -- only aliases with a certain routing type. To retrieve all aliases, leave
    -- this parameter empty.
    --
    -- Possible routing types include the following:
    --
    -- -   __SIMPLE__ -- The alias resolves to one specific fleet. Use this
    --     type when routing to active fleets.
    --
    -- -   __TERMINAL__ -- The alias does not resolve to a fleet but instead
    --     can be used to display a message to the user. A terminal alias
    --     throws a TerminalRoutingStrategyException with the RoutingStrategy
    --     message embedded.
    routingStrategyType :: Core.Maybe RoutingStrategyType,
    -- | A descriptive label that is associated with an alias. Alias names do not
    -- need to be unique.
    name :: Core.Maybe Core.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAliases_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'routingStrategyType', 'listAliases_routingStrategyType' - The routing type to filter results on. Use this parameter to retrieve
-- only aliases with a certain routing type. To retrieve all aliases, leave
-- this parameter empty.
--
-- Possible routing types include the following:
--
-- -   __SIMPLE__ -- The alias resolves to one specific fleet. Use this
--     type when routing to active fleets.
--
-- -   __TERMINAL__ -- The alias does not resolve to a fleet but instead
--     can be used to display a message to the user. A terminal alias
--     throws a TerminalRoutingStrategyException with the RoutingStrategy
--     message embedded.
--
-- 'name', 'listAliases_name' - A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
--
-- 'limit', 'listAliases_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
newListAliases ::
  ListAliases
newListAliases =
  ListAliases'
    { nextToken = Core.Nothing,
      routingStrategyType = Core.Nothing,
      name = Core.Nothing,
      limit = Core.Nothing
    }

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listAliases_nextToken :: Lens.Lens' ListAliases (Core.Maybe Core.Text)
listAliases_nextToken = Lens.lens (\ListAliases' {nextToken} -> nextToken) (\s@ListAliases' {} a -> s {nextToken = a} :: ListAliases)

-- | The routing type to filter results on. Use this parameter to retrieve
-- only aliases with a certain routing type. To retrieve all aliases, leave
-- this parameter empty.
--
-- Possible routing types include the following:
--
-- -   __SIMPLE__ -- The alias resolves to one specific fleet. Use this
--     type when routing to active fleets.
--
-- -   __TERMINAL__ -- The alias does not resolve to a fleet but instead
--     can be used to display a message to the user. A terminal alias
--     throws a TerminalRoutingStrategyException with the RoutingStrategy
--     message embedded.
listAliases_routingStrategyType :: Lens.Lens' ListAliases (Core.Maybe RoutingStrategyType)
listAliases_routingStrategyType = Lens.lens (\ListAliases' {routingStrategyType} -> routingStrategyType) (\s@ListAliases' {} a -> s {routingStrategyType = a} :: ListAliases)

-- | A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
listAliases_name :: Lens.Lens' ListAliases (Core.Maybe Core.Text)
listAliases_name = Lens.lens (\ListAliases' {name} -> name) (\s@ListAliases' {} a -> s {name = a} :: ListAliases)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listAliases_limit :: Lens.Lens' ListAliases (Core.Maybe Core.Natural)
listAliases_limit = Lens.lens (\ListAliases' {limit} -> limit) (\s@ListAliases' {} a -> s {limit = a} :: ListAliases)

instance Core.AWSPager ListAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_aliases Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAliases_nextToken
          Lens..~ rs
          Lens.^? listAliasesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListAliases where
  type AWSResponse ListAliases = ListAliasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Aliases" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAliases

instance Core.NFData ListAliases

instance Core.ToHeaders ListAliases where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ListAliases" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAliases where
  toJSON ListAliases' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("RoutingStrategyType" Core..=)
              Core.<$> routingStrategyType,
            ("Name" Core..=) Core.<$> name,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListAliases where
  toPath = Core.const "/"

instance Core.ToQuery ListAliases where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A collection of alias resources that match the request parameters.
    aliases :: Core.Maybe [Alias],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAliasesResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'aliases', 'listAliasesResponse_aliases' - A collection of alias resources that match the request parameters.
--
-- 'httpStatus', 'listAliasesResponse_httpStatus' - The response's http status code.
newListAliasesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAliasesResponse
newListAliasesResponse pHttpStatus_ =
  ListAliasesResponse'
    { nextToken = Core.Nothing,
      aliases = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listAliasesResponse_nextToken :: Lens.Lens' ListAliasesResponse (Core.Maybe Core.Text)
listAliasesResponse_nextToken = Lens.lens (\ListAliasesResponse' {nextToken} -> nextToken) (\s@ListAliasesResponse' {} a -> s {nextToken = a} :: ListAliasesResponse)

-- | A collection of alias resources that match the request parameters.
listAliasesResponse_aliases :: Lens.Lens' ListAliasesResponse (Core.Maybe [Alias])
listAliasesResponse_aliases = Lens.lens (\ListAliasesResponse' {aliases} -> aliases) (\s@ListAliasesResponse' {} a -> s {aliases = a} :: ListAliasesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAliasesResponse_httpStatus :: Lens.Lens' ListAliasesResponse Core.Int
listAliasesResponse_httpStatus = Lens.lens (\ListAliasesResponse' {httpStatus} -> httpStatus) (\s@ListAliasesResponse' {} a -> s {httpStatus = a} :: ListAliasesResponse)

instance Core.NFData ListAliasesResponse
