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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    routingStrategyType :: Prelude.Maybe RoutingStrategyType,
    -- | A descriptive label that is associated with an alias. Alias names do not
    -- need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      routingStrategyType = Prelude.Nothing,
      name = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listAliases_nextToken :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
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
listAliases_routingStrategyType :: Lens.Lens' ListAliases (Prelude.Maybe RoutingStrategyType)
listAliases_routingStrategyType = Lens.lens (\ListAliases' {routingStrategyType} -> routingStrategyType) (\s@ListAliases' {} a -> s {routingStrategyType = a} :: ListAliases)

-- | A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
listAliases_name :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_name = Lens.lens (\ListAliases' {name} -> name) (\s@ListAliases' {} a -> s {name = a} :: ListAliases)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listAliases_limit :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Natural)
listAliases_limit = Lens.lens (\ListAliases' {limit} -> limit) (\s@ListAliases' {} a -> s {limit = a} :: ListAliases)

instance Core.AWSPager ListAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_aliases Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAliases_nextToken
          Lens..~ rs
          Lens.^? listAliasesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAliases where
  type AWSResponse ListAliases = ListAliasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Aliases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAliases

instance Prelude.NFData ListAliases

instance Core.ToHeaders ListAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ListAliases" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAliases where
  toJSON ListAliases' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("RoutingStrategyType" Core..=)
              Prelude.<$> routingStrategyType,
            ("Name" Core..=) Prelude.<$> name,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListAliases where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAliases where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of alias resources that match the request parameters.
    aliases :: Prelude.Maybe [Alias],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListAliasesResponse
newListAliasesResponse pHttpStatus_ =
  ListAliasesResponse'
    { nextToken = Prelude.Nothing,
      aliases = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listAliasesResponse_nextToken :: Lens.Lens' ListAliasesResponse (Prelude.Maybe Prelude.Text)
listAliasesResponse_nextToken = Lens.lens (\ListAliasesResponse' {nextToken} -> nextToken) (\s@ListAliasesResponse' {} a -> s {nextToken = a} :: ListAliasesResponse)

-- | A collection of alias resources that match the request parameters.
listAliasesResponse_aliases :: Lens.Lens' ListAliasesResponse (Prelude.Maybe [Alias])
listAliasesResponse_aliases = Lens.lens (\ListAliasesResponse' {aliases} -> aliases) (\s@ListAliasesResponse' {} a -> s {aliases = a} :: ListAliasesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAliasesResponse_httpStatus :: Lens.Lens' ListAliasesResponse Prelude.Int
listAliasesResponse_httpStatus = Lens.lens (\ListAliasesResponse' {httpStatus} -> httpStatus) (\s@ListAliasesResponse' {} a -> s {httpStatus = a} :: ListAliasesResponse)

instance Prelude.NFData ListAliasesResponse
