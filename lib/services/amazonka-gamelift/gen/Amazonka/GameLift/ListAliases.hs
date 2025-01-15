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
-- Module      : Amazonka.GameLift.ListAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all aliases for this Amazon Web Services account. You can
-- filter the result set by alias name and\/or routing strategy type. Use
-- the pagination parameters to retrieve results in sequential pages.
--
-- Returned aliases are not listed in any particular order.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.ListAliases
  ( -- * Creating a Request
    ListAliases (..),
    newListAliases,

    -- * Request Lenses
    listAliases_limit,
    listAliases_name,
    listAliases_nextToken,
    listAliases_routingStrategyType,

    -- * Destructuring the Response
    ListAliasesResponse (..),
    newListAliasesResponse,

    -- * Response Lenses
    listAliasesResponse_aliases,
    listAliasesResponse_nextToken,
    listAliasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A descriptive label that is associated with an alias. Alias names do not
    -- need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates the start of the next sequential page of results.
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
    --     throws a TerminalRoutingStrategyException with the
    --     <https://docs.aws.amazon.com/gamelift/latest/apireference/API_RoutingStrategy.html RoutingStrategy>
    --     message embedded.
    routingStrategyType :: Prelude.Maybe RoutingStrategyType
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
-- 'limit', 'listAliases_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'name', 'listAliases_name' - A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
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
--     throws a TerminalRoutingStrategyException with the
--     <https://docs.aws.amazon.com/gamelift/latest/apireference/API_RoutingStrategy.html RoutingStrategy>
--     message embedded.
newListAliases ::
  ListAliases
newListAliases =
  ListAliases'
    { limit = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      routingStrategyType = Prelude.Nothing
    }

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listAliases_limit :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Natural)
listAliases_limit = Lens.lens (\ListAliases' {limit} -> limit) (\s@ListAliases' {} a -> s {limit = a} :: ListAliases)

-- | A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
listAliases_name :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_name = Lens.lens (\ListAliases' {name} -> name) (\s@ListAliases' {} a -> s {name = a} :: ListAliases)

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
--     throws a TerminalRoutingStrategyException with the
--     <https://docs.aws.amazon.com/gamelift/latest/apireference/API_RoutingStrategy.html RoutingStrategy>
--     message embedded.
listAliases_routingStrategyType :: Lens.Lens' ListAliases (Prelude.Maybe RoutingStrategyType)
listAliases_routingStrategyType = Lens.lens (\ListAliases' {routingStrategyType} -> routingStrategyType) (\s@ListAliases' {} a -> s {routingStrategyType = a} :: ListAliases)

instance Core.AWSPager ListAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_aliases
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listAliases_nextToken
              Lens..~ rs
              Lens.^? listAliasesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListAliases where
  type AWSResponse ListAliases = ListAliasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Prelude.<$> (x Data..?> "Aliases" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAliases where
  hashWithSalt _salt ListAliases' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` routingStrategyType

instance Prelude.NFData ListAliases where
  rnf ListAliases' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf routingStrategyType

instance Data.ToHeaders ListAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.ListAliases" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAliases where
  toJSON ListAliases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("Name" Data..=) Prelude.<$> name,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("RoutingStrategyType" Data..=)
              Prelude.<$> routingStrategyType
          ]
      )

instance Data.ToPath ListAliases where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAliases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | A collection of alias resources that match the request parameters.
    aliases :: Prelude.Maybe [Alias],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'aliases', 'listAliasesResponse_aliases' - A collection of alias resources that match the request parameters.
--
-- 'nextToken', 'listAliasesResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'listAliasesResponse_httpStatus' - The response's http status code.
newListAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAliasesResponse
newListAliasesResponse pHttpStatus_ =
  ListAliasesResponse'
    { aliases = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of alias resources that match the request parameters.
listAliasesResponse_aliases :: Lens.Lens' ListAliasesResponse (Prelude.Maybe [Alias])
listAliasesResponse_aliases = Lens.lens (\ListAliasesResponse' {aliases} -> aliases) (\s@ListAliasesResponse' {} a -> s {aliases = a} :: ListAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listAliasesResponse_nextToken :: Lens.Lens' ListAliasesResponse (Prelude.Maybe Prelude.Text)
listAliasesResponse_nextToken = Lens.lens (\ListAliasesResponse' {nextToken} -> nextToken) (\s@ListAliasesResponse' {} a -> s {nextToken = a} :: ListAliasesResponse)

-- | The response's http status code.
listAliasesResponse_httpStatus :: Lens.Lens' ListAliasesResponse Prelude.Int
listAliasesResponse_httpStatus = Lens.lens (\ListAliasesResponse' {httpStatus} -> httpStatus) (\s@ListAliasesResponse' {} a -> s {httpStatus = a} :: ListAliasesResponse)

instance Prelude.NFData ListAliasesResponse where
  rnf ListAliasesResponse' {..} =
    Prelude.rnf aliases `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
