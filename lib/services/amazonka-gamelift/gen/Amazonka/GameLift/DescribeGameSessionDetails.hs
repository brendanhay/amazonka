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
-- Module      : Amazonka.GameLift.DescribeGameSessionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves additional game session properties, including the game session
-- protection policy in force, a set of one or more game sessions in a
-- specific fleet location. You can optionally filter the results by
-- current game session status.
--
-- This operation can be used in the following ways:
--
-- -   To retrieve details for all game sessions that are currently running
--     on all locations in a fleet, provide a fleet or alias ID, with an
--     optional status filter. This approach returns details from the
--     fleet\'s home Region and all remote locations.
--
-- -   To retrieve details for all game sessions that are currently running
--     on a specific fleet location, provide a fleet or alias ID and a
--     location name, with optional status filter. The location can be the
--     fleet\'s home Region or any remote location.
--
-- -   To retrieve details for a specific game session, provide the game
--     session ID. This approach looks for the game session ID in all
--     fleets that reside in the Amazon Web Services Region defined in the
--     request.
--
-- Use the pagination parameters to retrieve results as a set of sequential
-- pages.
--
-- If successful, a @GameSessionDetail@ object is returned for each game
-- session that matches the request.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-client-api.html#gamelift-sdk-client-api-find Find a game session>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.DescribeGameSessionDetails
  ( -- * Creating a Request
    DescribeGameSessionDetails (..),
    newDescribeGameSessionDetails,

    -- * Request Lenses
    describeGameSessionDetails_aliasId,
    describeGameSessionDetails_fleetId,
    describeGameSessionDetails_gameSessionId,
    describeGameSessionDetails_limit,
    describeGameSessionDetails_location,
    describeGameSessionDetails_nextToken,
    describeGameSessionDetails_statusFilter,

    -- * Destructuring the Response
    DescribeGameSessionDetailsResponse (..),
    newDescribeGameSessionDetailsResponse,

    -- * Response Lenses
    describeGameSessionDetailsResponse_gameSessionDetails,
    describeGameSessionDetailsResponse_nextToken,
    describeGameSessionDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGameSessionDetails' smart constructor.
data DescribeGameSessionDetails = DescribeGameSessionDetails'
  { -- | A unique identifier for the alias associated with the fleet to retrieve
    -- all game sessions for. You can use either the alias ID or ARN value.
    aliasId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet to retrieve all game sessions active
    -- on the fleet. You can use either the fleet ID or ARN value.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game session to retrieve.
    gameSessionId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A fleet location to get game session details for. You can specify a
    -- fleet\'s home Region or a remote location. Use the Amazon Web Services
    -- Region code format, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Game session status to filter results on. Possible game session statuses
    -- include @ACTIVE@, @TERMINATED@, @ACTIVATING@ and @TERMINATING@ (the last
    -- two are transitory).
    statusFilter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameSessionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'describeGameSessionDetails_aliasId' - A unique identifier for the alias associated with the fleet to retrieve
-- all game sessions for. You can use either the alias ID or ARN value.
--
-- 'fleetId', 'describeGameSessionDetails_fleetId' - A unique identifier for the fleet to retrieve all game sessions active
-- on the fleet. You can use either the fleet ID or ARN value.
--
-- 'gameSessionId', 'describeGameSessionDetails_gameSessionId' - A unique identifier for the game session to retrieve.
--
-- 'limit', 'describeGameSessionDetails_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'location', 'describeGameSessionDetails_location' - A fleet location to get game session details for. You can specify a
-- fleet\'s home Region or a remote location. Use the Amazon Web Services
-- Region code format, such as @us-west-2@.
--
-- 'nextToken', 'describeGameSessionDetails_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'statusFilter', 'describeGameSessionDetails_statusFilter' - Game session status to filter results on. Possible game session statuses
-- include @ACTIVE@, @TERMINATED@, @ACTIVATING@ and @TERMINATING@ (the last
-- two are transitory).
newDescribeGameSessionDetails ::
  DescribeGameSessionDetails
newDescribeGameSessionDetails =
  DescribeGameSessionDetails'
    { aliasId =
        Prelude.Nothing,
      fleetId = Prelude.Nothing,
      gameSessionId = Prelude.Nothing,
      limit = Prelude.Nothing,
      location = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      statusFilter = Prelude.Nothing
    }

-- | A unique identifier for the alias associated with the fleet to retrieve
-- all game sessions for. You can use either the alias ID or ARN value.
describeGameSessionDetails_aliasId :: Lens.Lens' DescribeGameSessionDetails (Prelude.Maybe Prelude.Text)
describeGameSessionDetails_aliasId = Lens.lens (\DescribeGameSessionDetails' {aliasId} -> aliasId) (\s@DescribeGameSessionDetails' {} a -> s {aliasId = a} :: DescribeGameSessionDetails)

-- | A unique identifier for the fleet to retrieve all game sessions active
-- on the fleet. You can use either the fleet ID or ARN value.
describeGameSessionDetails_fleetId :: Lens.Lens' DescribeGameSessionDetails (Prelude.Maybe Prelude.Text)
describeGameSessionDetails_fleetId = Lens.lens (\DescribeGameSessionDetails' {fleetId} -> fleetId) (\s@DescribeGameSessionDetails' {} a -> s {fleetId = a} :: DescribeGameSessionDetails)

-- | A unique identifier for the game session to retrieve.
describeGameSessionDetails_gameSessionId :: Lens.Lens' DescribeGameSessionDetails (Prelude.Maybe Prelude.Text)
describeGameSessionDetails_gameSessionId = Lens.lens (\DescribeGameSessionDetails' {gameSessionId} -> gameSessionId) (\s@DescribeGameSessionDetails' {} a -> s {gameSessionId = a} :: DescribeGameSessionDetails)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeGameSessionDetails_limit :: Lens.Lens' DescribeGameSessionDetails (Prelude.Maybe Prelude.Natural)
describeGameSessionDetails_limit = Lens.lens (\DescribeGameSessionDetails' {limit} -> limit) (\s@DescribeGameSessionDetails' {} a -> s {limit = a} :: DescribeGameSessionDetails)

-- | A fleet location to get game session details for. You can specify a
-- fleet\'s home Region or a remote location. Use the Amazon Web Services
-- Region code format, such as @us-west-2@.
describeGameSessionDetails_location :: Lens.Lens' DescribeGameSessionDetails (Prelude.Maybe Prelude.Text)
describeGameSessionDetails_location = Lens.lens (\DescribeGameSessionDetails' {location} -> location) (\s@DescribeGameSessionDetails' {} a -> s {location = a} :: DescribeGameSessionDetails)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeGameSessionDetails_nextToken :: Lens.Lens' DescribeGameSessionDetails (Prelude.Maybe Prelude.Text)
describeGameSessionDetails_nextToken = Lens.lens (\DescribeGameSessionDetails' {nextToken} -> nextToken) (\s@DescribeGameSessionDetails' {} a -> s {nextToken = a} :: DescribeGameSessionDetails)

-- | Game session status to filter results on. Possible game session statuses
-- include @ACTIVE@, @TERMINATED@, @ACTIVATING@ and @TERMINATING@ (the last
-- two are transitory).
describeGameSessionDetails_statusFilter :: Lens.Lens' DescribeGameSessionDetails (Prelude.Maybe Prelude.Text)
describeGameSessionDetails_statusFilter = Lens.lens (\DescribeGameSessionDetails' {statusFilter} -> statusFilter) (\s@DescribeGameSessionDetails' {} a -> s {statusFilter = a} :: DescribeGameSessionDetails)

instance Core.AWSPager DescribeGameSessionDetails where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGameSessionDetailsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGameSessionDetailsResponse_gameSessionDetails
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeGameSessionDetails_nextToken
          Lens..~ rs
          Lens.^? describeGameSessionDetailsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeGameSessionDetails where
  type
    AWSResponse DescribeGameSessionDetails =
      DescribeGameSessionDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameSessionDetailsResponse'
            Prelude.<$> ( x
                            Data..?> "GameSessionDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGameSessionDetails where
  hashWithSalt _salt DescribeGameSessionDetails' {..} =
    _salt
      `Prelude.hashWithSalt` aliasId
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` gameSessionId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` statusFilter

instance Prelude.NFData DescribeGameSessionDetails where
  rnf DescribeGameSessionDetails' {..} =
    Prelude.rnf aliasId
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf gameSessionId
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf statusFilter

instance Data.ToHeaders DescribeGameSessionDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeGameSessionDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeGameSessionDetails where
  toJSON DescribeGameSessionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AliasId" Data..=) Prelude.<$> aliasId,
            ("FleetId" Data..=) Prelude.<$> fleetId,
            ("GameSessionId" Data..=) Prelude.<$> gameSessionId,
            ("Limit" Data..=) Prelude.<$> limit,
            ("Location" Data..=) Prelude.<$> location,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StatusFilter" Data..=) Prelude.<$> statusFilter
          ]
      )

instance Data.ToPath DescribeGameSessionDetails where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeGameSessionDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGameSessionDetailsResponse' smart constructor.
data DescribeGameSessionDetailsResponse = DescribeGameSessionDetailsResponse'
  { -- | A collection of properties for each game session that matches the
    -- request.
    gameSessionDetails :: Prelude.Maybe [GameSessionDetail],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameSessionDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessionDetails', 'describeGameSessionDetailsResponse_gameSessionDetails' - A collection of properties for each game session that matches the
-- request.
--
-- 'nextToken', 'describeGameSessionDetailsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'describeGameSessionDetailsResponse_httpStatus' - The response's http status code.
newDescribeGameSessionDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGameSessionDetailsResponse
newDescribeGameSessionDetailsResponse pHttpStatus_ =
  DescribeGameSessionDetailsResponse'
    { gameSessionDetails =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of properties for each game session that matches the
-- request.
describeGameSessionDetailsResponse_gameSessionDetails :: Lens.Lens' DescribeGameSessionDetailsResponse (Prelude.Maybe [GameSessionDetail])
describeGameSessionDetailsResponse_gameSessionDetails = Lens.lens (\DescribeGameSessionDetailsResponse' {gameSessionDetails} -> gameSessionDetails) (\s@DescribeGameSessionDetailsResponse' {} a -> s {gameSessionDetails = a} :: DescribeGameSessionDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeGameSessionDetailsResponse_nextToken :: Lens.Lens' DescribeGameSessionDetailsResponse (Prelude.Maybe Prelude.Text)
describeGameSessionDetailsResponse_nextToken = Lens.lens (\DescribeGameSessionDetailsResponse' {nextToken} -> nextToken) (\s@DescribeGameSessionDetailsResponse' {} a -> s {nextToken = a} :: DescribeGameSessionDetailsResponse)

-- | The response's http status code.
describeGameSessionDetailsResponse_httpStatus :: Lens.Lens' DescribeGameSessionDetailsResponse Prelude.Int
describeGameSessionDetailsResponse_httpStatus = Lens.lens (\DescribeGameSessionDetailsResponse' {httpStatus} -> httpStatus) (\s@DescribeGameSessionDetailsResponse' {} a -> s {httpStatus = a} :: DescribeGameSessionDetailsResponse)

instance
  Prelude.NFData
    DescribeGameSessionDetailsResponse
  where
  rnf DescribeGameSessionDetailsResponse' {..} =
    Prelude.rnf gameSessionDetails
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
