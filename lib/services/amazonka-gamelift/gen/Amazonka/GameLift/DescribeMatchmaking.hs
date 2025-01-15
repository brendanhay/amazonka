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
-- Module      : Amazonka.GameLift.DescribeMatchmaking
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more matchmaking tickets. Use this operation to
-- retrieve ticket information, including--after a successful match is
-- made--connection information for the resulting new game session.
--
-- To request matchmaking tickets, provide a list of up to 10 ticket IDs.
-- If the request is successful, a ticket object is returned for each
-- requested ID that currently exists.
--
-- This operation is not designed to be continually called to track
-- matchmaking ticket status. This practice can cause you to exceed your
-- API limit, which results in errors. Instead, as a best practice, set up
-- an Amazon Simple Notification Service to receive notifications, and
-- provide the topic ARN in the matchmaking configuration.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a game client>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch event notification>
module Amazonka.GameLift.DescribeMatchmaking
  ( -- * Creating a Request
    DescribeMatchmaking (..),
    newDescribeMatchmaking,

    -- * Request Lenses
    describeMatchmaking_ticketIds,

    -- * Destructuring the Response
    DescribeMatchmakingResponse (..),
    newDescribeMatchmakingResponse,

    -- * Response Lenses
    describeMatchmakingResponse_ticketList,
    describeMatchmakingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMatchmaking' smart constructor.
data DescribeMatchmaking = DescribeMatchmaking'
  { -- | A unique identifier for a matchmaking ticket. You can include up to 10
    -- ID values.
    ticketIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMatchmaking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ticketIds', 'describeMatchmaking_ticketIds' - A unique identifier for a matchmaking ticket. You can include up to 10
-- ID values.
newDescribeMatchmaking ::
  DescribeMatchmaking
newDescribeMatchmaking =
  DescribeMatchmaking' {ticketIds = Prelude.mempty}

-- | A unique identifier for a matchmaking ticket. You can include up to 10
-- ID values.
describeMatchmaking_ticketIds :: Lens.Lens' DescribeMatchmaking [Prelude.Text]
describeMatchmaking_ticketIds = Lens.lens (\DescribeMatchmaking' {ticketIds} -> ticketIds) (\s@DescribeMatchmaking' {} a -> s {ticketIds = a} :: DescribeMatchmaking) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeMatchmaking where
  type
    AWSResponse DescribeMatchmaking =
      DescribeMatchmakingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMatchmakingResponse'
            Prelude.<$> (x Data..?> "TicketList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMatchmaking where
  hashWithSalt _salt DescribeMatchmaking' {..} =
    _salt `Prelude.hashWithSalt` ticketIds

instance Prelude.NFData DescribeMatchmaking where
  rnf DescribeMatchmaking' {..} = Prelude.rnf ticketIds

instance Data.ToHeaders DescribeMatchmaking where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeMatchmaking" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMatchmaking where
  toJSON DescribeMatchmaking' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TicketIds" Data..= ticketIds)]
      )

instance Data.ToPath DescribeMatchmaking where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMatchmaking where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMatchmakingResponse' smart constructor.
data DescribeMatchmakingResponse = DescribeMatchmakingResponse'
  { -- | A collection of existing matchmaking ticket objects matching the
    -- request.
    ticketList :: Prelude.Maybe [MatchmakingTicket],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMatchmakingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ticketList', 'describeMatchmakingResponse_ticketList' - A collection of existing matchmaking ticket objects matching the
-- request.
--
-- 'httpStatus', 'describeMatchmakingResponse_httpStatus' - The response's http status code.
newDescribeMatchmakingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMatchmakingResponse
newDescribeMatchmakingResponse pHttpStatus_ =
  DescribeMatchmakingResponse'
    { ticketList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of existing matchmaking ticket objects matching the
-- request.
describeMatchmakingResponse_ticketList :: Lens.Lens' DescribeMatchmakingResponse (Prelude.Maybe [MatchmakingTicket])
describeMatchmakingResponse_ticketList = Lens.lens (\DescribeMatchmakingResponse' {ticketList} -> ticketList) (\s@DescribeMatchmakingResponse' {} a -> s {ticketList = a} :: DescribeMatchmakingResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMatchmakingResponse_httpStatus :: Lens.Lens' DescribeMatchmakingResponse Prelude.Int
describeMatchmakingResponse_httpStatus = Lens.lens (\DescribeMatchmakingResponse' {httpStatus} -> httpStatus) (\s@DescribeMatchmakingResponse' {} a -> s {httpStatus = a} :: DescribeMatchmakingResponse)

instance Prelude.NFData DescribeMatchmakingResponse where
  rnf DescribeMatchmakingResponse' {..} =
    Prelude.rnf ticketList `Prelude.seq`
      Prelude.rnf httpStatus
