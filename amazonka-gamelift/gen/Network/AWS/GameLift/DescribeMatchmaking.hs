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
-- Module      : Network.AWS.GameLift.DescribeMatchmaking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- an Amazon Simple Notification Service (SNS) to receive notifications,
-- and provide the topic ARN in the matchmaking configuration. Continuously
-- poling ticket status with DescribeMatchmaking should only be used for
-- games in development with low matchmaking usage.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a Game Client>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch Event Notification>
--
-- __Related operations__
--
-- -   StartMatchmaking
--
-- -   DescribeMatchmaking
--
-- -   StopMatchmaking
--
-- -   AcceptMatch
--
-- -   StartMatchBackfill
module Network.AWS.GameLift.DescribeMatchmaking
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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeMatchmaking' smart constructor.
data DescribeMatchmaking = DescribeMatchmaking'
  { -- | A unique identifier for a matchmaking ticket. You can include up to 10
    -- ID values.
    ticketIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeMatchmaking_ticketIds = Lens.lens (\DescribeMatchmaking' {ticketIds} -> ticketIds) (\s@DescribeMatchmaking' {} a -> s {ticketIds = a} :: DescribeMatchmaking) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DescribeMatchmaking where
  type
    Rs DescribeMatchmaking =
      DescribeMatchmakingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMatchmakingResponse'
            Prelude.<$> ( x Prelude..?> "TicketList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMatchmaking

instance Prelude.NFData DescribeMatchmaking

instance Prelude.ToHeaders DescribeMatchmaking where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.DescribeMatchmaking" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeMatchmaking where
  toJSON DescribeMatchmaking' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("TicketIds" Prelude..= ticketIds)]
      )

instance Prelude.ToPath DescribeMatchmaking where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeMatchmaking where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeMatchmakingResponse' smart constructor.
data DescribeMatchmakingResponse = DescribeMatchmakingResponse'
  { -- | A collection of existing matchmaking ticket objects matching the
    -- request.
    ticketList :: Prelude.Maybe [MatchmakingTicket],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeMatchmakingResponse_ticketList = Lens.lens (\DescribeMatchmakingResponse' {ticketList} -> ticketList) (\s@DescribeMatchmakingResponse' {} a -> s {ticketList = a} :: DescribeMatchmakingResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeMatchmakingResponse_httpStatus :: Lens.Lens' DescribeMatchmakingResponse Prelude.Int
describeMatchmakingResponse_httpStatus = Lens.lens (\DescribeMatchmakingResponse' {httpStatus} -> httpStatus) (\s@DescribeMatchmakingResponse' {} a -> s {httpStatus = a} :: DescribeMatchmakingResponse)

instance Prelude.NFData DescribeMatchmakingResponse
