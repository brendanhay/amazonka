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
-- Module      : Network.AWS.AppStream.DescribeSessions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the streaming sessions for a specified
-- stack and fleet. If a UserId is provided for the stack and fleet, only
-- streaming sessions for that user are described. If an authentication
-- type is not provided, the default is to authenticate users using a
-- streaming URL.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeSessions
  ( -- * Creating a Request
    DescribeSessions (..),
    newDescribeSessions,

    -- * Request Lenses
    describeSessions_nextToken,
    describeSessions_userId,
    describeSessions_authenticationType,
    describeSessions_limit,
    describeSessions_stackName,
    describeSessions_fleetName,

    -- * Destructuring the Response
    DescribeSessionsResponse (..),
    newDescribeSessionsResponse,

    -- * Response Lenses
    describeSessionsResponse_nextToken,
    describeSessionsResponse_sessions,
    describeSessionsResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSessions' smart constructor.
data DescribeSessions = DescribeSessions'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The user identifier (ID). If you specify a user ID, you must also
    -- specify the authentication type.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The authentication method. Specify @API@ for a user authenticated using
    -- a streaming URL or @SAML@ for a SAML federated user. The default is to
    -- authenticate users using a streaming URL.
    authenticationType :: Prelude.Maybe AuthenticationType,
    -- | The size of each page of results. The default value is 20 and the
    -- maximum value is 50.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The name of the stack. This value is case-sensitive.
    stackName :: Prelude.Text,
    -- | The name of the fleet. This value is case-sensitive.
    fleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSessions_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'userId', 'describeSessions_userId' - The user identifier (ID). If you specify a user ID, you must also
-- specify the authentication type.
--
-- 'authenticationType', 'describeSessions_authenticationType' - The authentication method. Specify @API@ for a user authenticated using
-- a streaming URL or @SAML@ for a SAML federated user. The default is to
-- authenticate users using a streaming URL.
--
-- 'limit', 'describeSessions_limit' - The size of each page of results. The default value is 20 and the
-- maximum value is 50.
--
-- 'stackName', 'describeSessions_stackName' - The name of the stack. This value is case-sensitive.
--
-- 'fleetName', 'describeSessions_fleetName' - The name of the fleet. This value is case-sensitive.
newDescribeSessions ::
  -- | 'stackName'
  Prelude.Text ->
  -- | 'fleetName'
  Prelude.Text ->
  DescribeSessions
newDescribeSessions pStackName_ pFleetName_ =
  DescribeSessions'
    { nextToken = Prelude.Nothing,
      userId = Prelude.Nothing,
      authenticationType = Prelude.Nothing,
      limit = Prelude.Nothing,
      stackName = pStackName_,
      fleetName = pFleetName_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeSessions_nextToken :: Lens.Lens' DescribeSessions (Prelude.Maybe Prelude.Text)
describeSessions_nextToken = Lens.lens (\DescribeSessions' {nextToken} -> nextToken) (\s@DescribeSessions' {} a -> s {nextToken = a} :: DescribeSessions)

-- | The user identifier (ID). If you specify a user ID, you must also
-- specify the authentication type.
describeSessions_userId :: Lens.Lens' DescribeSessions (Prelude.Maybe Prelude.Text)
describeSessions_userId = Lens.lens (\DescribeSessions' {userId} -> userId) (\s@DescribeSessions' {} a -> s {userId = a} :: DescribeSessions)

-- | The authentication method. Specify @API@ for a user authenticated using
-- a streaming URL or @SAML@ for a SAML federated user. The default is to
-- authenticate users using a streaming URL.
describeSessions_authenticationType :: Lens.Lens' DescribeSessions (Prelude.Maybe AuthenticationType)
describeSessions_authenticationType = Lens.lens (\DescribeSessions' {authenticationType} -> authenticationType) (\s@DescribeSessions' {} a -> s {authenticationType = a} :: DescribeSessions)

-- | The size of each page of results. The default value is 20 and the
-- maximum value is 50.
describeSessions_limit :: Lens.Lens' DescribeSessions (Prelude.Maybe Prelude.Int)
describeSessions_limit = Lens.lens (\DescribeSessions' {limit} -> limit) (\s@DescribeSessions' {} a -> s {limit = a} :: DescribeSessions)

-- | The name of the stack. This value is case-sensitive.
describeSessions_stackName :: Lens.Lens' DescribeSessions Prelude.Text
describeSessions_stackName = Lens.lens (\DescribeSessions' {stackName} -> stackName) (\s@DescribeSessions' {} a -> s {stackName = a} :: DescribeSessions)

-- | The name of the fleet. This value is case-sensitive.
describeSessions_fleetName :: Lens.Lens' DescribeSessions Prelude.Text
describeSessions_fleetName = Lens.lens (\DescribeSessions' {fleetName} -> fleetName) (\s@DescribeSessions' {} a -> s {fleetName = a} :: DescribeSessions)

instance Core.AWSPager DescribeSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSessionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSessionsResponse_sessions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSessions_nextToken
          Lens..~ rs
          Lens.^? describeSessionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeSessions where
  type
    AWSResponse DescribeSessions =
      DescribeSessionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSessionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Sessions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSessions

instance Prelude.NFData DescribeSessions

instance Core.ToHeaders DescribeSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSessions where
  toJSON DescribeSessions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("UserId" Core..=) Prelude.<$> userId,
            ("AuthenticationType" Core..=)
              Prelude.<$> authenticationType,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("StackName" Core..= stackName),
            Prelude.Just ("FleetName" Core..= fleetName)
          ]
      )

instance Core.ToPath DescribeSessions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSessions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSessionsResponse' smart constructor.
data DescribeSessionsResponse = DescribeSessionsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the streaming sessions.
    sessions :: Prelude.Maybe [Session],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSessionsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'sessions', 'describeSessionsResponse_sessions' - Information about the streaming sessions.
--
-- 'httpStatus', 'describeSessionsResponse_httpStatus' - The response's http status code.
newDescribeSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSessionsResponse
newDescribeSessionsResponse pHttpStatus_ =
  DescribeSessionsResponse'
    { nextToken =
        Prelude.Nothing,
      sessions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeSessionsResponse_nextToken :: Lens.Lens' DescribeSessionsResponse (Prelude.Maybe Prelude.Text)
describeSessionsResponse_nextToken = Lens.lens (\DescribeSessionsResponse' {nextToken} -> nextToken) (\s@DescribeSessionsResponse' {} a -> s {nextToken = a} :: DescribeSessionsResponse)

-- | Information about the streaming sessions.
describeSessionsResponse_sessions :: Lens.Lens' DescribeSessionsResponse (Prelude.Maybe [Session])
describeSessionsResponse_sessions = Lens.lens (\DescribeSessionsResponse' {sessions} -> sessions) (\s@DescribeSessionsResponse' {} a -> s {sessions = a} :: DescribeSessionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSessionsResponse_httpStatus :: Lens.Lens' DescribeSessionsResponse Prelude.Int
describeSessionsResponse_httpStatus = Lens.lens (\DescribeSessionsResponse' {httpStatus} -> httpStatus) (\s@DescribeSessionsResponse' {} a -> s {httpStatus = a} :: DescribeSessionsResponse)

instance Prelude.NFData DescribeSessionsResponse
