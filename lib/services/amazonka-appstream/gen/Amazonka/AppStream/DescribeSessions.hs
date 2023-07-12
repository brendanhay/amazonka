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
-- Module      : Amazonka.AppStream.DescribeSessions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.AppStream.DescribeSessions
  ( -- * Creating a Request
    DescribeSessions (..),
    newDescribeSessions,

    -- * Request Lenses
    describeSessions_authenticationType,
    describeSessions_limit,
    describeSessions_nextToken,
    describeSessions_userId,
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSessions' smart constructor.
data DescribeSessions = DescribeSessions'
  { -- | The authentication method. Specify @API@ for a user authenticated using
    -- a streaming URL or @SAML@ for a SAML federated user. The default is to
    -- authenticate users using a streaming URL.
    authenticationType :: Prelude.Maybe AuthenticationType,
    -- | The size of each page of results. The default value is 20 and the
    -- maximum value is 50.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The user identifier (ID). If you specify a user ID, you must also
    -- specify the authentication type.
    userId :: Prelude.Maybe Prelude.Text,
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
-- 'authenticationType', 'describeSessions_authenticationType' - The authentication method. Specify @API@ for a user authenticated using
-- a streaming URL or @SAML@ for a SAML federated user. The default is to
-- authenticate users using a streaming URL.
--
-- 'limit', 'describeSessions_limit' - The size of each page of results. The default value is 20 and the
-- maximum value is 50.
--
-- 'nextToken', 'describeSessions_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'userId', 'describeSessions_userId' - The user identifier (ID). If you specify a user ID, you must also
-- specify the authentication type.
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
    { authenticationType =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      userId = Prelude.Nothing,
      stackName = pStackName_,
      fleetName = pFleetName_
    }

-- | The authentication method. Specify @API@ for a user authenticated using
-- a streaming URL or @SAML@ for a SAML federated user. The default is to
-- authenticate users using a streaming URL.
describeSessions_authenticationType :: Lens.Lens' DescribeSessions (Prelude.Maybe AuthenticationType)
describeSessions_authenticationType = Lens.lens (\DescribeSessions' {authenticationType} -> authenticationType) (\s@DescribeSessions' {} a -> s {authenticationType = a} :: DescribeSessions)

-- | The size of each page of results. The default value is 20 and the
-- maximum value is 50.
describeSessions_limit :: Lens.Lens' DescribeSessions (Prelude.Maybe Prelude.Int)
describeSessions_limit = Lens.lens (\DescribeSessions' {limit} -> limit) (\s@DescribeSessions' {} a -> s {limit = a} :: DescribeSessions)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeSessions_nextToken :: Lens.Lens' DescribeSessions (Prelude.Maybe Prelude.Text)
describeSessions_nextToken = Lens.lens (\DescribeSessions' {nextToken} -> nextToken) (\s@DescribeSessions' {} a -> s {nextToken = a} :: DescribeSessions)

-- | The user identifier (ID). If you specify a user ID, you must also
-- specify the authentication type.
describeSessions_userId :: Lens.Lens' DescribeSessions (Prelude.Maybe Prelude.Text)
describeSessions_userId = Lens.lens (\DescribeSessions' {userId} -> userId) (\s@DescribeSessions' {} a -> s {userId = a} :: DescribeSessions)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeSessions_nextToken
          Lens..~ rs
          Lens.^? describeSessionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeSessions where
  type
    AWSResponse DescribeSessions =
      DescribeSessionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSessionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Sessions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSessions where
  hashWithSalt _salt DescribeSessions' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` fleetName

instance Prelude.NFData DescribeSessions where
  rnf DescribeSessions' {..} =
    Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf fleetName

instance Data.ToHeaders DescribeSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DescribeSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSessions where
  toJSON DescribeSessions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthenticationType" Data..=)
              Prelude.<$> authenticationType,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("UserId" Data..=) Prelude.<$> userId,
            Prelude.Just ("StackName" Data..= stackName),
            Prelude.Just ("FleetName" Data..= fleetName)
          ]
      )

instance Data.ToPath DescribeSessions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSessions where
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
describeSessionsResponse_sessions = Lens.lens (\DescribeSessionsResponse' {sessions} -> sessions) (\s@DescribeSessionsResponse' {} a -> s {sessions = a} :: DescribeSessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSessionsResponse_httpStatus :: Lens.Lens' DescribeSessionsResponse Prelude.Int
describeSessionsResponse_httpStatus = Lens.lens (\DescribeSessionsResponse' {httpStatus} -> httpStatus) (\s@DescribeSessionsResponse' {} a -> s {httpStatus = a} :: DescribeSessionsResponse)

instance Prelude.NFData DescribeSessionsResponse where
  rnf DescribeSessionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sessions
      `Prelude.seq` Prelude.rnf httpStatus
