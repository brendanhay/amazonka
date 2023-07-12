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
-- Module      : Amazonka.CognitoIdentityProvider.AdminListUserAuthEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A history of user activity and any risks detected as part of Amazon
-- Cognito advanced security.
--
-- This operation returns paginated results.
module Amazonka.CognitoIdentityProvider.AdminListUserAuthEvents
  ( -- * Creating a Request
    AdminListUserAuthEvents (..),
    newAdminListUserAuthEvents,

    -- * Request Lenses
    adminListUserAuthEvents_maxResults,
    adminListUserAuthEvents_nextToken,
    adminListUserAuthEvents_userPoolId,
    adminListUserAuthEvents_username,

    -- * Destructuring the Response
    AdminListUserAuthEventsResponse (..),
    newAdminListUserAuthEventsResponse,

    -- * Response Lenses
    adminListUserAuthEventsResponse_authEvents,
    adminListUserAuthEventsResponse_nextToken,
    adminListUserAuthEventsResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAdminListUserAuthEvents' smart constructor.
data AdminListUserAuthEvents = AdminListUserAuthEvents'
  { -- | The maximum number of authentication events to return. Returns 60 events
    -- if you set @MaxResults@ to 0, or if you don\'t include a @MaxResults@
    -- parameter.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The user pool username or an alias.
    username :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminListUserAuthEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'adminListUserAuthEvents_maxResults' - The maximum number of authentication events to return. Returns 60 events
-- if you set @MaxResults@ to 0, or if you don\'t include a @MaxResults@
-- parameter.
--
-- 'nextToken', 'adminListUserAuthEvents_nextToken' - A pagination token.
--
-- 'userPoolId', 'adminListUserAuthEvents_userPoolId' - The user pool ID.
--
-- 'username', 'adminListUserAuthEvents_username' - The user pool username or an alias.
newAdminListUserAuthEvents ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminListUserAuthEvents
newAdminListUserAuthEvents pUserPoolId_ pUsername_ =
  AdminListUserAuthEvents'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      username = Data._Sensitive Lens.# pUsername_
    }

-- | The maximum number of authentication events to return. Returns 60 events
-- if you set @MaxResults@ to 0, or if you don\'t include a @MaxResults@
-- parameter.
adminListUserAuthEvents_maxResults :: Lens.Lens' AdminListUserAuthEvents (Prelude.Maybe Prelude.Natural)
adminListUserAuthEvents_maxResults = Lens.lens (\AdminListUserAuthEvents' {maxResults} -> maxResults) (\s@AdminListUserAuthEvents' {} a -> s {maxResults = a} :: AdminListUserAuthEvents)

-- | A pagination token.
adminListUserAuthEvents_nextToken :: Lens.Lens' AdminListUserAuthEvents (Prelude.Maybe Prelude.Text)
adminListUserAuthEvents_nextToken = Lens.lens (\AdminListUserAuthEvents' {nextToken} -> nextToken) (\s@AdminListUserAuthEvents' {} a -> s {nextToken = a} :: AdminListUserAuthEvents)

-- | The user pool ID.
adminListUserAuthEvents_userPoolId :: Lens.Lens' AdminListUserAuthEvents Prelude.Text
adminListUserAuthEvents_userPoolId = Lens.lens (\AdminListUserAuthEvents' {userPoolId} -> userPoolId) (\s@AdminListUserAuthEvents' {} a -> s {userPoolId = a} :: AdminListUserAuthEvents)

-- | The user pool username or an alias.
adminListUserAuthEvents_username :: Lens.Lens' AdminListUserAuthEvents Prelude.Text
adminListUserAuthEvents_username = Lens.lens (\AdminListUserAuthEvents' {username} -> username) (\s@AdminListUserAuthEvents' {} a -> s {username = a} :: AdminListUserAuthEvents) Prelude.. Data._Sensitive

instance Core.AWSPager AdminListUserAuthEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? adminListUserAuthEventsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? adminListUserAuthEventsResponse_authEvents
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& adminListUserAuthEvents_nextToken
          Lens..~ rs
          Lens.^? adminListUserAuthEventsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest AdminListUserAuthEvents where
  type
    AWSResponse AdminListUserAuthEvents =
      AdminListUserAuthEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminListUserAuthEventsResponse'
            Prelude.<$> (x Data..?> "AuthEvents" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminListUserAuthEvents where
  hashWithSalt _salt AdminListUserAuthEvents' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username

instance Prelude.NFData AdminListUserAuthEvents where
  rnf AdminListUserAuthEvents' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf username

instance Data.ToHeaders AdminListUserAuthEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminListUserAuthEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminListUserAuthEvents where
  toJSON AdminListUserAuthEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath AdminListUserAuthEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminListUserAuthEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminListUserAuthEventsResponse' smart constructor.
data AdminListUserAuthEventsResponse = AdminListUserAuthEventsResponse'
  { -- | The response object. It includes the @EventID@, @EventType@,
    -- @CreationDate@, @EventRisk@, and @EventResponse@.
    authEvents :: Prelude.Maybe [AuthEventType],
    -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminListUserAuthEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authEvents', 'adminListUserAuthEventsResponse_authEvents' - The response object. It includes the @EventID@, @EventType@,
-- @CreationDate@, @EventRisk@, and @EventResponse@.
--
-- 'nextToken', 'adminListUserAuthEventsResponse_nextToken' - A pagination token.
--
-- 'httpStatus', 'adminListUserAuthEventsResponse_httpStatus' - The response's http status code.
newAdminListUserAuthEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminListUserAuthEventsResponse
newAdminListUserAuthEventsResponse pHttpStatus_ =
  AdminListUserAuthEventsResponse'
    { authEvents =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response object. It includes the @EventID@, @EventType@,
-- @CreationDate@, @EventRisk@, and @EventResponse@.
adminListUserAuthEventsResponse_authEvents :: Lens.Lens' AdminListUserAuthEventsResponse (Prelude.Maybe [AuthEventType])
adminListUserAuthEventsResponse_authEvents = Lens.lens (\AdminListUserAuthEventsResponse' {authEvents} -> authEvents) (\s@AdminListUserAuthEventsResponse' {} a -> s {authEvents = a} :: AdminListUserAuthEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token.
adminListUserAuthEventsResponse_nextToken :: Lens.Lens' AdminListUserAuthEventsResponse (Prelude.Maybe Prelude.Text)
adminListUserAuthEventsResponse_nextToken = Lens.lens (\AdminListUserAuthEventsResponse' {nextToken} -> nextToken) (\s@AdminListUserAuthEventsResponse' {} a -> s {nextToken = a} :: AdminListUserAuthEventsResponse)

-- | The response's http status code.
adminListUserAuthEventsResponse_httpStatus :: Lens.Lens' AdminListUserAuthEventsResponse Prelude.Int
adminListUserAuthEventsResponse_httpStatus = Lens.lens (\AdminListUserAuthEventsResponse' {httpStatus} -> httpStatus) (\s@AdminListUserAuthEventsResponse' {} a -> s {httpStatus = a} :: AdminListUserAuthEventsResponse)

instance
  Prelude.NFData
    AdminListUserAuthEventsResponse
  where
  rnf AdminListUserAuthEventsResponse' {..} =
    Prelude.rnf authEvents
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
