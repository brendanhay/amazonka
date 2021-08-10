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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists a history of user activity and any risks detected as part of
-- Amazon Cognito advanced security.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents
  ( -- * Creating a Request
    AdminListUserAuthEvents (..),
    newAdminListUserAuthEvents,

    -- * Request Lenses
    adminListUserAuthEvents_nextToken,
    adminListUserAuthEvents_maxResults,
    adminListUserAuthEvents_userPoolId,
    adminListUserAuthEvents_username,

    -- * Destructuring the Response
    AdminListUserAuthEventsResponse (..),
    newAdminListUserAuthEventsResponse,

    -- * Response Lenses
    adminListUserAuthEventsResponse_nextToken,
    adminListUserAuthEventsResponse_authEvents,
    adminListUserAuthEventsResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdminListUserAuthEvents' smart constructor.
data AdminListUserAuthEvents = AdminListUserAuthEvents'
  { -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of authentication events to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The user pool username or an alias.
    username :: Core.Sensitive Prelude.Text
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
-- 'nextToken', 'adminListUserAuthEvents_nextToken' - A pagination token.
--
-- 'maxResults', 'adminListUserAuthEvents_maxResults' - The maximum number of authentication events to return.
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
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      username = Core._Sensitive Lens.# pUsername_
    }

-- | A pagination token.
adminListUserAuthEvents_nextToken :: Lens.Lens' AdminListUserAuthEvents (Prelude.Maybe Prelude.Text)
adminListUserAuthEvents_nextToken = Lens.lens (\AdminListUserAuthEvents' {nextToken} -> nextToken) (\s@AdminListUserAuthEvents' {} a -> s {nextToken = a} :: AdminListUserAuthEvents)

-- | The maximum number of authentication events to return.
adminListUserAuthEvents_maxResults :: Lens.Lens' AdminListUserAuthEvents (Prelude.Maybe Prelude.Natural)
adminListUserAuthEvents_maxResults = Lens.lens (\AdminListUserAuthEvents' {maxResults} -> maxResults) (\s@AdminListUserAuthEvents' {} a -> s {maxResults = a} :: AdminListUserAuthEvents)

-- | The user pool ID.
adminListUserAuthEvents_userPoolId :: Lens.Lens' AdminListUserAuthEvents Prelude.Text
adminListUserAuthEvents_userPoolId = Lens.lens (\AdminListUserAuthEvents' {userPoolId} -> userPoolId) (\s@AdminListUserAuthEvents' {} a -> s {userPoolId = a} :: AdminListUserAuthEvents)

-- | The user pool username or an alias.
adminListUserAuthEvents_username :: Lens.Lens' AdminListUserAuthEvents Prelude.Text
adminListUserAuthEvents_username = Lens.lens (\AdminListUserAuthEvents' {username} -> username) (\s@AdminListUserAuthEvents' {} a -> s {username = a} :: AdminListUserAuthEvents) Prelude.. Core._Sensitive

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
      Prelude.Just Prelude.$
        rq
          Prelude.& adminListUserAuthEvents_nextToken
          Lens..~ rs
          Lens.^? adminListUserAuthEventsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest AdminListUserAuthEvents where
  type
    AWSResponse AdminListUserAuthEvents =
      AdminListUserAuthEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminListUserAuthEventsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "AuthEvents" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminListUserAuthEvents

instance Prelude.NFData AdminListUserAuthEvents

instance Core.ToHeaders AdminListUserAuthEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminListUserAuthEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AdminListUserAuthEvents where
  toJSON AdminListUserAuthEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("Username" Core..= username)
          ]
      )

instance Core.ToPath AdminListUserAuthEvents where
  toPath = Prelude.const "/"

instance Core.ToQuery AdminListUserAuthEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminListUserAuthEventsResponse' smart constructor.
data AdminListUserAuthEventsResponse = AdminListUserAuthEventsResponse'
  { -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response object. It includes the @EventID@, @EventType@,
    -- @CreationDate@, @EventRisk@, and @EventResponse@.
    authEvents :: Prelude.Maybe [AuthEventType],
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
-- 'nextToken', 'adminListUserAuthEventsResponse_nextToken' - A pagination token.
--
-- 'authEvents', 'adminListUserAuthEventsResponse_authEvents' - The response object. It includes the @EventID@, @EventType@,
-- @CreationDate@, @EventRisk@, and @EventResponse@.
--
-- 'httpStatus', 'adminListUserAuthEventsResponse_httpStatus' - The response's http status code.
newAdminListUserAuthEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminListUserAuthEventsResponse
newAdminListUserAuthEventsResponse pHttpStatus_ =
  AdminListUserAuthEventsResponse'
    { nextToken =
        Prelude.Nothing,
      authEvents = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token.
adminListUserAuthEventsResponse_nextToken :: Lens.Lens' AdminListUserAuthEventsResponse (Prelude.Maybe Prelude.Text)
adminListUserAuthEventsResponse_nextToken = Lens.lens (\AdminListUserAuthEventsResponse' {nextToken} -> nextToken) (\s@AdminListUserAuthEventsResponse' {} a -> s {nextToken = a} :: AdminListUserAuthEventsResponse)

-- | The response object. It includes the @EventID@, @EventType@,
-- @CreationDate@, @EventRisk@, and @EventResponse@.
adminListUserAuthEventsResponse_authEvents :: Lens.Lens' AdminListUserAuthEventsResponse (Prelude.Maybe [AuthEventType])
adminListUserAuthEventsResponse_authEvents = Lens.lens (\AdminListUserAuthEventsResponse' {authEvents} -> authEvents) (\s@AdminListUserAuthEventsResponse' {} a -> s {authEvents = a} :: AdminListUserAuthEventsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
adminListUserAuthEventsResponse_httpStatus :: Lens.Lens' AdminListUserAuthEventsResponse Prelude.Int
adminListUserAuthEventsResponse_httpStatus = Lens.lens (\AdminListUserAuthEventsResponse' {httpStatus} -> httpStatus) (\s@AdminListUserAuthEventsResponse' {} a -> s {httpStatus = a} :: AdminListUserAuthEventsResponse)

instance
  Prelude.NFData
    AdminListUserAuthEventsResponse
