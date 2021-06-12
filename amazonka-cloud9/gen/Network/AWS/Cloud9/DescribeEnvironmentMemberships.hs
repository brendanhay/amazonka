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
-- Module      : Network.AWS.Cloud9.DescribeEnvironmentMemberships
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about environment members for an AWS Cloud9 development
-- environment.
--
-- This operation returns paginated results.
module Network.AWS.Cloud9.DescribeEnvironmentMemberships
  ( -- * Creating a Request
    DescribeEnvironmentMemberships (..),
    newDescribeEnvironmentMemberships,

    -- * Request Lenses
    describeEnvironmentMemberships_nextToken,
    describeEnvironmentMemberships_userArn,
    describeEnvironmentMemberships_maxResults,
    describeEnvironmentMemberships_permissions,
    describeEnvironmentMemberships_environmentId,

    -- * Destructuring the Response
    DescribeEnvironmentMembershipsResponse (..),
    newDescribeEnvironmentMembershipsResponse,

    -- * Response Lenses
    describeEnvironmentMembershipsResponse_nextToken,
    describeEnvironmentMembershipsResponse_memberships,
    describeEnvironmentMembershipsResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEnvironmentMemberships' smart constructor.
data DescribeEnvironmentMemberships = DescribeEnvironmentMemberships'
  { -- | During a previous call, if there are more than 25 items in the list,
    -- only the first 25 items are returned, along with a unique string called
    -- a /next token/. To get the next batch of items in the list, call this
    -- operation again, adding the next token to the call. To get all of the
    -- items in the list, keep calling this operation with each subsequent next
    -- token that is returned, until no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of an individual environment member to
    -- get information about. If no value is specified, information about all
    -- environment members are returned.
    userArn :: Core.Maybe Core.Text,
    -- | The maximum number of environment members to get information about.
    maxResults :: Core.Maybe Core.Natural,
    -- | The type of environment member permissions to get information about.
    -- Available values include:
    --
    -- -   @owner@: Owns the environment.
    --
    -- -   @read-only@: Has read-only access to the environment.
    --
    -- -   @read-write@: Has read-write access to the environment.
    --
    -- If no value is specified, information about all environment members are
    -- returned.
    permissions :: Core.Maybe [Permissions],
    -- | The ID of the environment to get environment member information about.
    environmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEnvironmentMemberships' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEnvironmentMemberships_nextToken' - During a previous call, if there are more than 25 items in the list,
-- only the first 25 items are returned, along with a unique string called
-- a /next token/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
--
-- 'userArn', 'describeEnvironmentMemberships_userArn' - The Amazon Resource Name (ARN) of an individual environment member to
-- get information about. If no value is specified, information about all
-- environment members are returned.
--
-- 'maxResults', 'describeEnvironmentMemberships_maxResults' - The maximum number of environment members to get information about.
--
-- 'permissions', 'describeEnvironmentMemberships_permissions' - The type of environment member permissions to get information about.
-- Available values include:
--
-- -   @owner@: Owns the environment.
--
-- -   @read-only@: Has read-only access to the environment.
--
-- -   @read-write@: Has read-write access to the environment.
--
-- If no value is specified, information about all environment members are
-- returned.
--
-- 'environmentId', 'describeEnvironmentMemberships_environmentId' - The ID of the environment to get environment member information about.
newDescribeEnvironmentMemberships ::
  DescribeEnvironmentMemberships
newDescribeEnvironmentMemberships =
  DescribeEnvironmentMemberships'
    { nextToken =
        Core.Nothing,
      userArn = Core.Nothing,
      maxResults = Core.Nothing,
      permissions = Core.Nothing,
      environmentId = Core.Nothing
    }

-- | During a previous call, if there are more than 25 items in the list,
-- only the first 25 items are returned, along with a unique string called
-- a /next token/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
describeEnvironmentMemberships_nextToken :: Lens.Lens' DescribeEnvironmentMemberships (Core.Maybe Core.Text)
describeEnvironmentMemberships_nextToken = Lens.lens (\DescribeEnvironmentMemberships' {nextToken} -> nextToken) (\s@DescribeEnvironmentMemberships' {} a -> s {nextToken = a} :: DescribeEnvironmentMemberships)

-- | The Amazon Resource Name (ARN) of an individual environment member to
-- get information about. If no value is specified, information about all
-- environment members are returned.
describeEnvironmentMemberships_userArn :: Lens.Lens' DescribeEnvironmentMemberships (Core.Maybe Core.Text)
describeEnvironmentMemberships_userArn = Lens.lens (\DescribeEnvironmentMemberships' {userArn} -> userArn) (\s@DescribeEnvironmentMemberships' {} a -> s {userArn = a} :: DescribeEnvironmentMemberships)

-- | The maximum number of environment members to get information about.
describeEnvironmentMemberships_maxResults :: Lens.Lens' DescribeEnvironmentMemberships (Core.Maybe Core.Natural)
describeEnvironmentMemberships_maxResults = Lens.lens (\DescribeEnvironmentMemberships' {maxResults} -> maxResults) (\s@DescribeEnvironmentMemberships' {} a -> s {maxResults = a} :: DescribeEnvironmentMemberships)

-- | The type of environment member permissions to get information about.
-- Available values include:
--
-- -   @owner@: Owns the environment.
--
-- -   @read-only@: Has read-only access to the environment.
--
-- -   @read-write@: Has read-write access to the environment.
--
-- If no value is specified, information about all environment members are
-- returned.
describeEnvironmentMemberships_permissions :: Lens.Lens' DescribeEnvironmentMemberships (Core.Maybe [Permissions])
describeEnvironmentMemberships_permissions = Lens.lens (\DescribeEnvironmentMemberships' {permissions} -> permissions) (\s@DescribeEnvironmentMemberships' {} a -> s {permissions = a} :: DescribeEnvironmentMemberships) Core.. Lens.mapping Lens._Coerce

-- | The ID of the environment to get environment member information about.
describeEnvironmentMemberships_environmentId :: Lens.Lens' DescribeEnvironmentMemberships (Core.Maybe Core.Text)
describeEnvironmentMemberships_environmentId = Lens.lens (\DescribeEnvironmentMemberships' {environmentId} -> environmentId) (\s@DescribeEnvironmentMemberships' {} a -> s {environmentId = a} :: DescribeEnvironmentMemberships)

instance Core.AWSPager DescribeEnvironmentMemberships where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEnvironmentMembershipsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEnvironmentMembershipsResponse_memberships
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEnvironmentMemberships_nextToken
          Lens..~ rs
          Lens.^? describeEnvironmentMembershipsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeEnvironmentMemberships
  where
  type
    AWSResponse DescribeEnvironmentMemberships =
      DescribeEnvironmentMembershipsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEnvironmentMembershipsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "memberships" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEnvironmentMemberships

instance Core.NFData DescribeEnvironmentMemberships

instance
  Core.ToHeaders
    DescribeEnvironmentMemberships
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCloud9WorkspaceManagementService.DescribeEnvironmentMemberships" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEnvironmentMemberships where
  toJSON DescribeEnvironmentMemberships' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("userArn" Core..=) Core.<$> userArn,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("permissions" Core..=) Core.<$> permissions,
            ("environmentId" Core..=) Core.<$> environmentId
          ]
      )

instance Core.ToPath DescribeEnvironmentMemberships where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEnvironmentMemberships where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEnvironmentMembershipsResponse' smart constructor.
data DescribeEnvironmentMembershipsResponse = DescribeEnvironmentMembershipsResponse'
  { -- | If there are more than 25 items in the list, only the first 25 items are
    -- returned, along with a unique string called a /next token/. To get the
    -- next batch of items in the list, call this operation again, adding the
    -- next token to the call.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the environment members for the environment.
    memberships :: Core.Maybe [EnvironmentMember],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEnvironmentMembershipsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEnvironmentMembershipsResponse_nextToken' - If there are more than 25 items in the list, only the first 25 items are
-- returned, along with a unique string called a /next token/. To get the
-- next batch of items in the list, call this operation again, adding the
-- next token to the call.
--
-- 'memberships', 'describeEnvironmentMembershipsResponse_memberships' - Information about the environment members for the environment.
--
-- 'httpStatus', 'describeEnvironmentMembershipsResponse_httpStatus' - The response's http status code.
newDescribeEnvironmentMembershipsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEnvironmentMembershipsResponse
newDescribeEnvironmentMembershipsResponse
  pHttpStatus_ =
    DescribeEnvironmentMembershipsResponse'
      { nextToken =
          Core.Nothing,
        memberships = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If there are more than 25 items in the list, only the first 25 items are
-- returned, along with a unique string called a /next token/. To get the
-- next batch of items in the list, call this operation again, adding the
-- next token to the call.
describeEnvironmentMembershipsResponse_nextToken :: Lens.Lens' DescribeEnvironmentMembershipsResponse (Core.Maybe Core.Text)
describeEnvironmentMembershipsResponse_nextToken = Lens.lens (\DescribeEnvironmentMembershipsResponse' {nextToken} -> nextToken) (\s@DescribeEnvironmentMembershipsResponse' {} a -> s {nextToken = a} :: DescribeEnvironmentMembershipsResponse)

-- | Information about the environment members for the environment.
describeEnvironmentMembershipsResponse_memberships :: Lens.Lens' DescribeEnvironmentMembershipsResponse (Core.Maybe [EnvironmentMember])
describeEnvironmentMembershipsResponse_memberships = Lens.lens (\DescribeEnvironmentMembershipsResponse' {memberships} -> memberships) (\s@DescribeEnvironmentMembershipsResponse' {} a -> s {memberships = a} :: DescribeEnvironmentMembershipsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEnvironmentMembershipsResponse_httpStatus :: Lens.Lens' DescribeEnvironmentMembershipsResponse Core.Int
describeEnvironmentMembershipsResponse_httpStatus = Lens.lens (\DescribeEnvironmentMembershipsResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentMembershipsResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentMembershipsResponse)

instance
  Core.NFData
    DescribeEnvironmentMembershipsResponse
