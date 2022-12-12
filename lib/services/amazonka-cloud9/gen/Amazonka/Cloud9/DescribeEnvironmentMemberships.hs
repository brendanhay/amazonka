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
-- Module      : Amazonka.Cloud9.DescribeEnvironmentMemberships
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about environment members for an Cloud9 development
-- environment.
--
-- This operation returns paginated results.
module Amazonka.Cloud9.DescribeEnvironmentMemberships
  ( -- * Creating a Request
    DescribeEnvironmentMemberships (..),
    newDescribeEnvironmentMemberships,

    -- * Request Lenses
    describeEnvironmentMemberships_environmentId,
    describeEnvironmentMemberships_maxResults,
    describeEnvironmentMemberships_nextToken,
    describeEnvironmentMemberships_permissions,
    describeEnvironmentMemberships_userArn,

    -- * Destructuring the Response
    DescribeEnvironmentMembershipsResponse (..),
    newDescribeEnvironmentMembershipsResponse,

    -- * Response Lenses
    describeEnvironmentMembershipsResponse_memberships,
    describeEnvironmentMembershipsResponse_nextToken,
    describeEnvironmentMembershipsResponse_httpStatus,
  )
where

import Amazonka.Cloud9.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEnvironmentMemberships' smart constructor.
data DescribeEnvironmentMemberships = DescribeEnvironmentMemberships'
  { -- | The ID of the environment to get environment member information about.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of environment members to get information about.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | During a previous call, if there are more than 25 items in the list,
    -- only the first 25 items are returned, along with a unique string called
    -- a /next token/. To get the next batch of items in the list, call this
    -- operation again, adding the next token to the call. To get all of the
    -- items in the list, keep calling this operation with each subsequent next
    -- token that is returned, until no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    permissions :: Prelude.Maybe [Permissions],
    -- | The Amazon Resource Name (ARN) of an individual environment member to
    -- get information about. If no value is specified, information about all
    -- environment members are returned.
    userArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironmentMemberships' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'describeEnvironmentMemberships_environmentId' - The ID of the environment to get environment member information about.
--
-- 'maxResults', 'describeEnvironmentMemberships_maxResults' - The maximum number of environment members to get information about.
--
-- 'nextToken', 'describeEnvironmentMemberships_nextToken' - During a previous call, if there are more than 25 items in the list,
-- only the first 25 items are returned, along with a unique string called
-- a /next token/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
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
-- 'userArn', 'describeEnvironmentMemberships_userArn' - The Amazon Resource Name (ARN) of an individual environment member to
-- get information about. If no value is specified, information about all
-- environment members are returned.
newDescribeEnvironmentMemberships ::
  DescribeEnvironmentMemberships
newDescribeEnvironmentMemberships =
  DescribeEnvironmentMemberships'
    { environmentId =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      permissions = Prelude.Nothing,
      userArn = Prelude.Nothing
    }

-- | The ID of the environment to get environment member information about.
describeEnvironmentMemberships_environmentId :: Lens.Lens' DescribeEnvironmentMemberships (Prelude.Maybe Prelude.Text)
describeEnvironmentMemberships_environmentId = Lens.lens (\DescribeEnvironmentMemberships' {environmentId} -> environmentId) (\s@DescribeEnvironmentMemberships' {} a -> s {environmentId = a} :: DescribeEnvironmentMemberships)

-- | The maximum number of environment members to get information about.
describeEnvironmentMemberships_maxResults :: Lens.Lens' DescribeEnvironmentMemberships (Prelude.Maybe Prelude.Natural)
describeEnvironmentMemberships_maxResults = Lens.lens (\DescribeEnvironmentMemberships' {maxResults} -> maxResults) (\s@DescribeEnvironmentMemberships' {} a -> s {maxResults = a} :: DescribeEnvironmentMemberships)

-- | During a previous call, if there are more than 25 items in the list,
-- only the first 25 items are returned, along with a unique string called
-- a /next token/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
describeEnvironmentMemberships_nextToken :: Lens.Lens' DescribeEnvironmentMemberships (Prelude.Maybe Prelude.Text)
describeEnvironmentMemberships_nextToken = Lens.lens (\DescribeEnvironmentMemberships' {nextToken} -> nextToken) (\s@DescribeEnvironmentMemberships' {} a -> s {nextToken = a} :: DescribeEnvironmentMemberships)

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
describeEnvironmentMemberships_permissions :: Lens.Lens' DescribeEnvironmentMemberships (Prelude.Maybe [Permissions])
describeEnvironmentMemberships_permissions = Lens.lens (\DescribeEnvironmentMemberships' {permissions} -> permissions) (\s@DescribeEnvironmentMemberships' {} a -> s {permissions = a} :: DescribeEnvironmentMemberships) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an individual environment member to
-- get information about. If no value is specified, information about all
-- environment members are returned.
describeEnvironmentMemberships_userArn :: Lens.Lens' DescribeEnvironmentMemberships (Prelude.Maybe Prelude.Text)
describeEnvironmentMemberships_userArn = Lens.lens (\DescribeEnvironmentMemberships' {userArn} -> userArn) (\s@DescribeEnvironmentMemberships' {} a -> s {userArn = a} :: DescribeEnvironmentMemberships)

instance Core.AWSPager DescribeEnvironmentMemberships where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEnvironmentMembershipsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEnvironmentMembershipsResponse_memberships
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEnvironmentMemberships_nextToken
          Lens..~ rs
          Lens.^? describeEnvironmentMembershipsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeEnvironmentMemberships
  where
  type
    AWSResponse DescribeEnvironmentMemberships =
      DescribeEnvironmentMembershipsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEnvironmentMembershipsResponse'
            Prelude.<$> (x Data..?> "memberships" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEnvironmentMemberships
  where
  hashWithSalt
    _salt
    DescribeEnvironmentMemberships' {..} =
      _salt `Prelude.hashWithSalt` environmentId
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` permissions
        `Prelude.hashWithSalt` userArn

instance
  Prelude.NFData
    DescribeEnvironmentMemberships
  where
  rnf DescribeEnvironmentMemberships' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf userArn

instance
  Data.ToHeaders
    DescribeEnvironmentMemberships
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCloud9WorkspaceManagementService.DescribeEnvironmentMemberships" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEnvironmentMemberships where
  toJSON DescribeEnvironmentMemberships' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("environmentId" Data..=) Prelude.<$> environmentId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("permissions" Data..=) Prelude.<$> permissions,
            ("userArn" Data..=) Prelude.<$> userArn
          ]
      )

instance Data.ToPath DescribeEnvironmentMemberships where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEnvironmentMemberships where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEnvironmentMembershipsResponse' smart constructor.
data DescribeEnvironmentMembershipsResponse = DescribeEnvironmentMembershipsResponse'
  { -- | Information about the environment members for the environment.
    memberships :: Prelude.Maybe [EnvironmentMember],
    -- | If there are more than 25 items in the list, only the first 25 items are
    -- returned, along with a unique string called a /next token/. To get the
    -- next batch of items in the list, call this operation again, adding the
    -- next token to the call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironmentMembershipsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberships', 'describeEnvironmentMembershipsResponse_memberships' - Information about the environment members for the environment.
--
-- 'nextToken', 'describeEnvironmentMembershipsResponse_nextToken' - If there are more than 25 items in the list, only the first 25 items are
-- returned, along with a unique string called a /next token/. To get the
-- next batch of items in the list, call this operation again, adding the
-- next token to the call.
--
-- 'httpStatus', 'describeEnvironmentMembershipsResponse_httpStatus' - The response's http status code.
newDescribeEnvironmentMembershipsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEnvironmentMembershipsResponse
newDescribeEnvironmentMembershipsResponse
  pHttpStatus_ =
    DescribeEnvironmentMembershipsResponse'
      { memberships =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the environment members for the environment.
describeEnvironmentMembershipsResponse_memberships :: Lens.Lens' DescribeEnvironmentMembershipsResponse (Prelude.Maybe [EnvironmentMember])
describeEnvironmentMembershipsResponse_memberships = Lens.lens (\DescribeEnvironmentMembershipsResponse' {memberships} -> memberships) (\s@DescribeEnvironmentMembershipsResponse' {} a -> s {memberships = a} :: DescribeEnvironmentMembershipsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more than 25 items in the list, only the first 25 items are
-- returned, along with a unique string called a /next token/. To get the
-- next batch of items in the list, call this operation again, adding the
-- next token to the call.
describeEnvironmentMembershipsResponse_nextToken :: Lens.Lens' DescribeEnvironmentMembershipsResponse (Prelude.Maybe Prelude.Text)
describeEnvironmentMembershipsResponse_nextToken = Lens.lens (\DescribeEnvironmentMembershipsResponse' {nextToken} -> nextToken) (\s@DescribeEnvironmentMembershipsResponse' {} a -> s {nextToken = a} :: DescribeEnvironmentMembershipsResponse)

-- | The response's http status code.
describeEnvironmentMembershipsResponse_httpStatus :: Lens.Lens' DescribeEnvironmentMembershipsResponse Prelude.Int
describeEnvironmentMembershipsResponse_httpStatus = Lens.lens (\DescribeEnvironmentMembershipsResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentMembershipsResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentMembershipsResponse)

instance
  Prelude.NFData
    DescribeEnvironmentMembershipsResponse
  where
  rnf DescribeEnvironmentMembershipsResponse' {..} =
    Prelude.rnf memberships
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
