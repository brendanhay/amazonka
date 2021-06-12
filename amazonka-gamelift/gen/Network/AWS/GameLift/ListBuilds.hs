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
-- Module      : Network.AWS.GameLift.ListBuilds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves build resources for all builds associated with the AWS account
-- in use. You can limit results to builds that are in a specific status by
-- using the @Status@ parameter. Use the pagination parameters to retrieve
-- results in a set of sequential pages.
--
-- Build resources are not listed in any particular order.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Upload a Custom Server Build>
--
-- __Related operations__
--
-- -   CreateBuild
--
-- -   ListBuilds
--
-- -   DescribeBuild
--
-- -   UpdateBuild
--
-- -   DeleteBuild
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListBuilds
  ( -- * Creating a Request
    ListBuilds (..),
    newListBuilds,

    -- * Request Lenses
    listBuilds_status,
    listBuilds_nextToken,
    listBuilds_limit,

    -- * Destructuring the Response
    ListBuildsResponse (..),
    newListBuildsResponse,

    -- * Response Lenses
    listBuildsResponse_nextToken,
    listBuildsResponse_builds,
    listBuildsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newListBuilds' smart constructor.
data ListBuilds = ListBuilds'
  { -- | Build status to filter results by. To retrieve all builds, leave this
    -- parameter empty.
    --
    -- Possible build statuses include the following:
    --
    -- -   __INITIALIZED__ -- A new build has been defined, but no files have
    --     been uploaded. You cannot create fleets for builds that are in this
    --     status. When a build is successfully created, the build status is
    --     set to this value.
    --
    -- -   __READY__ -- The game build has been successfully uploaded. You can
    --     now create new fleets for this build.
    --
    -- -   __FAILED__ -- The game build upload failed. You cannot create new
    --     fleets for this build.
    status :: Core.Maybe BuildStatus,
    -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuilds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listBuilds_status' - Build status to filter results by. To retrieve all builds, leave this
-- parameter empty.
--
-- Possible build statuses include the following:
--
-- -   __INITIALIZED__ -- A new build has been defined, but no files have
--     been uploaded. You cannot create fleets for builds that are in this
--     status. When a build is successfully created, the build status is
--     set to this value.
--
-- -   __READY__ -- The game build has been successfully uploaded. You can
--     now create new fleets for this build.
--
-- -   __FAILED__ -- The game build upload failed. You cannot create new
--     fleets for this build.
--
-- 'nextToken', 'listBuilds_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'limit', 'listBuilds_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
newListBuilds ::
  ListBuilds
newListBuilds =
  ListBuilds'
    { status = Core.Nothing,
      nextToken = Core.Nothing,
      limit = Core.Nothing
    }

-- | Build status to filter results by. To retrieve all builds, leave this
-- parameter empty.
--
-- Possible build statuses include the following:
--
-- -   __INITIALIZED__ -- A new build has been defined, but no files have
--     been uploaded. You cannot create fleets for builds that are in this
--     status. When a build is successfully created, the build status is
--     set to this value.
--
-- -   __READY__ -- The game build has been successfully uploaded. You can
--     now create new fleets for this build.
--
-- -   __FAILED__ -- The game build upload failed. You cannot create new
--     fleets for this build.
listBuilds_status :: Lens.Lens' ListBuilds (Core.Maybe BuildStatus)
listBuilds_status = Lens.lens (\ListBuilds' {status} -> status) (\s@ListBuilds' {} a -> s {status = a} :: ListBuilds)

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listBuilds_nextToken :: Lens.Lens' ListBuilds (Core.Maybe Core.Text)
listBuilds_nextToken = Lens.lens (\ListBuilds' {nextToken} -> nextToken) (\s@ListBuilds' {} a -> s {nextToken = a} :: ListBuilds)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listBuilds_limit :: Lens.Lens' ListBuilds (Core.Maybe Core.Natural)
listBuilds_limit = Lens.lens (\ListBuilds' {limit} -> limit) (\s@ListBuilds' {} a -> s {limit = a} :: ListBuilds)

instance Core.AWSPager ListBuilds where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBuildsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBuildsResponse_builds Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBuilds_nextToken
          Lens..~ rs
          Lens.^? listBuildsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListBuilds where
  type AWSResponse ListBuilds = ListBuildsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Builds" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBuilds

instance Core.NFData ListBuilds

instance Core.ToHeaders ListBuilds where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ListBuilds" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListBuilds where
  toJSON ListBuilds' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListBuilds where
  toPath = Core.const "/"

instance Core.ToQuery ListBuilds where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newListBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A collection of build resources that match the request.
    builds :: Core.Maybe [Build],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuildsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBuildsResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'builds', 'listBuildsResponse_builds' - A collection of build resources that match the request.
--
-- 'httpStatus', 'listBuildsResponse_httpStatus' - The response's http status code.
newListBuildsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBuildsResponse
newListBuildsResponse pHttpStatus_ =
  ListBuildsResponse'
    { nextToken = Core.Nothing,
      builds = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
listBuildsResponse_nextToken :: Lens.Lens' ListBuildsResponse (Core.Maybe Core.Text)
listBuildsResponse_nextToken = Lens.lens (\ListBuildsResponse' {nextToken} -> nextToken) (\s@ListBuildsResponse' {} a -> s {nextToken = a} :: ListBuildsResponse)

-- | A collection of build resources that match the request.
listBuildsResponse_builds :: Lens.Lens' ListBuildsResponse (Core.Maybe [Build])
listBuildsResponse_builds = Lens.lens (\ListBuildsResponse' {builds} -> builds) (\s@ListBuildsResponse' {} a -> s {builds = a} :: ListBuildsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBuildsResponse_httpStatus :: Lens.Lens' ListBuildsResponse Core.Int
listBuildsResponse_httpStatus = Lens.lens (\ListBuildsResponse' {httpStatus} -> httpStatus) (\s@ListBuildsResponse' {} a -> s {httpStatus = a} :: ListBuildsResponse)

instance Core.NFData ListBuildsResponse
