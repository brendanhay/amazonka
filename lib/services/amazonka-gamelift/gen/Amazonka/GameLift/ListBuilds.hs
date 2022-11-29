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
-- Module      : Amazonka.GameLift.ListBuilds
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves build resources for all builds associated with the Amazon Web
-- Services account in use. You can limit results to builds that are in a
-- specific status by using the @Status@ parameter. Use the pagination
-- parameters to retrieve results in a set of sequential pages.
--
-- Build resources are not listed in any particular order.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Upload a Custom Server Build>
--
-- __Related actions__
--
-- CreateBuild | ListBuilds | DescribeBuild | UpdateBuild | DeleteBuild |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.ListBuilds
  ( -- * Creating a Request
    ListBuilds (..),
    newListBuilds,

    -- * Request Lenses
    listBuilds_nextToken,
    listBuilds_status,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newListBuilds' smart constructor.
data ListBuilds = ListBuilds'
  { -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe BuildStatus,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBuilds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBuilds_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
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
-- 'limit', 'listBuilds_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
newListBuilds ::
  ListBuilds
newListBuilds =
  ListBuilds'
    { nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listBuilds_nextToken :: Lens.Lens' ListBuilds (Prelude.Maybe Prelude.Text)
listBuilds_nextToken = Lens.lens (\ListBuilds' {nextToken} -> nextToken) (\s@ListBuilds' {} a -> s {nextToken = a} :: ListBuilds)

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
listBuilds_status :: Lens.Lens' ListBuilds (Prelude.Maybe BuildStatus)
listBuilds_status = Lens.lens (\ListBuilds' {status} -> status) (\s@ListBuilds' {} a -> s {status = a} :: ListBuilds)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listBuilds_limit :: Lens.Lens' ListBuilds (Prelude.Maybe Prelude.Natural)
listBuilds_limit = Lens.lens (\ListBuilds' {limit} -> limit) (\s@ListBuilds' {} a -> s {limit = a} :: ListBuilds)

instance Core.AWSPager ListBuilds where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBuildsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBuildsResponse_builds Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBuilds_nextToken
          Lens..~ rs
          Lens.^? listBuildsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListBuilds where
  type AWSResponse ListBuilds = ListBuildsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Builds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBuilds where
  hashWithSalt _salt ListBuilds' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` limit

instance Prelude.NFData ListBuilds where
  rnf ListBuilds' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf limit

instance Core.ToHeaders ListBuilds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ListBuilds" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListBuilds where
  toJSON ListBuilds' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Status" Core..=) Prelude.<$> status,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListBuilds where
  toPath = Prelude.const "/"

instance Core.ToQuery ListBuilds where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newListBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of build resources that match the request.
    builds :: Prelude.Maybe [Build],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBuildsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBuildsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'builds', 'listBuildsResponse_builds' - A collection of build resources that match the request.
--
-- 'httpStatus', 'listBuildsResponse_httpStatus' - The response's http status code.
newListBuildsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBuildsResponse
newListBuildsResponse pHttpStatus_ =
  ListBuildsResponse'
    { nextToken = Prelude.Nothing,
      builds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listBuildsResponse_nextToken :: Lens.Lens' ListBuildsResponse (Prelude.Maybe Prelude.Text)
listBuildsResponse_nextToken = Lens.lens (\ListBuildsResponse' {nextToken} -> nextToken) (\s@ListBuildsResponse' {} a -> s {nextToken = a} :: ListBuildsResponse)

-- | A collection of build resources that match the request.
listBuildsResponse_builds :: Lens.Lens' ListBuildsResponse (Prelude.Maybe [Build])
listBuildsResponse_builds = Lens.lens (\ListBuildsResponse' {builds} -> builds) (\s@ListBuildsResponse' {} a -> s {builds = a} :: ListBuildsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listBuildsResponse_httpStatus :: Lens.Lens' ListBuildsResponse Prelude.Int
listBuildsResponse_httpStatus = Lens.lens (\ListBuildsResponse' {httpStatus} -> httpStatus) (\s@ListBuildsResponse' {} a -> s {httpStatus = a} :: ListBuildsResponse)

instance Prelude.NFData ListBuildsResponse where
  rnf ListBuildsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf builds
      `Prelude.seq` Prelude.rnf httpStatus
