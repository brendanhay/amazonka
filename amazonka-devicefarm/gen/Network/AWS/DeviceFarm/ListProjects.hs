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
-- Module      : Network.AWS.DeviceFarm.ListProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about projects.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListProjects
  ( -- * Creating a Request
    ListProjects (..),
    newListProjects,

    -- * Request Lenses
    listProjects_nextToken,
    listProjects_arn,

    -- * Destructuring the Response
    ListProjectsResponse (..),
    newListProjectsResponse,

    -- * Response Lenses
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list projects operation.
--
-- /See:/ 'newListProjects' smart constructor.
data ListProjects = ListProjects'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | Optional. If no Amazon Resource Name (ARN) is specified, then AWS Device
    -- Farm returns a list of all projects for the AWS account. You can also
    -- specify a project ARN.
    arn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProjects_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listProjects_arn' - Optional. If no Amazon Resource Name (ARN) is specified, then AWS Device
-- Farm returns a list of all projects for the AWS account. You can also
-- specify a project ARN.
newListProjects ::
  ListProjects
newListProjects =
  ListProjects'
    { nextToken = Core.Nothing,
      arn = Core.Nothing
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listProjects_nextToken :: Lens.Lens' ListProjects (Core.Maybe Core.Text)
listProjects_nextToken = Lens.lens (\ListProjects' {nextToken} -> nextToken) (\s@ListProjects' {} a -> s {nextToken = a} :: ListProjects)

-- | Optional. If no Amazon Resource Name (ARN) is specified, then AWS Device
-- Farm returns a list of all projects for the AWS account. You can also
-- specify a project ARN.
listProjects_arn :: Lens.Lens' ListProjects (Core.Maybe Core.Text)
listProjects_arn = Lens.lens (\ListProjects' {arn} -> arn) (\s@ListProjects' {} a -> s {arn = a} :: ListProjects)

instance Core.AWSPager ListProjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProjectsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listProjectsResponse_projects Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listProjects_nextToken
          Lens..~ rs
          Lens.^? listProjectsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListProjects where
  type AWSResponse ListProjects = ListProjectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProjectsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "projects" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListProjects

instance Core.NFData ListProjects

instance Core.ToHeaders ListProjects where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListProjects" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListProjects where
  toJSON ListProjects' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("arn" Core..=) Core.<$> arn
          ]
      )

instance Core.ToPath ListProjects where
  toPath = Core.const "/"

instance Core.ToQuery ListProjects where
  toQuery = Core.const Core.mempty

-- | Represents the result of a list projects request.
--
-- /See:/ 'newListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the projects.
    projects :: Core.Maybe [Project],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProjectsResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'projects', 'listProjectsResponse_projects' - Information about the projects.
--
-- 'httpStatus', 'listProjectsResponse_httpStatus' - The response's http status code.
newListProjectsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListProjectsResponse
newListProjectsResponse pHttpStatus_ =
  ListProjectsResponse'
    { nextToken = Core.Nothing,
      projects = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listProjectsResponse_nextToken :: Lens.Lens' ListProjectsResponse (Core.Maybe Core.Text)
listProjectsResponse_nextToken = Lens.lens (\ListProjectsResponse' {nextToken} -> nextToken) (\s@ListProjectsResponse' {} a -> s {nextToken = a} :: ListProjectsResponse)

-- | Information about the projects.
listProjectsResponse_projects :: Lens.Lens' ListProjectsResponse (Core.Maybe [Project])
listProjectsResponse_projects = Lens.lens (\ListProjectsResponse' {projects} -> projects) (\s@ListProjectsResponse' {} a -> s {projects = a} :: ListProjectsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listProjectsResponse_httpStatus :: Lens.Lens' ListProjectsResponse Core.Int
listProjectsResponse_httpStatus = Lens.lens (\ListProjectsResponse' {httpStatus} -> httpStatus) (\s@ListProjectsResponse' {} a -> s {httpStatus = a} :: ListProjectsResponse)

instance Core.NFData ListProjectsResponse
