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
-- Module      : Network.AWS.CodeDeploy.ListApplications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the applications registered with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListApplications
  ( -- * Creating a Request
    ListApplications (..),
    newListApplications,

    -- * Request Lenses
    listApplications_nextToken,

    -- * Destructuring the Response
    ListApplicationsResponse (..),
    newListApplicationsResponse,

    -- * Response Lenses
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListApplications@ operation.
--
-- /See:/ 'newListApplications' smart constructor.
data ListApplications = ListApplications'
  { -- | An identifier returned from the previous list applications call. It can
    -- be used to return the next set of applications in the list.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplications_nextToken' - An identifier returned from the previous list applications call. It can
-- be used to return the next set of applications in the list.
newListApplications ::
  ListApplications
newListApplications =
  ListApplications' {nextToken = Core.Nothing}

-- | An identifier returned from the previous list applications call. It can
-- be used to return the next set of applications in the list.
listApplications_nextToken :: Lens.Lens' ListApplications (Core.Maybe Core.Text)
listApplications_nextToken = Lens.lens (\ListApplications' {nextToken} -> nextToken) (\s@ListApplications' {} a -> s {nextToken = a} :: ListApplications)

instance Core.AWSPager ListApplications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationsResponse_applications
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listApplications_nextToken
          Lens..~ rs
          Lens.^? listApplicationsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListApplications where
  type
    AWSResponse ListApplications =
      ListApplicationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "applications" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListApplications

instance Core.NFData ListApplications

instance Core.ToHeaders ListApplications where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.ListApplications" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListApplications where
  toJSON ListApplications' {..} =
    Core.object
      ( Core.catMaybes
          [("nextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath ListApplications where
  toPath = Core.const "/"

instance Core.ToQuery ListApplications where
  toQuery = Core.const Core.mempty

-- | Represents the output of a ListApplications operation.
--
-- /See:/ 'newListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list applications call to
    -- return the next set of applications in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of application names.
    applications :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationsResponse_nextToken' - If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list applications call to
-- return the next set of applications in the list.
--
-- 'applications', 'listApplicationsResponse_applications' - A list of application names.
--
-- 'httpStatus', 'listApplicationsResponse_httpStatus' - The response's http status code.
newListApplicationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListApplicationsResponse
newListApplicationsResponse pHttpStatus_ =
  ListApplicationsResponse'
    { nextToken = Core.Nothing,
      applications = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list applications call to
-- return the next set of applications in the list.
listApplicationsResponse_nextToken :: Lens.Lens' ListApplicationsResponse (Core.Maybe Core.Text)
listApplicationsResponse_nextToken = Lens.lens (\ListApplicationsResponse' {nextToken} -> nextToken) (\s@ListApplicationsResponse' {} a -> s {nextToken = a} :: ListApplicationsResponse)

-- | A list of application names.
listApplicationsResponse_applications :: Lens.Lens' ListApplicationsResponse (Core.Maybe [Core.Text])
listApplicationsResponse_applications = Lens.lens (\ListApplicationsResponse' {applications} -> applications) (\s@ListApplicationsResponse' {} a -> s {applications = a} :: ListApplicationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listApplicationsResponse_httpStatus :: Lens.Lens' ListApplicationsResponse Core.Int
listApplicationsResponse_httpStatus = Lens.lens (\ListApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationsResponse' {} a -> s {httpStatus = a} :: ListApplicationsResponse)

instance Core.NFData ListApplicationsResponse
