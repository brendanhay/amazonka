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
-- Module      : Network.AWS.DeviceFarm.ListTestGridProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all Selenium testing projects in your account.
module Network.AWS.DeviceFarm.ListTestGridProjects
  ( -- * Creating a Request
    ListTestGridProjects (..),
    newListTestGridProjects,

    -- * Request Lenses
    listTestGridProjects_nextToken,
    listTestGridProjects_maxResult,

    -- * Destructuring the Response
    ListTestGridProjectsResponse (..),
    newListTestGridProjectsResponse,

    -- * Response Lenses
    listTestGridProjectsResponse_nextToken,
    listTestGridProjectsResponse_testGridProjects,
    listTestGridProjectsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTestGridProjects' smart constructor.
data ListTestGridProjects = ListTestGridProjects'
  { -- | From a response, used to continue a paginated listing.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Return no more than this number of results.
    maxResult :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestGridProjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestGridProjects_nextToken' - From a response, used to continue a paginated listing.
--
-- 'maxResult', 'listTestGridProjects_maxResult' - Return no more than this number of results.
newListTestGridProjects ::
  ListTestGridProjects
newListTestGridProjects =
  ListTestGridProjects'
    { nextToken = Prelude.Nothing,
      maxResult = Prelude.Nothing
    }

-- | From a response, used to continue a paginated listing.
listTestGridProjects_nextToken :: Lens.Lens' ListTestGridProjects (Prelude.Maybe Prelude.Text)
listTestGridProjects_nextToken = Lens.lens (\ListTestGridProjects' {nextToken} -> nextToken) (\s@ListTestGridProjects' {} a -> s {nextToken = a} :: ListTestGridProjects)

-- | Return no more than this number of results.
listTestGridProjects_maxResult :: Lens.Lens' ListTestGridProjects (Prelude.Maybe Prelude.Natural)
listTestGridProjects_maxResult = Lens.lens (\ListTestGridProjects' {maxResult} -> maxResult) (\s@ListTestGridProjects' {} a -> s {maxResult = a} :: ListTestGridProjects)

instance Core.AWSRequest ListTestGridProjects where
  type
    AWSResponse ListTestGridProjects =
      ListTestGridProjectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestGridProjectsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "testGridProjects"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTestGridProjects

instance Prelude.NFData ListTestGridProjects

instance Core.ToHeaders ListTestGridProjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListTestGridProjects" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTestGridProjects where
  toJSON ListTestGridProjects' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResult" Core..=) Prelude.<$> maxResult
          ]
      )

instance Core.ToPath ListTestGridProjects where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTestGridProjects where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTestGridProjectsResponse' smart constructor.
data ListTestGridProjectsResponse = ListTestGridProjectsResponse'
  { -- | Used for pagination. Pass into ListTestGridProjects to get more results
    -- in a paginated request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of TestGridProjects, based on a ListTestGridProjectsRequest.
    testGridProjects :: Prelude.Maybe [TestGridProject],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestGridProjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestGridProjectsResponse_nextToken' - Used for pagination. Pass into ListTestGridProjects to get more results
-- in a paginated request.
--
-- 'testGridProjects', 'listTestGridProjectsResponse_testGridProjects' - The list of TestGridProjects, based on a ListTestGridProjectsRequest.
--
-- 'httpStatus', 'listTestGridProjectsResponse_httpStatus' - The response's http status code.
newListTestGridProjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTestGridProjectsResponse
newListTestGridProjectsResponse pHttpStatus_ =
  ListTestGridProjectsResponse'
    { nextToken =
        Prelude.Nothing,
      testGridProjects = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Used for pagination. Pass into ListTestGridProjects to get more results
-- in a paginated request.
listTestGridProjectsResponse_nextToken :: Lens.Lens' ListTestGridProjectsResponse (Prelude.Maybe Prelude.Text)
listTestGridProjectsResponse_nextToken = Lens.lens (\ListTestGridProjectsResponse' {nextToken} -> nextToken) (\s@ListTestGridProjectsResponse' {} a -> s {nextToken = a} :: ListTestGridProjectsResponse)

-- | The list of TestGridProjects, based on a ListTestGridProjectsRequest.
listTestGridProjectsResponse_testGridProjects :: Lens.Lens' ListTestGridProjectsResponse (Prelude.Maybe [TestGridProject])
listTestGridProjectsResponse_testGridProjects = Lens.lens (\ListTestGridProjectsResponse' {testGridProjects} -> testGridProjects) (\s@ListTestGridProjectsResponse' {} a -> s {testGridProjects = a} :: ListTestGridProjectsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTestGridProjectsResponse_httpStatus :: Lens.Lens' ListTestGridProjectsResponse Prelude.Int
listTestGridProjectsResponse_httpStatus = Lens.lens (\ListTestGridProjectsResponse' {httpStatus} -> httpStatus) (\s@ListTestGridProjectsResponse' {} a -> s {httpStatus = a} :: ListTestGridProjectsResponse)

instance Prelude.NFData ListTestGridProjectsResponse
