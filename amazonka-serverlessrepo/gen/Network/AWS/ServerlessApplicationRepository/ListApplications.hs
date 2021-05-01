{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists applications owned by the requester.
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplications
  ( -- * Creating a Request
    ListApplications (..),
    newListApplications,

    -- * Request Lenses
    listApplications_nextToken,
    listApplications_maxItems,

    -- * Destructuring the Response
    ListApplicationsResponse (..),
    newListApplicationsResponse,

    -- * Response Lenses
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newListApplications' smart constructor.
data ListApplications = ListApplications'
  { -- | A token to specify where to start paginating.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of items to return.
    maxItems :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplications_nextToken' - A token to specify where to start paginating.
--
-- 'maxItems', 'listApplications_maxItems' - The total number of items to return.
newListApplications ::
  ListApplications
newListApplications =
  ListApplications'
    { nextToken = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | A token to specify where to start paginating.
listApplications_nextToken :: Lens.Lens' ListApplications (Prelude.Maybe Prelude.Text)
listApplications_nextToken = Lens.lens (\ListApplications' {nextToken} -> nextToken) (\s@ListApplications' {} a -> s {nextToken = a} :: ListApplications)

-- | The total number of items to return.
listApplications_maxItems :: Lens.Lens' ListApplications (Prelude.Maybe Prelude.Natural)
listApplications_maxItems = Lens.lens (\ListApplications' {maxItems} -> maxItems) (\s@ListApplications' {} a -> s {maxItems = a} :: ListApplications)

instance Pager.AWSPager ListApplications where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listApplicationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listApplicationsResponse_applications
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listApplications_nextToken
          Lens..~ rs
          Lens.^? listApplicationsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListApplications where
  type Rs ListApplications = ListApplicationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "applications"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplications

instance Prelude.NFData ListApplications

instance Prelude.ToHeaders ListApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListApplications where
  toPath = Prelude.const "/applications"

instance Prelude.ToQuery ListApplications where
  toQuery ListApplications' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxItems" Prelude.=: maxItems
      ]

-- | /See:/ 'newListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of application summaries.
    applications :: Prelude.Maybe [ApplicationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationsResponse_nextToken' - The token to request the next page of results.
--
-- 'applications', 'listApplicationsResponse_applications' - An array of application summaries.
--
-- 'httpStatus', 'listApplicationsResponse_httpStatus' - The response's http status code.
newListApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationsResponse
newListApplicationsResponse pHttpStatus_ =
  ListApplicationsResponse'
    { nextToken =
        Prelude.Nothing,
      applications = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to request the next page of results.
listApplicationsResponse_nextToken :: Lens.Lens' ListApplicationsResponse (Prelude.Maybe Prelude.Text)
listApplicationsResponse_nextToken = Lens.lens (\ListApplicationsResponse' {nextToken} -> nextToken) (\s@ListApplicationsResponse' {} a -> s {nextToken = a} :: ListApplicationsResponse)

-- | An array of application summaries.
listApplicationsResponse_applications :: Lens.Lens' ListApplicationsResponse (Prelude.Maybe [ApplicationSummary])
listApplicationsResponse_applications = Lens.lens (\ListApplicationsResponse' {applications} -> applications) (\s@ListApplicationsResponse' {} a -> s {applications = a} :: ListApplicationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listApplicationsResponse_httpStatus :: Lens.Lens' ListApplicationsResponse Prelude.Int
listApplicationsResponse_httpStatus = Lens.lens (\ListApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationsResponse' {} a -> s {httpStatus = a} :: ListApplicationsResponse)

instance Prelude.NFData ListApplicationsResponse
