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
-- Module      : Amazonka.ApplicationInsights.ListApplications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IDs of the applications that you are monitoring.
module Amazonka.ApplicationInsights.ListApplications
  ( -- * Creating a Request
    ListApplications (..),
    newListApplications,

    -- * Request Lenses
    listApplications_maxResults,
    listApplications_nextToken,

    -- * Destructuring the Response
    ListApplicationsResponse (..),
    newListApplicationsResponse,

    -- * Response Lenses
    listApplicationsResponse_applicationInfoList,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApplications' smart constructor.
data ListApplications = ListApplications'
  { -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listApplications_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'nextToken', 'listApplications_nextToken' - The token to request the next page of results.
newListApplications ::
  ListApplications
newListApplications =
  ListApplications'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
listApplications_maxResults :: Lens.Lens' ListApplications (Prelude.Maybe Prelude.Natural)
listApplications_maxResults = Lens.lens (\ListApplications' {maxResults} -> maxResults) (\s@ListApplications' {} a -> s {maxResults = a} :: ListApplications)

-- | The token to request the next page of results.
listApplications_nextToken :: Lens.Lens' ListApplications (Prelude.Maybe Prelude.Text)
listApplications_nextToken = Lens.lens (\ListApplications' {nextToken} -> nextToken) (\s@ListApplications' {} a -> s {nextToken = a} :: ListApplications)

instance Core.AWSRequest ListApplications where
  type
    AWSResponse ListApplications =
      ListApplicationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationsResponse'
            Prelude.<$> ( x
                            Data..?> "ApplicationInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplications where
  hashWithSalt _salt ListApplications' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListApplications where
  rnf ListApplications' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.ListApplications" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListApplications where
  toJSON ListApplications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListApplications where
  toPath = Prelude.const "/"

instance Data.ToQuery ListApplications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { -- | The list of applications.
    applicationInfoList :: Prelude.Maybe [ApplicationInfo],
    -- | The token used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInfoList', 'listApplicationsResponse_applicationInfoList' - The list of applications.
--
-- 'nextToken', 'listApplicationsResponse_nextToken' - The token used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listApplicationsResponse_httpStatus' - The response's http status code.
newListApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationsResponse
newListApplicationsResponse pHttpStatus_ =
  ListApplicationsResponse'
    { applicationInfoList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of applications.
listApplicationsResponse_applicationInfoList :: Lens.Lens' ListApplicationsResponse (Prelude.Maybe [ApplicationInfo])
listApplicationsResponse_applicationInfoList = Lens.lens (\ListApplicationsResponse' {applicationInfoList} -> applicationInfoList) (\s@ListApplicationsResponse' {} a -> s {applicationInfoList = a} :: ListApplicationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listApplicationsResponse_nextToken :: Lens.Lens' ListApplicationsResponse (Prelude.Maybe Prelude.Text)
listApplicationsResponse_nextToken = Lens.lens (\ListApplicationsResponse' {nextToken} -> nextToken) (\s@ListApplicationsResponse' {} a -> s {nextToken = a} :: ListApplicationsResponse)

-- | The response's http status code.
listApplicationsResponse_httpStatus :: Lens.Lens' ListApplicationsResponse Prelude.Int
listApplicationsResponse_httpStatus = Lens.lens (\ListApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationsResponse' {} a -> s {httpStatus = a} :: ListApplicationsResponse)

instance Prelude.NFData ListApplicationsResponse where
  rnf ListApplicationsResponse' {..} =
    Prelude.rnf applicationInfoList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
