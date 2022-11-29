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
-- Module      : Amazonka.AppStream.DescribeApplications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more applications.
module Amazonka.AppStream.DescribeApplications
  ( -- * Creating a Request
    DescribeApplications (..),
    newDescribeApplications,

    -- * Request Lenses
    describeApplications_nextToken,
    describeApplications_arns,
    describeApplications_maxResults,

    -- * Destructuring the Response
    DescribeApplicationsResponse (..),
    newDescribeApplicationsResponse,

    -- * Response Lenses
    describeApplicationsResponse_nextToken,
    describeApplicationsResponse_applications,
    describeApplicationsResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApplications' smart constructor.
data DescribeApplications = DescribeApplications'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARNs for the applications.
    arns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeApplications_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'arns', 'describeApplications_arns' - The ARNs for the applications.
--
-- 'maxResults', 'describeApplications_maxResults' - The maximum size of each page of results.
newDescribeApplications ::
  DescribeApplications
newDescribeApplications =
  DescribeApplications'
    { nextToken = Prelude.Nothing,
      arns = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
describeApplications_nextToken :: Lens.Lens' DescribeApplications (Prelude.Maybe Prelude.Text)
describeApplications_nextToken = Lens.lens (\DescribeApplications' {nextToken} -> nextToken) (\s@DescribeApplications' {} a -> s {nextToken = a} :: DescribeApplications)

-- | The ARNs for the applications.
describeApplications_arns :: Lens.Lens' DescribeApplications (Prelude.Maybe [Prelude.Text])
describeApplications_arns = Lens.lens (\DescribeApplications' {arns} -> arns) (\s@DescribeApplications' {} a -> s {arns = a} :: DescribeApplications) Prelude.. Lens.mapping Lens.coerced

-- | The maximum size of each page of results.
describeApplications_maxResults :: Lens.Lens' DescribeApplications (Prelude.Maybe Prelude.Int)
describeApplications_maxResults = Lens.lens (\DescribeApplications' {maxResults} -> maxResults) (\s@DescribeApplications' {} a -> s {maxResults = a} :: DescribeApplications)

instance Core.AWSRequest DescribeApplications where
  type
    AWSResponse DescribeApplications =
      DescribeApplicationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Applications" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApplications where
  hashWithSalt _salt DescribeApplications' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` arns
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeApplications where
  rnf DescribeApplications' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf arns
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeApplications" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeApplications where
  toJSON DescribeApplications' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Arns" Core..=) Prelude.<$> arns,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeApplications where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeApplications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeApplicationsResponse' smart constructor.
data DescribeApplicationsResponse = DescribeApplicationsResponse'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The applications in the list.
    applications :: Prelude.Maybe [Application],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeApplicationsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'applications', 'describeApplicationsResponse_applications' - The applications in the list.
--
-- 'httpStatus', 'describeApplicationsResponse_httpStatus' - The response's http status code.
newDescribeApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeApplicationsResponse
newDescribeApplicationsResponse pHttpStatus_ =
  DescribeApplicationsResponse'
    { nextToken =
        Prelude.Nothing,
      applications = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
describeApplicationsResponse_nextToken :: Lens.Lens' DescribeApplicationsResponse (Prelude.Maybe Prelude.Text)
describeApplicationsResponse_nextToken = Lens.lens (\DescribeApplicationsResponse' {nextToken} -> nextToken) (\s@DescribeApplicationsResponse' {} a -> s {nextToken = a} :: DescribeApplicationsResponse)

-- | The applications in the list.
describeApplicationsResponse_applications :: Lens.Lens' DescribeApplicationsResponse (Prelude.Maybe [Application])
describeApplicationsResponse_applications = Lens.lens (\DescribeApplicationsResponse' {applications} -> applications) (\s@DescribeApplicationsResponse' {} a -> s {applications = a} :: DescribeApplicationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeApplicationsResponse_httpStatus :: Lens.Lens' DescribeApplicationsResponse Prelude.Int
describeApplicationsResponse_httpStatus = Lens.lens (\DescribeApplicationsResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationsResponse' {} a -> s {httpStatus = a} :: DescribeApplicationsResponse)

instance Prelude.NFData DescribeApplicationsResponse where
  rnf DescribeApplicationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf applications
      `Prelude.seq` Prelude.rnf httpStatus
