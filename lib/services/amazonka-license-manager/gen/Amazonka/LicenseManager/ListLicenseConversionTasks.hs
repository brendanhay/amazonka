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
-- Module      : Amazonka.LicenseManager.ListLicenseConversionTasks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the license type conversion tasks for your account.
module Amazonka.LicenseManager.ListLicenseConversionTasks
  ( -- * Creating a Request
    ListLicenseConversionTasks (..),
    newListLicenseConversionTasks,

    -- * Request Lenses
    listLicenseConversionTasks_nextToken,
    listLicenseConversionTasks_filters,
    listLicenseConversionTasks_maxResults,

    -- * Destructuring the Response
    ListLicenseConversionTasksResponse (..),
    newListLicenseConversionTasksResponse,

    -- * Response Lenses
    listLicenseConversionTasksResponse_nextToken,
    listLicenseConversionTasksResponse_licenseConversionTasks,
    listLicenseConversionTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLicenseConversionTasks' smart constructor.
data ListLicenseConversionTasks = ListLicenseConversionTasks'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters to scope the results. Valid filters are @ResourceArns@ and
    -- @Status@.
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLicenseConversionTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLicenseConversionTasks_nextToken' - Token for the next set of results.
--
-- 'filters', 'listLicenseConversionTasks_filters' - Filters to scope the results. Valid filters are @ResourceArns@ and
-- @Status@.
--
-- 'maxResults', 'listLicenseConversionTasks_maxResults' - Maximum number of results to return in a single call.
newListLicenseConversionTasks ::
  ListLicenseConversionTasks
newListLicenseConversionTasks =
  ListLicenseConversionTasks'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Token for the next set of results.
listLicenseConversionTasks_nextToken :: Lens.Lens' ListLicenseConversionTasks (Prelude.Maybe Prelude.Text)
listLicenseConversionTasks_nextToken = Lens.lens (\ListLicenseConversionTasks' {nextToken} -> nextToken) (\s@ListLicenseConversionTasks' {} a -> s {nextToken = a} :: ListLicenseConversionTasks)

-- | Filters to scope the results. Valid filters are @ResourceArns@ and
-- @Status@.
listLicenseConversionTasks_filters :: Lens.Lens' ListLicenseConversionTasks (Prelude.Maybe [Filter])
listLicenseConversionTasks_filters = Lens.lens (\ListLicenseConversionTasks' {filters} -> filters) (\s@ListLicenseConversionTasks' {} a -> s {filters = a} :: ListLicenseConversionTasks) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listLicenseConversionTasks_maxResults :: Lens.Lens' ListLicenseConversionTasks (Prelude.Maybe Prelude.Int)
listLicenseConversionTasks_maxResults = Lens.lens (\ListLicenseConversionTasks' {maxResults} -> maxResults) (\s@ListLicenseConversionTasks' {} a -> s {maxResults = a} :: ListLicenseConversionTasks)

instance Core.AWSRequest ListLicenseConversionTasks where
  type
    AWSResponse ListLicenseConversionTasks =
      ListLicenseConversionTasksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLicenseConversionTasksResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "LicenseConversionTasks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLicenseConversionTasks where
  hashWithSalt _salt ListLicenseConversionTasks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListLicenseConversionTasks where
  rnf ListLicenseConversionTasks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListLicenseConversionTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.ListLicenseConversionTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListLicenseConversionTasks where
  toJSON ListLicenseConversionTasks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListLicenseConversionTasks where
  toPath = Prelude.const "/"

instance Core.ToQuery ListLicenseConversionTasks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLicenseConversionTasksResponse' smart constructor.
data ListLicenseConversionTasksResponse = ListLicenseConversionTasksResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the license configuration tasks for your account.
    licenseConversionTasks :: Prelude.Maybe [LicenseConversionTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLicenseConversionTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLicenseConversionTasksResponse_nextToken' - Token for the next set of results.
--
-- 'licenseConversionTasks', 'listLicenseConversionTasksResponse_licenseConversionTasks' - Information about the license configuration tasks for your account.
--
-- 'httpStatus', 'listLicenseConversionTasksResponse_httpStatus' - The response's http status code.
newListLicenseConversionTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLicenseConversionTasksResponse
newListLicenseConversionTasksResponse pHttpStatus_ =
  ListLicenseConversionTasksResponse'
    { nextToken =
        Prelude.Nothing,
      licenseConversionTasks =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for the next set of results.
listLicenseConversionTasksResponse_nextToken :: Lens.Lens' ListLicenseConversionTasksResponse (Prelude.Maybe Prelude.Text)
listLicenseConversionTasksResponse_nextToken = Lens.lens (\ListLicenseConversionTasksResponse' {nextToken} -> nextToken) (\s@ListLicenseConversionTasksResponse' {} a -> s {nextToken = a} :: ListLicenseConversionTasksResponse)

-- | Information about the license configuration tasks for your account.
listLicenseConversionTasksResponse_licenseConversionTasks :: Lens.Lens' ListLicenseConversionTasksResponse (Prelude.Maybe [LicenseConversionTask])
listLicenseConversionTasksResponse_licenseConversionTasks = Lens.lens (\ListLicenseConversionTasksResponse' {licenseConversionTasks} -> licenseConversionTasks) (\s@ListLicenseConversionTasksResponse' {} a -> s {licenseConversionTasks = a} :: ListLicenseConversionTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listLicenseConversionTasksResponse_httpStatus :: Lens.Lens' ListLicenseConversionTasksResponse Prelude.Int
listLicenseConversionTasksResponse_httpStatus = Lens.lens (\ListLicenseConversionTasksResponse' {httpStatus} -> httpStatus) (\s@ListLicenseConversionTasksResponse' {} a -> s {httpStatus = a} :: ListLicenseConversionTasksResponse)

instance
  Prelude.NFData
    ListLicenseConversionTasksResponse
  where
  rnf ListLicenseConversionTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf licenseConversionTasks
      `Prelude.seq` Prelude.rnf httpStatus
