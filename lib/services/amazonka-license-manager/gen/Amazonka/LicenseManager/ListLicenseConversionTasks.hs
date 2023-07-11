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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the license type conversion tasks for your account.
module Amazonka.LicenseManager.ListLicenseConversionTasks
  ( -- * Creating a Request
    ListLicenseConversionTasks (..),
    newListLicenseConversionTasks,

    -- * Request Lenses
    listLicenseConversionTasks_filters,
    listLicenseConversionTasks_maxResults,
    listLicenseConversionTasks_nextToken,

    -- * Destructuring the Response
    ListLicenseConversionTasksResponse (..),
    newListLicenseConversionTasksResponse,

    -- * Response Lenses
    listLicenseConversionTasksResponse_licenseConversionTasks,
    listLicenseConversionTasksResponse_nextToken,
    listLicenseConversionTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLicenseConversionTasks' smart constructor.
data ListLicenseConversionTasks = ListLicenseConversionTasks'
  { -- | Filters to scope the results. Valid filters are @ResourceArns@ and
    -- @Status@.
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filters', 'listLicenseConversionTasks_filters' - Filters to scope the results. Valid filters are @ResourceArns@ and
-- @Status@.
--
-- 'maxResults', 'listLicenseConversionTasks_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listLicenseConversionTasks_nextToken' - Token for the next set of results.
newListLicenseConversionTasks ::
  ListLicenseConversionTasks
newListLicenseConversionTasks =
  ListLicenseConversionTasks'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters to scope the results. Valid filters are @ResourceArns@ and
-- @Status@.
listLicenseConversionTasks_filters :: Lens.Lens' ListLicenseConversionTasks (Prelude.Maybe [Filter])
listLicenseConversionTasks_filters = Lens.lens (\ListLicenseConversionTasks' {filters} -> filters) (\s@ListLicenseConversionTasks' {} a -> s {filters = a} :: ListLicenseConversionTasks) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listLicenseConversionTasks_maxResults :: Lens.Lens' ListLicenseConversionTasks (Prelude.Maybe Prelude.Int)
listLicenseConversionTasks_maxResults = Lens.lens (\ListLicenseConversionTasks' {maxResults} -> maxResults) (\s@ListLicenseConversionTasks' {} a -> s {maxResults = a} :: ListLicenseConversionTasks)

-- | Token for the next set of results.
listLicenseConversionTasks_nextToken :: Lens.Lens' ListLicenseConversionTasks (Prelude.Maybe Prelude.Text)
listLicenseConversionTasks_nextToken = Lens.lens (\ListLicenseConversionTasks' {nextToken} -> nextToken) (\s@ListLicenseConversionTasks' {} a -> s {nextToken = a} :: ListLicenseConversionTasks)

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
            Prelude.<$> ( x
                            Data..?> "LicenseConversionTasks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLicenseConversionTasks where
  hashWithSalt _salt ListLicenseConversionTasks' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLicenseConversionTasks where
  rnf ListLicenseConversionTasks' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLicenseConversionTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListLicenseConversionTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLicenseConversionTasks where
  toJSON ListLicenseConversionTasks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLicenseConversionTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLicenseConversionTasks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLicenseConversionTasksResponse' smart constructor.
data ListLicenseConversionTasksResponse = ListLicenseConversionTasksResponse'
  { -- | Information about the license configuration tasks for your account.
    licenseConversionTasks :: Prelude.Maybe [LicenseConversionTask],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'licenseConversionTasks', 'listLicenseConversionTasksResponse_licenseConversionTasks' - Information about the license configuration tasks for your account.
--
-- 'nextToken', 'listLicenseConversionTasksResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listLicenseConversionTasksResponse_httpStatus' - The response's http status code.
newListLicenseConversionTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLicenseConversionTasksResponse
newListLicenseConversionTasksResponse pHttpStatus_ =
  ListLicenseConversionTasksResponse'
    { licenseConversionTasks =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the license configuration tasks for your account.
listLicenseConversionTasksResponse_licenseConversionTasks :: Lens.Lens' ListLicenseConversionTasksResponse (Prelude.Maybe [LicenseConversionTask])
listLicenseConversionTasksResponse_licenseConversionTasks = Lens.lens (\ListLicenseConversionTasksResponse' {licenseConversionTasks} -> licenseConversionTasks) (\s@ListLicenseConversionTasksResponse' {} a -> s {licenseConversionTasks = a} :: ListLicenseConversionTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listLicenseConversionTasksResponse_nextToken :: Lens.Lens' ListLicenseConversionTasksResponse (Prelude.Maybe Prelude.Text)
listLicenseConversionTasksResponse_nextToken = Lens.lens (\ListLicenseConversionTasksResponse' {nextToken} -> nextToken) (\s@ListLicenseConversionTasksResponse' {} a -> s {nextToken = a} :: ListLicenseConversionTasksResponse)

-- | The response's http status code.
listLicenseConversionTasksResponse_httpStatus :: Lens.Lens' ListLicenseConversionTasksResponse Prelude.Int
listLicenseConversionTasksResponse_httpStatus = Lens.lens (\ListLicenseConversionTasksResponse' {httpStatus} -> httpStatus) (\s@ListLicenseConversionTasksResponse' {} a -> s {httpStatus = a} :: ListLicenseConversionTasksResponse)

instance
  Prelude.NFData
    ListLicenseConversionTasksResponse
  where
  rnf ListLicenseConversionTasksResponse' {..} =
    Prelude.rnf licenseConversionTasks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
