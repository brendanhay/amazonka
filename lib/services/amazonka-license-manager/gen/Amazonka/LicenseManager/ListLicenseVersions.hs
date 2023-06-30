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
-- Module      : Amazonka.LicenseManager.ListLicenseVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all versions of the specified license.
module Amazonka.LicenseManager.ListLicenseVersions
  ( -- * Creating a Request
    ListLicenseVersions (..),
    newListLicenseVersions,

    -- * Request Lenses
    listLicenseVersions_maxResults,
    listLicenseVersions_nextToken,
    listLicenseVersions_licenseArn,

    -- * Destructuring the Response
    ListLicenseVersionsResponse (..),
    newListLicenseVersionsResponse,

    -- * Response Lenses
    listLicenseVersionsResponse_licenses,
    listLicenseVersionsResponse_nextToken,
    listLicenseVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLicenseVersions' smart constructor.
data ListLicenseVersions = ListLicenseVersions'
  { -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the license.
    licenseArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLicenseVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLicenseVersions_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listLicenseVersions_nextToken' - Token for the next set of results.
--
-- 'licenseArn', 'listLicenseVersions_licenseArn' - Amazon Resource Name (ARN) of the license.
newListLicenseVersions ::
  -- | 'licenseArn'
  Prelude.Text ->
  ListLicenseVersions
newListLicenseVersions pLicenseArn_ =
  ListLicenseVersions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      licenseArn = pLicenseArn_
    }

-- | Maximum number of results to return in a single call.
listLicenseVersions_maxResults :: Lens.Lens' ListLicenseVersions (Prelude.Maybe Prelude.Natural)
listLicenseVersions_maxResults = Lens.lens (\ListLicenseVersions' {maxResults} -> maxResults) (\s@ListLicenseVersions' {} a -> s {maxResults = a} :: ListLicenseVersions)

-- | Token for the next set of results.
listLicenseVersions_nextToken :: Lens.Lens' ListLicenseVersions (Prelude.Maybe Prelude.Text)
listLicenseVersions_nextToken = Lens.lens (\ListLicenseVersions' {nextToken} -> nextToken) (\s@ListLicenseVersions' {} a -> s {nextToken = a} :: ListLicenseVersions)

-- | Amazon Resource Name (ARN) of the license.
listLicenseVersions_licenseArn :: Lens.Lens' ListLicenseVersions Prelude.Text
listLicenseVersions_licenseArn = Lens.lens (\ListLicenseVersions' {licenseArn} -> licenseArn) (\s@ListLicenseVersions' {} a -> s {licenseArn = a} :: ListLicenseVersions)

instance Core.AWSRequest ListLicenseVersions where
  type
    AWSResponse ListLicenseVersions =
      ListLicenseVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLicenseVersionsResponse'
            Prelude.<$> (x Data..?> "Licenses" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLicenseVersions where
  hashWithSalt _salt ListLicenseVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` licenseArn

instance Prelude.NFData ListLicenseVersions where
  rnf ListLicenseVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf licenseArn

instance Data.ToHeaders ListLicenseVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListLicenseVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLicenseVersions where
  toJSON ListLicenseVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("LicenseArn" Data..= licenseArn)
          ]
      )

instance Data.ToPath ListLicenseVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLicenseVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLicenseVersionsResponse' smart constructor.
data ListLicenseVersionsResponse = ListLicenseVersionsResponse'
  { -- | License details.
    licenses :: Prelude.Maybe [License],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLicenseVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenses', 'listLicenseVersionsResponse_licenses' - License details.
--
-- 'nextToken', 'listLicenseVersionsResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listLicenseVersionsResponse_httpStatus' - The response's http status code.
newListLicenseVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLicenseVersionsResponse
newListLicenseVersionsResponse pHttpStatus_ =
  ListLicenseVersionsResponse'
    { licenses =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | License details.
listLicenseVersionsResponse_licenses :: Lens.Lens' ListLicenseVersionsResponse (Prelude.Maybe [License])
listLicenseVersionsResponse_licenses = Lens.lens (\ListLicenseVersionsResponse' {licenses} -> licenses) (\s@ListLicenseVersionsResponse' {} a -> s {licenses = a} :: ListLicenseVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listLicenseVersionsResponse_nextToken :: Lens.Lens' ListLicenseVersionsResponse (Prelude.Maybe Prelude.Text)
listLicenseVersionsResponse_nextToken = Lens.lens (\ListLicenseVersionsResponse' {nextToken} -> nextToken) (\s@ListLicenseVersionsResponse' {} a -> s {nextToken = a} :: ListLicenseVersionsResponse)

-- | The response's http status code.
listLicenseVersionsResponse_httpStatus :: Lens.Lens' ListLicenseVersionsResponse Prelude.Int
listLicenseVersionsResponse_httpStatus = Lens.lens (\ListLicenseVersionsResponse' {httpStatus} -> httpStatus) (\s@ListLicenseVersionsResponse' {} a -> s {httpStatus = a} :: ListLicenseVersionsResponse)

instance Prelude.NFData ListLicenseVersionsResponse where
  rnf ListLicenseVersionsResponse' {..} =
    Prelude.rnf licenses
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
