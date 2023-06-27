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
-- Module      : Amazonka.LicenseManager.ListLicenseSpecificationsForResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the license configurations for the specified resource.
--
-- This operation returns paginated results.
module Amazonka.LicenseManager.ListLicenseSpecificationsForResource
  ( -- * Creating a Request
    ListLicenseSpecificationsForResource (..),
    newListLicenseSpecificationsForResource,

    -- * Request Lenses
    listLicenseSpecificationsForResource_maxResults,
    listLicenseSpecificationsForResource_nextToken,
    listLicenseSpecificationsForResource_resourceArn,

    -- * Destructuring the Response
    ListLicenseSpecificationsForResourceResponse (..),
    newListLicenseSpecificationsForResourceResponse,

    -- * Response Lenses
    listLicenseSpecificationsForResourceResponse_licenseSpecifications,
    listLicenseSpecificationsForResourceResponse_nextToken,
    listLicenseSpecificationsForResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLicenseSpecificationsForResource' smart constructor.
data ListLicenseSpecificationsForResource = ListLicenseSpecificationsForResource'
  { -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of a resource that has an associated license
    -- configuration.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLicenseSpecificationsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLicenseSpecificationsForResource_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listLicenseSpecificationsForResource_nextToken' - Token for the next set of results.
--
-- 'resourceArn', 'listLicenseSpecificationsForResource_resourceArn' - Amazon Resource Name (ARN) of a resource that has an associated license
-- configuration.
newListLicenseSpecificationsForResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  ListLicenseSpecificationsForResource
newListLicenseSpecificationsForResource pResourceArn_ =
  ListLicenseSpecificationsForResource'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | Maximum number of results to return in a single call.
listLicenseSpecificationsForResource_maxResults :: Lens.Lens' ListLicenseSpecificationsForResource (Prelude.Maybe Prelude.Int)
listLicenseSpecificationsForResource_maxResults = Lens.lens (\ListLicenseSpecificationsForResource' {maxResults} -> maxResults) (\s@ListLicenseSpecificationsForResource' {} a -> s {maxResults = a} :: ListLicenseSpecificationsForResource)

-- | Token for the next set of results.
listLicenseSpecificationsForResource_nextToken :: Lens.Lens' ListLicenseSpecificationsForResource (Prelude.Maybe Prelude.Text)
listLicenseSpecificationsForResource_nextToken = Lens.lens (\ListLicenseSpecificationsForResource' {nextToken} -> nextToken) (\s@ListLicenseSpecificationsForResource' {} a -> s {nextToken = a} :: ListLicenseSpecificationsForResource)

-- | Amazon Resource Name (ARN) of a resource that has an associated license
-- configuration.
listLicenseSpecificationsForResource_resourceArn :: Lens.Lens' ListLicenseSpecificationsForResource Prelude.Text
listLicenseSpecificationsForResource_resourceArn = Lens.lens (\ListLicenseSpecificationsForResource' {resourceArn} -> resourceArn) (\s@ListLicenseSpecificationsForResource' {} a -> s {resourceArn = a} :: ListLicenseSpecificationsForResource)

instance
  Core.AWSPager
    ListLicenseSpecificationsForResource
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLicenseSpecificationsForResourceResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLicenseSpecificationsForResourceResponse_licenseSpecifications
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listLicenseSpecificationsForResource_nextToken
          Lens..~ rs
          Lens.^? listLicenseSpecificationsForResourceResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListLicenseSpecificationsForResource
  where
  type
    AWSResponse ListLicenseSpecificationsForResource =
      ListLicenseSpecificationsForResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLicenseSpecificationsForResourceResponse'
            Prelude.<$> ( x
                            Data..?> "LicenseSpecifications"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListLicenseSpecificationsForResource
  where
  hashWithSalt
    _salt
    ListLicenseSpecificationsForResource' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` resourceArn

instance
  Prelude.NFData
    ListLicenseSpecificationsForResource
  where
  rnf ListLicenseSpecificationsForResource' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceArn

instance
  Data.ToHeaders
    ListLicenseSpecificationsForResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListLicenseSpecificationsForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListLicenseSpecificationsForResource
  where
  toJSON ListLicenseSpecificationsForResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )

instance
  Data.ToPath
    ListLicenseSpecificationsForResource
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListLicenseSpecificationsForResource
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLicenseSpecificationsForResourceResponse' smart constructor.
data ListLicenseSpecificationsForResourceResponse = ListLicenseSpecificationsForResourceResponse'
  { -- | License configurations associated with a resource.
    licenseSpecifications :: Prelude.Maybe [LicenseSpecification],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLicenseSpecificationsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseSpecifications', 'listLicenseSpecificationsForResourceResponse_licenseSpecifications' - License configurations associated with a resource.
--
-- 'nextToken', 'listLicenseSpecificationsForResourceResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listLicenseSpecificationsForResourceResponse_httpStatus' - The response's http status code.
newListLicenseSpecificationsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLicenseSpecificationsForResourceResponse
newListLicenseSpecificationsForResourceResponse
  pHttpStatus_ =
    ListLicenseSpecificationsForResourceResponse'
      { licenseSpecifications =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | License configurations associated with a resource.
listLicenseSpecificationsForResourceResponse_licenseSpecifications :: Lens.Lens' ListLicenseSpecificationsForResourceResponse (Prelude.Maybe [LicenseSpecification])
listLicenseSpecificationsForResourceResponse_licenseSpecifications = Lens.lens (\ListLicenseSpecificationsForResourceResponse' {licenseSpecifications} -> licenseSpecifications) (\s@ListLicenseSpecificationsForResourceResponse' {} a -> s {licenseSpecifications = a} :: ListLicenseSpecificationsForResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listLicenseSpecificationsForResourceResponse_nextToken :: Lens.Lens' ListLicenseSpecificationsForResourceResponse (Prelude.Maybe Prelude.Text)
listLicenseSpecificationsForResourceResponse_nextToken = Lens.lens (\ListLicenseSpecificationsForResourceResponse' {nextToken} -> nextToken) (\s@ListLicenseSpecificationsForResourceResponse' {} a -> s {nextToken = a} :: ListLicenseSpecificationsForResourceResponse)

-- | The response's http status code.
listLicenseSpecificationsForResourceResponse_httpStatus :: Lens.Lens' ListLicenseSpecificationsForResourceResponse Prelude.Int
listLicenseSpecificationsForResourceResponse_httpStatus = Lens.lens (\ListLicenseSpecificationsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListLicenseSpecificationsForResourceResponse' {} a -> s {httpStatus = a} :: ListLicenseSpecificationsForResourceResponse)

instance
  Prelude.NFData
    ListLicenseSpecificationsForResourceResponse
  where
  rnf ListLicenseSpecificationsForResourceResponse' {..} =
    Prelude.rnf licenseSpecifications
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
