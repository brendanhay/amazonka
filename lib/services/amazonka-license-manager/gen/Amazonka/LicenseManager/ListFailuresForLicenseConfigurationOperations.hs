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
-- Module      : Amazonka.LicenseManager.ListFailuresForLicenseConfigurationOperations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the license configuration operations that failed.
module Amazonka.LicenseManager.ListFailuresForLicenseConfigurationOperations
  ( -- * Creating a Request
    ListFailuresForLicenseConfigurationOperations (..),
    newListFailuresForLicenseConfigurationOperations,

    -- * Request Lenses
    listFailuresForLicenseConfigurationOperations_maxResults,
    listFailuresForLicenseConfigurationOperations_nextToken,
    listFailuresForLicenseConfigurationOperations_licenseConfigurationArn,

    -- * Destructuring the Response
    ListFailuresForLicenseConfigurationOperationsResponse (..),
    newListFailuresForLicenseConfigurationOperationsResponse,

    -- * Response Lenses
    listFailuresForLicenseConfigurationOperationsResponse_licenseOperationFailureList,
    listFailuresForLicenseConfigurationOperationsResponse_nextToken,
    listFailuresForLicenseConfigurationOperationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFailuresForLicenseConfigurationOperations' smart constructor.
data ListFailuresForLicenseConfigurationOperations = ListFailuresForLicenseConfigurationOperations'
  { -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name of the license configuration.
    licenseConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFailuresForLicenseConfigurationOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFailuresForLicenseConfigurationOperations_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listFailuresForLicenseConfigurationOperations_nextToken' - Token for the next set of results.
--
-- 'licenseConfigurationArn', 'listFailuresForLicenseConfigurationOperations_licenseConfigurationArn' - Amazon Resource Name of the license configuration.
newListFailuresForLicenseConfigurationOperations ::
  -- | 'licenseConfigurationArn'
  Prelude.Text ->
  ListFailuresForLicenseConfigurationOperations
newListFailuresForLicenseConfigurationOperations
  pLicenseConfigurationArn_ =
    ListFailuresForLicenseConfigurationOperations'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        licenseConfigurationArn =
          pLicenseConfigurationArn_
      }

-- | Maximum number of results to return in a single call.
listFailuresForLicenseConfigurationOperations_maxResults :: Lens.Lens' ListFailuresForLicenseConfigurationOperations (Prelude.Maybe Prelude.Int)
listFailuresForLicenseConfigurationOperations_maxResults = Lens.lens (\ListFailuresForLicenseConfigurationOperations' {maxResults} -> maxResults) (\s@ListFailuresForLicenseConfigurationOperations' {} a -> s {maxResults = a} :: ListFailuresForLicenseConfigurationOperations)

-- | Token for the next set of results.
listFailuresForLicenseConfigurationOperations_nextToken :: Lens.Lens' ListFailuresForLicenseConfigurationOperations (Prelude.Maybe Prelude.Text)
listFailuresForLicenseConfigurationOperations_nextToken = Lens.lens (\ListFailuresForLicenseConfigurationOperations' {nextToken} -> nextToken) (\s@ListFailuresForLicenseConfigurationOperations' {} a -> s {nextToken = a} :: ListFailuresForLicenseConfigurationOperations)

-- | Amazon Resource Name of the license configuration.
listFailuresForLicenseConfigurationOperations_licenseConfigurationArn :: Lens.Lens' ListFailuresForLicenseConfigurationOperations Prelude.Text
listFailuresForLicenseConfigurationOperations_licenseConfigurationArn = Lens.lens (\ListFailuresForLicenseConfigurationOperations' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@ListFailuresForLicenseConfigurationOperations' {} a -> s {licenseConfigurationArn = a} :: ListFailuresForLicenseConfigurationOperations)

instance
  Core.AWSRequest
    ListFailuresForLicenseConfigurationOperations
  where
  type
    AWSResponse
      ListFailuresForLicenseConfigurationOperations =
      ListFailuresForLicenseConfigurationOperationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFailuresForLicenseConfigurationOperationsResponse'
            Prelude.<$> ( x Data..?> "LicenseOperationFailureList"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFailuresForLicenseConfigurationOperations
  where
  hashWithSalt
    _salt
    ListFailuresForLicenseConfigurationOperations' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` licenseConfigurationArn

instance
  Prelude.NFData
    ListFailuresForLicenseConfigurationOperations
  where
  rnf
    ListFailuresForLicenseConfigurationOperations' {..} =
      Prelude.rnf maxResults
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf licenseConfigurationArn

instance
  Data.ToHeaders
    ListFailuresForLicenseConfigurationOperations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListFailuresForLicenseConfigurationOperations" ::
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
    ListFailuresForLicenseConfigurationOperations
  where
  toJSON
    ListFailuresForLicenseConfigurationOperations' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("MaxResults" Data..=) Prelude.<$> maxResults,
              ("NextToken" Data..=) Prelude.<$> nextToken,
              Prelude.Just
                ( "LicenseConfigurationArn"
                    Data..= licenseConfigurationArn
                )
            ]
        )

instance
  Data.ToPath
    ListFailuresForLicenseConfigurationOperations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListFailuresForLicenseConfigurationOperations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFailuresForLicenseConfigurationOperationsResponse' smart constructor.
data ListFailuresForLicenseConfigurationOperationsResponse = ListFailuresForLicenseConfigurationOperationsResponse'
  { -- | License configuration operations that failed.
    licenseOperationFailureList :: Prelude.Maybe [LicenseOperationFailure],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFailuresForLicenseConfigurationOperationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseOperationFailureList', 'listFailuresForLicenseConfigurationOperationsResponse_licenseOperationFailureList' - License configuration operations that failed.
--
-- 'nextToken', 'listFailuresForLicenseConfigurationOperationsResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listFailuresForLicenseConfigurationOperationsResponse_httpStatus' - The response's http status code.
newListFailuresForLicenseConfigurationOperationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFailuresForLicenseConfigurationOperationsResponse
newListFailuresForLicenseConfigurationOperationsResponse
  pHttpStatus_ =
    ListFailuresForLicenseConfigurationOperationsResponse'
      { licenseOperationFailureList =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | License configuration operations that failed.
listFailuresForLicenseConfigurationOperationsResponse_licenseOperationFailureList :: Lens.Lens' ListFailuresForLicenseConfigurationOperationsResponse (Prelude.Maybe [LicenseOperationFailure])
listFailuresForLicenseConfigurationOperationsResponse_licenseOperationFailureList = Lens.lens (\ListFailuresForLicenseConfigurationOperationsResponse' {licenseOperationFailureList} -> licenseOperationFailureList) (\s@ListFailuresForLicenseConfigurationOperationsResponse' {} a -> s {licenseOperationFailureList = a} :: ListFailuresForLicenseConfigurationOperationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listFailuresForLicenseConfigurationOperationsResponse_nextToken :: Lens.Lens' ListFailuresForLicenseConfigurationOperationsResponse (Prelude.Maybe Prelude.Text)
listFailuresForLicenseConfigurationOperationsResponse_nextToken = Lens.lens (\ListFailuresForLicenseConfigurationOperationsResponse' {nextToken} -> nextToken) (\s@ListFailuresForLicenseConfigurationOperationsResponse' {} a -> s {nextToken = a} :: ListFailuresForLicenseConfigurationOperationsResponse)

-- | The response's http status code.
listFailuresForLicenseConfigurationOperationsResponse_httpStatus :: Lens.Lens' ListFailuresForLicenseConfigurationOperationsResponse Prelude.Int
listFailuresForLicenseConfigurationOperationsResponse_httpStatus = Lens.lens (\ListFailuresForLicenseConfigurationOperationsResponse' {httpStatus} -> httpStatus) (\s@ListFailuresForLicenseConfigurationOperationsResponse' {} a -> s {httpStatus = a} :: ListFailuresForLicenseConfigurationOperationsResponse)

instance
  Prelude.NFData
    ListFailuresForLicenseConfigurationOperationsResponse
  where
  rnf
    ListFailuresForLicenseConfigurationOperationsResponse' {..} =
      Prelude.rnf licenseOperationFailureList
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
