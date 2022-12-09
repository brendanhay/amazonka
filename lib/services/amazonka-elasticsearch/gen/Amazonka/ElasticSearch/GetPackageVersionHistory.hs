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
-- Module      : Amazonka.ElasticSearch.GetPackageVersionHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of versions of the package, along with their creation
-- time and commit message.
module Amazonka.ElasticSearch.GetPackageVersionHistory
  ( -- * Creating a Request
    GetPackageVersionHistory (..),
    newGetPackageVersionHistory,

    -- * Request Lenses
    getPackageVersionHistory_maxResults,
    getPackageVersionHistory_nextToken,
    getPackageVersionHistory_packageID,

    -- * Destructuring the Response
    GetPackageVersionHistoryResponse (..),
    newGetPackageVersionHistoryResponse,

    -- * Response Lenses
    getPackageVersionHistoryResponse_nextToken,
    getPackageVersionHistoryResponse_packageID,
    getPackageVersionHistoryResponse_packageVersionHistoryList,
    getPackageVersionHistoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to @ GetPackageVersionHistory @
-- operation.
--
-- /See:/ 'newGetPackageVersionHistory' smart constructor.
data GetPackageVersionHistory = GetPackageVersionHistory'
  { -- | Limits results to a maximum number of versions.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Used for pagination. Only necessary if a previous API call includes a
    -- non-null NextToken value. If provided, returns results for the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns an audit history of versions of the package.
    packageID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageVersionHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getPackageVersionHistory_maxResults' - Limits results to a maximum number of versions.
--
-- 'nextToken', 'getPackageVersionHistory_nextToken' - Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
--
-- 'packageID', 'getPackageVersionHistory_packageID' - Returns an audit history of versions of the package.
newGetPackageVersionHistory ::
  -- | 'packageID'
  Prelude.Text ->
  GetPackageVersionHistory
newGetPackageVersionHistory pPackageID_ =
  GetPackageVersionHistory'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      packageID = pPackageID_
    }

-- | Limits results to a maximum number of versions.
getPackageVersionHistory_maxResults :: Lens.Lens' GetPackageVersionHistory (Prelude.Maybe Prelude.Int)
getPackageVersionHistory_maxResults = Lens.lens (\GetPackageVersionHistory' {maxResults} -> maxResults) (\s@GetPackageVersionHistory' {} a -> s {maxResults = a} :: GetPackageVersionHistory)

-- | Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
getPackageVersionHistory_nextToken :: Lens.Lens' GetPackageVersionHistory (Prelude.Maybe Prelude.Text)
getPackageVersionHistory_nextToken = Lens.lens (\GetPackageVersionHistory' {nextToken} -> nextToken) (\s@GetPackageVersionHistory' {} a -> s {nextToken = a} :: GetPackageVersionHistory)

-- | Returns an audit history of versions of the package.
getPackageVersionHistory_packageID :: Lens.Lens' GetPackageVersionHistory Prelude.Text
getPackageVersionHistory_packageID = Lens.lens (\GetPackageVersionHistory' {packageID} -> packageID) (\s@GetPackageVersionHistory' {} a -> s {packageID = a} :: GetPackageVersionHistory)

instance Core.AWSRequest GetPackageVersionHistory where
  type
    AWSResponse GetPackageVersionHistory =
      GetPackageVersionHistoryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPackageVersionHistoryResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "PackageID")
            Prelude.<*> ( x Data..?> "PackageVersionHistoryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPackageVersionHistory where
  hashWithSalt _salt GetPackageVersionHistory' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` packageID

instance Prelude.NFData GetPackageVersionHistory where
  rnf GetPackageVersionHistory' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf packageID

instance Data.ToHeaders GetPackageVersionHistory where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPackageVersionHistory where
  toPath GetPackageVersionHistory' {..} =
    Prelude.mconcat
      [ "/2015-01-01/packages/",
        Data.toBS packageID,
        "/history"
      ]

instance Data.ToQuery GetPackageVersionHistory where
  toQuery GetPackageVersionHistory' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Container for response returned by @ GetPackageVersionHistory @
-- operation.
--
-- /See:/ 'newGetPackageVersionHistoryResponse' smart constructor.
data GetPackageVersionHistoryResponse = GetPackageVersionHistoryResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    packageID :: Prelude.Maybe Prelude.Text,
    -- | List of @PackageVersionHistory@ objects.
    packageVersionHistoryList :: Prelude.Maybe [PackageVersionHistory],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageVersionHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getPackageVersionHistoryResponse_nextToken' - Undocumented member.
--
-- 'packageID', 'getPackageVersionHistoryResponse_packageID' - Undocumented member.
--
-- 'packageVersionHistoryList', 'getPackageVersionHistoryResponse_packageVersionHistoryList' - List of @PackageVersionHistory@ objects.
--
-- 'httpStatus', 'getPackageVersionHistoryResponse_httpStatus' - The response's http status code.
newGetPackageVersionHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPackageVersionHistoryResponse
newGetPackageVersionHistoryResponse pHttpStatus_ =
  GetPackageVersionHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      packageID = Prelude.Nothing,
      packageVersionHistoryList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getPackageVersionHistoryResponse_nextToken :: Lens.Lens' GetPackageVersionHistoryResponse (Prelude.Maybe Prelude.Text)
getPackageVersionHistoryResponse_nextToken = Lens.lens (\GetPackageVersionHistoryResponse' {nextToken} -> nextToken) (\s@GetPackageVersionHistoryResponse' {} a -> s {nextToken = a} :: GetPackageVersionHistoryResponse)

-- | Undocumented member.
getPackageVersionHistoryResponse_packageID :: Lens.Lens' GetPackageVersionHistoryResponse (Prelude.Maybe Prelude.Text)
getPackageVersionHistoryResponse_packageID = Lens.lens (\GetPackageVersionHistoryResponse' {packageID} -> packageID) (\s@GetPackageVersionHistoryResponse' {} a -> s {packageID = a} :: GetPackageVersionHistoryResponse)

-- | List of @PackageVersionHistory@ objects.
getPackageVersionHistoryResponse_packageVersionHistoryList :: Lens.Lens' GetPackageVersionHistoryResponse (Prelude.Maybe [PackageVersionHistory])
getPackageVersionHistoryResponse_packageVersionHistoryList = Lens.lens (\GetPackageVersionHistoryResponse' {packageVersionHistoryList} -> packageVersionHistoryList) (\s@GetPackageVersionHistoryResponse' {} a -> s {packageVersionHistoryList = a} :: GetPackageVersionHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getPackageVersionHistoryResponse_httpStatus :: Lens.Lens' GetPackageVersionHistoryResponse Prelude.Int
getPackageVersionHistoryResponse_httpStatus = Lens.lens (\GetPackageVersionHistoryResponse' {httpStatus} -> httpStatus) (\s@GetPackageVersionHistoryResponse' {} a -> s {httpStatus = a} :: GetPackageVersionHistoryResponse)

instance
  Prelude.NFData
    GetPackageVersionHistoryResponse
  where
  rnf GetPackageVersionHistoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf packageID
      `Prelude.seq` Prelude.rnf packageVersionHistoryList
      `Prelude.seq` Prelude.rnf httpStatus
