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
-- Module      : Amazonka.Glue.GetResourcePolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource policies set on individual resources by Resource
-- Access Manager during cross-account permission grants. Also retrieves
-- the Data Catalog resource policy.
--
-- If you enabled metadata encryption in Data Catalog settings, and you do
-- not have permission on the KMS key, the operation can\'t return the Data
-- Catalog resource policy.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetResourcePolicies
  ( -- * Creating a Request
    GetResourcePolicies (..),
    newGetResourcePolicies,

    -- * Request Lenses
    getResourcePolicies_maxResults,
    getResourcePolicies_nextToken,

    -- * Destructuring the Response
    GetResourcePoliciesResponse (..),
    newGetResourcePoliciesResponse,

    -- * Response Lenses
    getResourcePoliciesResponse_getResourcePoliciesResponseList,
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourcePolicies' smart constructor.
data GetResourcePolicies = GetResourcePolicies'
  { -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getResourcePolicies_maxResults' - The maximum size of a list to return.
--
-- 'nextToken', 'getResourcePolicies_nextToken' - A continuation token, if this is a continuation request.
newGetResourcePolicies ::
  GetResourcePolicies
newGetResourcePolicies =
  GetResourcePolicies'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum size of a list to return.
getResourcePolicies_maxResults :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Natural)
getResourcePolicies_maxResults = Lens.lens (\GetResourcePolicies' {maxResults} -> maxResults) (\s@GetResourcePolicies' {} a -> s {maxResults = a} :: GetResourcePolicies)

-- | A continuation token, if this is a continuation request.
getResourcePolicies_nextToken :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Text)
getResourcePolicies_nextToken = Lens.lens (\GetResourcePolicies' {nextToken} -> nextToken) (\s@GetResourcePolicies' {} a -> s {nextToken = a} :: GetResourcePolicies)

instance Core.AWSPager GetResourcePolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourcePoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourcePoliciesResponse_getResourcePoliciesResponseList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getResourcePolicies_nextToken
          Lens..~ rs
          Lens.^? getResourcePoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetResourcePolicies where
  type
    AWSResponse GetResourcePolicies =
      GetResourcePoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePoliciesResponse'
            Prelude.<$> ( x Data..?> "GetResourcePoliciesResponseList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePolicies where
  hashWithSalt _salt GetResourcePolicies' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetResourcePolicies where
  rnf GetResourcePolicies' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetResourcePolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetResourcePolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourcePolicies where
  toJSON GetResourcePolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetResourcePolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResourcePolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcePoliciesResponse' smart constructor.
data GetResourcePoliciesResponse = GetResourcePoliciesResponse'
  { -- | A list of the individual resource policies and the account-level
    -- resource policy.
    getResourcePoliciesResponseList :: Prelude.Maybe [GluePolicy],
    -- | A continuation token, if the returned list does not contain the last
    -- resource policy available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'getResourcePoliciesResponseList', 'getResourcePoliciesResponse_getResourcePoliciesResponseList' - A list of the individual resource policies and the account-level
-- resource policy.
--
-- 'nextToken', 'getResourcePoliciesResponse_nextToken' - A continuation token, if the returned list does not contain the last
-- resource policy available.
--
-- 'httpStatus', 'getResourcePoliciesResponse_httpStatus' - The response's http status code.
newGetResourcePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcePoliciesResponse
newGetResourcePoliciesResponse pHttpStatus_ =
  GetResourcePoliciesResponse'
    { getResourcePoliciesResponseList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the individual resource policies and the account-level
-- resource policy.
getResourcePoliciesResponse_getResourcePoliciesResponseList :: Lens.Lens' GetResourcePoliciesResponse (Prelude.Maybe [GluePolicy])
getResourcePoliciesResponse_getResourcePoliciesResponseList = Lens.lens (\GetResourcePoliciesResponse' {getResourcePoliciesResponseList} -> getResourcePoliciesResponseList) (\s@GetResourcePoliciesResponse' {} a -> s {getResourcePoliciesResponseList = a} :: GetResourcePoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if the returned list does not contain the last
-- resource policy available.
getResourcePoliciesResponse_nextToken :: Lens.Lens' GetResourcePoliciesResponse (Prelude.Maybe Prelude.Text)
getResourcePoliciesResponse_nextToken = Lens.lens (\GetResourcePoliciesResponse' {nextToken} -> nextToken) (\s@GetResourcePoliciesResponse' {} a -> s {nextToken = a} :: GetResourcePoliciesResponse)

-- | The response's http status code.
getResourcePoliciesResponse_httpStatus :: Lens.Lens' GetResourcePoliciesResponse Prelude.Int
getResourcePoliciesResponse_httpStatus = Lens.lens (\GetResourcePoliciesResponse' {httpStatus} -> httpStatus) (\s@GetResourcePoliciesResponse' {} a -> s {httpStatus = a} :: GetResourcePoliciesResponse)

instance Prelude.NFData GetResourcePoliciesResponse where
  rnf GetResourcePoliciesResponse' {..} =
    Prelude.rnf getResourcePoliciesResponseList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
