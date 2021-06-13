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
-- Module      : Network.AWS.Glue.GetResourcePolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the security configurations for the resource policies set on
-- individual resources, and also the account-level policy.
--
-- This operation also returns the Data Catalog resource policy. However,
-- if you enabled metadata encryption in Data Catalog settings, and you do
-- not have permission on the AWS KMS key, the operation can\'t return the
-- Data Catalog resource policy.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetResourcePolicies
  ( -- * Creating a Request
    GetResourcePolicies (..),
    newGetResourcePolicies,

    -- * Request Lenses
    getResourcePolicies_nextToken,
    getResourcePolicies_maxResults,

    -- * Destructuring the Response
    GetResourcePoliciesResponse (..),
    newGetResourcePoliciesResponse,

    -- * Response Lenses
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_getResourcePoliciesResponseList,
    getResourcePoliciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetResourcePolicies' smart constructor.
data GetResourcePolicies = GetResourcePolicies'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'getResourcePolicies_nextToken' - A continuation token, if this is a continuation request.
--
-- 'maxResults', 'getResourcePolicies_maxResults' - The maximum size of a list to return.
newGetResourcePolicies ::
  GetResourcePolicies
newGetResourcePolicies =
  GetResourcePolicies'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation request.
getResourcePolicies_nextToken :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Text)
getResourcePolicies_nextToken = Lens.lens (\GetResourcePolicies' {nextToken} -> nextToken) (\s@GetResourcePolicies' {} a -> s {nextToken = a} :: GetResourcePolicies)

-- | The maximum size of a list to return.
getResourcePolicies_maxResults :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Natural)
getResourcePolicies_maxResults = Lens.lens (\GetResourcePolicies' {maxResults} -> maxResults) (\s@GetResourcePolicies' {} a -> s {maxResults = a} :: GetResourcePolicies)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePoliciesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "GetResourcePoliciesResponseList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePolicies

instance Prelude.NFData GetResourcePolicies

instance Core.ToHeaders GetResourcePolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetResourcePolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResourcePolicies where
  toJSON GetResourcePolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetResourcePolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery GetResourcePolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcePoliciesResponse' smart constructor.
data GetResourcePoliciesResponse = GetResourcePoliciesResponse'
  { -- | A continuation token, if the returned list does not contain the last
    -- resource policy available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the individual resource policies and the account-level
    -- resource policy.
    getResourcePoliciesResponseList :: Prelude.Maybe [GluePolicy],
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
-- 'nextToken', 'getResourcePoliciesResponse_nextToken' - A continuation token, if the returned list does not contain the last
-- resource policy available.
--
-- 'getResourcePoliciesResponseList', 'getResourcePoliciesResponse_getResourcePoliciesResponseList' - A list of the individual resource policies and the account-level
-- resource policy.
--
-- 'httpStatus', 'getResourcePoliciesResponse_httpStatus' - The response's http status code.
newGetResourcePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcePoliciesResponse
newGetResourcePoliciesResponse pHttpStatus_ =
  GetResourcePoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      getResourcePoliciesResponseList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the returned list does not contain the last
-- resource policy available.
getResourcePoliciesResponse_nextToken :: Lens.Lens' GetResourcePoliciesResponse (Prelude.Maybe Prelude.Text)
getResourcePoliciesResponse_nextToken = Lens.lens (\GetResourcePoliciesResponse' {nextToken} -> nextToken) (\s@GetResourcePoliciesResponse' {} a -> s {nextToken = a} :: GetResourcePoliciesResponse)

-- | A list of the individual resource policies and the account-level
-- resource policy.
getResourcePoliciesResponse_getResourcePoliciesResponseList :: Lens.Lens' GetResourcePoliciesResponse (Prelude.Maybe [GluePolicy])
getResourcePoliciesResponse_getResourcePoliciesResponseList = Lens.lens (\GetResourcePoliciesResponse' {getResourcePoliciesResponseList} -> getResourcePoliciesResponseList) (\s@GetResourcePoliciesResponse' {} a -> s {getResourcePoliciesResponseList = a} :: GetResourcePoliciesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getResourcePoliciesResponse_httpStatus :: Lens.Lens' GetResourcePoliciesResponse Prelude.Int
getResourcePoliciesResponse_httpStatus = Lens.lens (\GetResourcePoliciesResponse' {httpStatus} -> httpStatus) (\s@GetResourcePoliciesResponse' {} a -> s {httpStatus = a} :: GetResourcePoliciesResponse)

instance Prelude.NFData GetResourcePoliciesResponse
