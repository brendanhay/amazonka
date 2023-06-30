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
-- Module      : Amazonka.SSMIncidents.GetResourcePolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource policies attached to the specified response plan.
--
-- This operation returns paginated results.
module Amazonka.SSMIncidents.GetResourcePolicies
  ( -- * Creating a Request
    GetResourcePolicies (..),
    newGetResourcePolicies,

    -- * Request Lenses
    getResourcePolicies_maxResults,
    getResourcePolicies_nextToken,
    getResourcePolicies_resourceArn,

    -- * Destructuring the Response
    GetResourcePoliciesResponse (..),
    newGetResourcePoliciesResponse,

    -- * Response Lenses
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_httpStatus,
    getResourcePoliciesResponse_resourcePolicies,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newGetResourcePolicies' smart constructor.
data GetResourcePolicies = GetResourcePolicies'
  { -- | The maximum number of resource policies to display for each page of
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the response plan with the attached
    -- resource policy.
    resourceArn :: Prelude.Text
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
-- 'maxResults', 'getResourcePolicies_maxResults' - The maximum number of resource policies to display for each page of
-- results.
--
-- 'nextToken', 'getResourcePolicies_nextToken' - The pagination token to continue to the next page of results.
--
-- 'resourceArn', 'getResourcePolicies_resourceArn' - The Amazon Resource Name (ARN) of the response plan with the attached
-- resource policy.
newGetResourcePolicies ::
  -- | 'resourceArn'
  Prelude.Text ->
  GetResourcePolicies
newGetResourcePolicies pResourceArn_ =
  GetResourcePolicies'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The maximum number of resource policies to display for each page of
-- results.
getResourcePolicies_maxResults :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Natural)
getResourcePolicies_maxResults = Lens.lens (\GetResourcePolicies' {maxResults} -> maxResults) (\s@GetResourcePolicies' {} a -> s {maxResults = a} :: GetResourcePolicies)

-- | The pagination token to continue to the next page of results.
getResourcePolicies_nextToken :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Text)
getResourcePolicies_nextToken = Lens.lens (\GetResourcePolicies' {nextToken} -> nextToken) (\s@GetResourcePolicies' {} a -> s {nextToken = a} :: GetResourcePolicies)

-- | The Amazon Resource Name (ARN) of the response plan with the attached
-- resource policy.
getResourcePolicies_resourceArn :: Lens.Lens' GetResourcePolicies Prelude.Text
getResourcePolicies_resourceArn = Lens.lens (\GetResourcePolicies' {resourceArn} -> resourceArn) (\s@GetResourcePolicies' {} a -> s {resourceArn = a} :: GetResourcePolicies)

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
            Lens.^. getResourcePoliciesResponse_resourcePolicies
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "resourcePolicies"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetResourcePolicies where
  hashWithSalt _salt GetResourcePolicies' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData GetResourcePolicies where
  rnf GetResourcePolicies' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders GetResourcePolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourcePolicies where
  toJSON GetResourcePolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetResourcePolicies where
  toPath = Prelude.const "/getResourcePolicies"

instance Data.ToQuery GetResourcePolicies where
  toQuery GetResourcePolicies' {..} =
    Prelude.mconcat ["resourceArn" Data.=: resourceArn]

-- | /See:/ 'newGetResourcePoliciesResponse' smart constructor.
data GetResourcePoliciesResponse = GetResourcePoliciesResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Details about the resource policy attached to the response plan.
    resourcePolicies :: [ResourcePolicy]
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
-- 'nextToken', 'getResourcePoliciesResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'httpStatus', 'getResourcePoliciesResponse_httpStatus' - The response's http status code.
--
-- 'resourcePolicies', 'getResourcePoliciesResponse_resourcePolicies' - Details about the resource policy attached to the response plan.
newGetResourcePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcePoliciesResponse
newGetResourcePoliciesResponse pHttpStatus_ =
  GetResourcePoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      resourcePolicies = Prelude.mempty
    }

-- | The pagination token to continue to the next page of results.
getResourcePoliciesResponse_nextToken :: Lens.Lens' GetResourcePoliciesResponse (Prelude.Maybe Prelude.Text)
getResourcePoliciesResponse_nextToken = Lens.lens (\GetResourcePoliciesResponse' {nextToken} -> nextToken) (\s@GetResourcePoliciesResponse' {} a -> s {nextToken = a} :: GetResourcePoliciesResponse)

-- | The response's http status code.
getResourcePoliciesResponse_httpStatus :: Lens.Lens' GetResourcePoliciesResponse Prelude.Int
getResourcePoliciesResponse_httpStatus = Lens.lens (\GetResourcePoliciesResponse' {httpStatus} -> httpStatus) (\s@GetResourcePoliciesResponse' {} a -> s {httpStatus = a} :: GetResourcePoliciesResponse)

-- | Details about the resource policy attached to the response plan.
getResourcePoliciesResponse_resourcePolicies :: Lens.Lens' GetResourcePoliciesResponse [ResourcePolicy]
getResourcePoliciesResponse_resourcePolicies = Lens.lens (\GetResourcePoliciesResponse' {resourcePolicies} -> resourcePolicies) (\s@GetResourcePoliciesResponse' {} a -> s {resourcePolicies = a} :: GetResourcePoliciesResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetResourcePoliciesResponse where
  rnf GetResourcePoliciesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourcePolicies
