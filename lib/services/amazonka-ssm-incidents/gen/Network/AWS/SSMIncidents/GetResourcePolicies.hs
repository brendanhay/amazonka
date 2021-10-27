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
-- Module      : Network.AWS.SSMIncidents.GetResourcePolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource policies attached to the specified response plan.
--
-- This operation returns paginated results.
module Network.AWS.SSMIncidents.GetResourcePolicies
  ( -- * Creating a Request
    GetResourcePolicies (..),
    newGetResourcePolicies,

    -- * Request Lenses
    getResourcePolicies_nextToken,
    getResourcePolicies_maxResults,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSMIncidents.Types

-- | /See:/ 'newGetResourcePolicies' smart constructor.
data GetResourcePolicies = GetResourcePolicies'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of resource policies to display per page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'getResourcePolicies_nextToken' - The pagination token to continue to the next page of results.
--
-- 'maxResults', 'getResourcePolicies_maxResults' - The maximum number of resource policies to display per page of results.
--
-- 'resourceArn', 'getResourcePolicies_resourceArn' - The Amazon Resource Name (ARN) of the response plan with the attached
-- resource policy.
newGetResourcePolicies ::
  -- | 'resourceArn'
  Prelude.Text ->
  GetResourcePolicies
newGetResourcePolicies pResourceArn_ =
  GetResourcePolicies'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The pagination token to continue to the next page of results.
getResourcePolicies_nextToken :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Text)
getResourcePolicies_nextToken = Lens.lens (\GetResourcePolicies' {nextToken} -> nextToken) (\s@GetResourcePolicies' {} a -> s {nextToken = a} :: GetResourcePolicies)

-- | The maximum number of resource policies to display per page of results.
getResourcePolicies_maxResults :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Natural)
getResourcePolicies_maxResults = Lens.lens (\GetResourcePolicies' {maxResults} -> maxResults) (\s@GetResourcePolicies' {} a -> s {maxResults = a} :: GetResourcePolicies)

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
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "resourcePolicies"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetResourcePolicies

instance Prelude.NFData GetResourcePolicies

instance Core.ToHeaders GetResourcePolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResourcePolicies where
  toJSON GetResourcePolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetResourcePolicies where
  toPath = Prelude.const "/getResourcePolicies"

instance Core.ToQuery GetResourcePolicies where
  toQuery GetResourcePolicies' {..} =
    Prelude.mconcat ["resourceArn" Core.=: resourceArn]

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

instance Prelude.NFData GetResourcePoliciesResponse
