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
-- Module      : Amazonka.RAM.GetResourcePolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource policies for the specified resources that you own
-- and have shared.
--
-- This operation returns paginated results.
module Amazonka.RAM.GetResourcePolicies
  ( -- * Creating a Request
    GetResourcePolicies (..),
    newGetResourcePolicies,

    -- * Request Lenses
    getResourcePolicies_maxResults,
    getResourcePolicies_nextToken,
    getResourcePolicies_principal,
    getResourcePolicies_resourceArns,

    -- * Destructuring the Response
    GetResourcePoliciesResponse (..),
    newGetResourcePoliciesResponse,

    -- * Response Lenses
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_policies,
    getResourcePoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourcePolicies' smart constructor.
data GetResourcePolicies = GetResourcePolicies'
  { -- | Specifies the total number of results that you want included on each
    -- page of the response. If you do not include this parameter, it defaults
    -- to a value that is specific to the operation. If additional items exist
    -- beyond the number you specify, the @NextToken@ response element is
    -- returned with a value (not null). Include the specified value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results. Note that the service might return fewer
    -- results than the maximum even when there are more results available. You
    -- should check @NextToken@ after every operation to ensure that you
    -- receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the principal.
    principal :: Prelude.Maybe Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- of the resources whose policies you want to retrieve.
    resourceArns :: [Prelude.Text]
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
-- 'maxResults', 'getResourcePolicies_maxResults' - Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
--
-- 'nextToken', 'getResourcePolicies_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'principal', 'getResourcePolicies_principal' - Specifies the principal.
--
-- 'resourceArns', 'getResourcePolicies_resourceArns' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- of the resources whose policies you want to retrieve.
newGetResourcePolicies ::
  GetResourcePolicies
newGetResourcePolicies =
  GetResourcePolicies'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      principal = Prelude.Nothing,
      resourceArns = Prelude.mempty
    }

-- | Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
getResourcePolicies_maxResults :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Natural)
getResourcePolicies_maxResults = Lens.lens (\GetResourcePolicies' {maxResults} -> maxResults) (\s@GetResourcePolicies' {} a -> s {maxResults = a} :: GetResourcePolicies)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
getResourcePolicies_nextToken :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Text)
getResourcePolicies_nextToken = Lens.lens (\GetResourcePolicies' {nextToken} -> nextToken) (\s@GetResourcePolicies' {} a -> s {nextToken = a} :: GetResourcePolicies)

-- | Specifies the principal.
getResourcePolicies_principal :: Lens.Lens' GetResourcePolicies (Prelude.Maybe Prelude.Text)
getResourcePolicies_principal = Lens.lens (\GetResourcePolicies' {principal} -> principal) (\s@GetResourcePolicies' {} a -> s {principal = a} :: GetResourcePolicies)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- of the resources whose policies you want to retrieve.
getResourcePolicies_resourceArns :: Lens.Lens' GetResourcePolicies [Prelude.Text]
getResourcePolicies_resourceArns = Lens.lens (\GetResourcePolicies' {resourceArns} -> resourceArns) (\s@GetResourcePolicies' {} a -> s {resourceArns = a} :: GetResourcePolicies) Prelude.. Lens.coerced

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
            Lens.^? getResourcePoliciesResponse_policies
            Prelude.. Lens._Just
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
            Prelude.<*> (x Data..?> "policies" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePolicies where
  hashWithSalt _salt GetResourcePolicies' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` resourceArns

instance Prelude.NFData GetResourcePolicies where
  rnf GetResourcePolicies' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resourceArns

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
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("principal" Data..=) Prelude.<$> principal,
            Prelude.Just ("resourceArns" Data..= resourceArns)
          ]
      )

instance Data.ToPath GetResourcePolicies where
  toPath = Prelude.const "/getresourcepolicies"

instance Data.ToQuery GetResourcePolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcePoliciesResponse' smart constructor.
data GetResourcePoliciesResponse = GetResourcePoliciesResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of resource policy documents in JSON format.
    policies :: Prelude.Maybe [Prelude.Text],
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
-- 'nextToken', 'getResourcePoliciesResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'policies', 'getResourcePoliciesResponse_policies' - An array of resource policy documents in JSON format.
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
      policies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
getResourcePoliciesResponse_nextToken :: Lens.Lens' GetResourcePoliciesResponse (Prelude.Maybe Prelude.Text)
getResourcePoliciesResponse_nextToken = Lens.lens (\GetResourcePoliciesResponse' {nextToken} -> nextToken) (\s@GetResourcePoliciesResponse' {} a -> s {nextToken = a} :: GetResourcePoliciesResponse)

-- | An array of resource policy documents in JSON format.
getResourcePoliciesResponse_policies :: Lens.Lens' GetResourcePoliciesResponse (Prelude.Maybe [Prelude.Text])
getResourcePoliciesResponse_policies = Lens.lens (\GetResourcePoliciesResponse' {policies} -> policies) (\s@GetResourcePoliciesResponse' {} a -> s {policies = a} :: GetResourcePoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getResourcePoliciesResponse_httpStatus :: Lens.Lens' GetResourcePoliciesResponse Prelude.Int
getResourcePoliciesResponse_httpStatus = Lens.lens (\GetResourcePoliciesResponse' {httpStatus} -> httpStatus) (\s@GetResourcePoliciesResponse' {} a -> s {httpStatus = a} :: GetResourcePoliciesResponse)

instance Prelude.NFData GetResourcePoliciesResponse where
  rnf GetResourcePoliciesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf httpStatus
