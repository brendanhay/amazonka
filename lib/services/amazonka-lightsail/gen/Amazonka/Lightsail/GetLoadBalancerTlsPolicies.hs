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
-- Module      : Amazonka.Lightsail.GetLoadBalancerTlsPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of TLS security policies that you can apply to Lightsail
-- load balancers.
--
-- For more information about load balancer TLS security policies, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configure-load-balancer-tls-security-policy Configuring TLS security policies on your Amazon Lightsail load balancers>
-- in the /Amazon Lightsail Developer Guide/.
module Amazonka.Lightsail.GetLoadBalancerTlsPolicies
  ( -- * Creating a Request
    GetLoadBalancerTlsPolicies (..),
    newGetLoadBalancerTlsPolicies,

    -- * Request Lenses
    getLoadBalancerTlsPolicies_pageToken,

    -- * Destructuring the Response
    GetLoadBalancerTlsPoliciesResponse (..),
    newGetLoadBalancerTlsPoliciesResponse,

    -- * Response Lenses
    getLoadBalancerTlsPoliciesResponse_nextPageToken,
    getLoadBalancerTlsPoliciesResponse_tlsPolicies,
    getLoadBalancerTlsPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLoadBalancerTlsPolicies' smart constructor.
data GetLoadBalancerTlsPolicies = GetLoadBalancerTlsPolicies'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetLoadBalancerTlsPolicies@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoadBalancerTlsPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getLoadBalancerTlsPolicies_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetLoadBalancerTlsPolicies@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
newGetLoadBalancerTlsPolicies ::
  GetLoadBalancerTlsPolicies
newGetLoadBalancerTlsPolicies =
  GetLoadBalancerTlsPolicies'
    { pageToken =
        Prelude.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetLoadBalancerTlsPolicies@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getLoadBalancerTlsPolicies_pageToken :: Lens.Lens' GetLoadBalancerTlsPolicies (Prelude.Maybe Prelude.Text)
getLoadBalancerTlsPolicies_pageToken = Lens.lens (\GetLoadBalancerTlsPolicies' {pageToken} -> pageToken) (\s@GetLoadBalancerTlsPolicies' {} a -> s {pageToken = a} :: GetLoadBalancerTlsPolicies)

instance Core.AWSRequest GetLoadBalancerTlsPolicies where
  type
    AWSResponse GetLoadBalancerTlsPolicies =
      GetLoadBalancerTlsPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoadBalancerTlsPoliciesResponse'
            Prelude.<$> (x Data..?> "nextPageToken")
            Prelude.<*> (x Data..?> "tlsPolicies" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoadBalancerTlsPolicies where
  hashWithSalt _salt GetLoadBalancerTlsPolicies' {..} =
    _salt `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetLoadBalancerTlsPolicies where
  rnf GetLoadBalancerTlsPolicies' {..} =
    Prelude.rnf pageToken

instance Data.ToHeaders GetLoadBalancerTlsPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetLoadBalancerTlsPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLoadBalancerTlsPolicies where
  toJSON GetLoadBalancerTlsPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [("pageToken" Data..=) Prelude.<$> pageToken]
      )

instance Data.ToPath GetLoadBalancerTlsPolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLoadBalancerTlsPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoadBalancerTlsPoliciesResponse' smart constructor.
data GetLoadBalancerTlsPoliciesResponse = GetLoadBalancerTlsPoliciesResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetLoadBalancerTlsPolicies@ request and specify the next page token
    -- using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the TLS security policies that are
    -- available.
    tlsPolicies :: Prelude.Maybe [LoadBalancerTlsPolicy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoadBalancerTlsPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getLoadBalancerTlsPoliciesResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetLoadBalancerTlsPolicies@ request and specify the next page token
-- using the @pageToken@ parameter.
--
-- 'tlsPolicies', 'getLoadBalancerTlsPoliciesResponse_tlsPolicies' - An array of objects that describe the TLS security policies that are
-- available.
--
-- 'httpStatus', 'getLoadBalancerTlsPoliciesResponse_httpStatus' - The response's http status code.
newGetLoadBalancerTlsPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLoadBalancerTlsPoliciesResponse
newGetLoadBalancerTlsPoliciesResponse pHttpStatus_ =
  GetLoadBalancerTlsPoliciesResponse'
    { nextPageToken =
        Prelude.Nothing,
      tlsPolicies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetLoadBalancerTlsPolicies@ request and specify the next page token
-- using the @pageToken@ parameter.
getLoadBalancerTlsPoliciesResponse_nextPageToken :: Lens.Lens' GetLoadBalancerTlsPoliciesResponse (Prelude.Maybe Prelude.Text)
getLoadBalancerTlsPoliciesResponse_nextPageToken = Lens.lens (\GetLoadBalancerTlsPoliciesResponse' {nextPageToken} -> nextPageToken) (\s@GetLoadBalancerTlsPoliciesResponse' {} a -> s {nextPageToken = a} :: GetLoadBalancerTlsPoliciesResponse)

-- | An array of objects that describe the TLS security policies that are
-- available.
getLoadBalancerTlsPoliciesResponse_tlsPolicies :: Lens.Lens' GetLoadBalancerTlsPoliciesResponse (Prelude.Maybe [LoadBalancerTlsPolicy])
getLoadBalancerTlsPoliciesResponse_tlsPolicies = Lens.lens (\GetLoadBalancerTlsPoliciesResponse' {tlsPolicies} -> tlsPolicies) (\s@GetLoadBalancerTlsPoliciesResponse' {} a -> s {tlsPolicies = a} :: GetLoadBalancerTlsPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLoadBalancerTlsPoliciesResponse_httpStatus :: Lens.Lens' GetLoadBalancerTlsPoliciesResponse Prelude.Int
getLoadBalancerTlsPoliciesResponse_httpStatus = Lens.lens (\GetLoadBalancerTlsPoliciesResponse' {httpStatus} -> httpStatus) (\s@GetLoadBalancerTlsPoliciesResponse' {} a -> s {httpStatus = a} :: GetLoadBalancerTlsPoliciesResponse)

instance
  Prelude.NFData
    GetLoadBalancerTlsPoliciesResponse
  where
  rnf GetLoadBalancerTlsPoliciesResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf tlsPolicies
      `Prelude.seq` Prelude.rnf httpStatus
