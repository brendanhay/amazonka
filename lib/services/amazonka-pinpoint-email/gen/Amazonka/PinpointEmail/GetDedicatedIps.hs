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
-- Module      : Amazonka.PinpointEmail.GetDedicatedIps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the dedicated IP addresses that are associated with your Amazon
-- Pinpoint account.
--
-- This operation returns paginated results.
module Amazonka.PinpointEmail.GetDedicatedIps
  ( -- * Creating a Request
    GetDedicatedIps (..),
    newGetDedicatedIps,

    -- * Request Lenses
    getDedicatedIps_nextToken,
    getDedicatedIps_pageSize,
    getDedicatedIps_poolName,

    -- * Destructuring the Response
    GetDedicatedIpsResponse (..),
    newGetDedicatedIpsResponse,

    -- * Response Lenses
    getDedicatedIpsResponse_dedicatedIps,
    getDedicatedIpsResponse_nextToken,
    getDedicatedIpsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to obtain more information about dedicated IP pools.
--
-- /See:/ 'newGetDedicatedIps' smart constructor.
data GetDedicatedIps = GetDedicatedIps'
  { -- | A token returned from a previous call to @GetDedicatedIps@ to indicate
    -- the position of the dedicated IP pool in the list of IP pools.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of results to show in a single call to
    -- @GetDedicatedIpsRequest@. If the number of results is larger than the
    -- number you specified in this parameter, then the response includes a
    -- @NextToken@ element, which you can use to obtain additional results.
    pageSize :: Prelude.Maybe Prelude.Int,
    -- | The name of the IP pool that the dedicated IP address is associated
    -- with.
    poolName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDedicatedIps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDedicatedIps_nextToken' - A token returned from a previous call to @GetDedicatedIps@ to indicate
-- the position of the dedicated IP pool in the list of IP pools.
--
-- 'pageSize', 'getDedicatedIps_pageSize' - The number of results to show in a single call to
-- @GetDedicatedIpsRequest@. If the number of results is larger than the
-- number you specified in this parameter, then the response includes a
-- @NextToken@ element, which you can use to obtain additional results.
--
-- 'poolName', 'getDedicatedIps_poolName' - The name of the IP pool that the dedicated IP address is associated
-- with.
newGetDedicatedIps ::
  GetDedicatedIps
newGetDedicatedIps =
  GetDedicatedIps'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      poolName = Prelude.Nothing
    }

-- | A token returned from a previous call to @GetDedicatedIps@ to indicate
-- the position of the dedicated IP pool in the list of IP pools.
getDedicatedIps_nextToken :: Lens.Lens' GetDedicatedIps (Prelude.Maybe Prelude.Text)
getDedicatedIps_nextToken = Lens.lens (\GetDedicatedIps' {nextToken} -> nextToken) (\s@GetDedicatedIps' {} a -> s {nextToken = a} :: GetDedicatedIps)

-- | The number of results to show in a single call to
-- @GetDedicatedIpsRequest@. If the number of results is larger than the
-- number you specified in this parameter, then the response includes a
-- @NextToken@ element, which you can use to obtain additional results.
getDedicatedIps_pageSize :: Lens.Lens' GetDedicatedIps (Prelude.Maybe Prelude.Int)
getDedicatedIps_pageSize = Lens.lens (\GetDedicatedIps' {pageSize} -> pageSize) (\s@GetDedicatedIps' {} a -> s {pageSize = a} :: GetDedicatedIps)

-- | The name of the IP pool that the dedicated IP address is associated
-- with.
getDedicatedIps_poolName :: Lens.Lens' GetDedicatedIps (Prelude.Maybe Prelude.Text)
getDedicatedIps_poolName = Lens.lens (\GetDedicatedIps' {poolName} -> poolName) (\s@GetDedicatedIps' {} a -> s {poolName = a} :: GetDedicatedIps)

instance Core.AWSPager GetDedicatedIps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDedicatedIpsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDedicatedIpsResponse_dedicatedIps
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getDedicatedIps_nextToken
          Lens..~ rs
          Lens.^? getDedicatedIpsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetDedicatedIps where
  type
    AWSResponse GetDedicatedIps =
      GetDedicatedIpsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDedicatedIpsResponse'
            Prelude.<$> (x Data..?> "DedicatedIps" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDedicatedIps where
  hashWithSalt _salt GetDedicatedIps' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` poolName

instance Prelude.NFData GetDedicatedIps where
  rnf GetDedicatedIps' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf poolName

instance Data.ToHeaders GetDedicatedIps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDedicatedIps where
  toPath = Prelude.const "/v1/email/dedicated-ips"

instance Data.ToQuery GetDedicatedIps where
  toQuery GetDedicatedIps' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "PageSize" Data.=: pageSize,
        "PoolName" Data.=: poolName
      ]

-- | Information about the dedicated IP addresses that are associated with
-- your Amazon Pinpoint account.
--
-- /See:/ 'newGetDedicatedIpsResponse' smart constructor.
data GetDedicatedIpsResponse = GetDedicatedIpsResponse'
  { -- | A list of dedicated IP addresses that are reserved for use by your
    -- Amazon Pinpoint account.
    dedicatedIps :: Prelude.Maybe [DedicatedIp],
    -- | A token that indicates that there are additional dedicated IP addresses
    -- to list. To view additional addresses, issue another request to
    -- @GetDedicatedIps@, passing this token in the @NextToken@ parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDedicatedIpsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedIps', 'getDedicatedIpsResponse_dedicatedIps' - A list of dedicated IP addresses that are reserved for use by your
-- Amazon Pinpoint account.
--
-- 'nextToken', 'getDedicatedIpsResponse_nextToken' - A token that indicates that there are additional dedicated IP addresses
-- to list. To view additional addresses, issue another request to
-- @GetDedicatedIps@, passing this token in the @NextToken@ parameter.
--
-- 'httpStatus', 'getDedicatedIpsResponse_httpStatus' - The response's http status code.
newGetDedicatedIpsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDedicatedIpsResponse
newGetDedicatedIpsResponse pHttpStatus_ =
  GetDedicatedIpsResponse'
    { dedicatedIps =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of dedicated IP addresses that are reserved for use by your
-- Amazon Pinpoint account.
getDedicatedIpsResponse_dedicatedIps :: Lens.Lens' GetDedicatedIpsResponse (Prelude.Maybe [DedicatedIp])
getDedicatedIpsResponse_dedicatedIps = Lens.lens (\GetDedicatedIpsResponse' {dedicatedIps} -> dedicatedIps) (\s@GetDedicatedIpsResponse' {} a -> s {dedicatedIps = a} :: GetDedicatedIpsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates that there are additional dedicated IP addresses
-- to list. To view additional addresses, issue another request to
-- @GetDedicatedIps@, passing this token in the @NextToken@ parameter.
getDedicatedIpsResponse_nextToken :: Lens.Lens' GetDedicatedIpsResponse (Prelude.Maybe Prelude.Text)
getDedicatedIpsResponse_nextToken = Lens.lens (\GetDedicatedIpsResponse' {nextToken} -> nextToken) (\s@GetDedicatedIpsResponse' {} a -> s {nextToken = a} :: GetDedicatedIpsResponse)

-- | The response's http status code.
getDedicatedIpsResponse_httpStatus :: Lens.Lens' GetDedicatedIpsResponse Prelude.Int
getDedicatedIpsResponse_httpStatus = Lens.lens (\GetDedicatedIpsResponse' {httpStatus} -> httpStatus) (\s@GetDedicatedIpsResponse' {} a -> s {httpStatus = a} :: GetDedicatedIpsResponse)

instance Prelude.NFData GetDedicatedIpsResponse where
  rnf GetDedicatedIpsResponse' {..} =
    Prelude.rnf dedicatedIps
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
