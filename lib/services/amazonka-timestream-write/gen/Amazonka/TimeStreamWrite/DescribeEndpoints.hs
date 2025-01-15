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
-- Module      : Amazonka.TimeStreamWrite.DescribeEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- DescribeEndpoints returns a list of available endpoints to make
-- Timestream API calls against. This API is available through both Write
-- and Query.
--
-- Because the Timestream SDKs are designed to transparently work with the
-- service’s architecture, including the management and mapping of the
-- service endpoints, /it is not recommended that you use this API unless/:
--
-- -   You are using
--     <https://docs.aws.amazon.com/timestream/latest/developerguide/VPCEndpoints VPC endpoints (Amazon Web Services PrivateLink) with Timestream>
--
-- -   Your application uses a programming language that does not yet have
--     SDK support
--
-- -   You require better control over the client-side implementation
--
-- For detailed information on how and when to use and implement
-- DescribeEndpoints, see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/Using.API.html#Using-API.endpoint-discovery The Endpoint Discovery Pattern>.
module Amazonka.TimeStreamWrite.DescribeEndpoints
  ( -- * Creating a Request
    DescribeEndpoints (..),
    newDescribeEndpoints,

    -- * Destructuring the Response
    DescribeEndpointsResponse (..),
    newDescribeEndpointsResponse,

    -- * Response Lenses
    describeEndpointsResponse_httpStatus,
    describeEndpointsResponse_endpoints,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newDescribeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeEndpoints ::
  DescribeEndpoints
newDescribeEndpoints = DescribeEndpoints'

instance Core.AWSRequest DescribeEndpoints where
  type
    AWSResponse DescribeEndpoints =
      DescribeEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Endpoints" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeEndpoints where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeEndpoints where
  rnf _ = ()

instance Data.ToHeaders DescribeEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.DescribeEndpoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEndpoints where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An @Endpoints@ object is returned when a @DescribeEndpoints@ request is
    -- made.
    endpoints :: [Endpoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeEndpointsResponse_httpStatus' - The response's http status code.
--
-- 'endpoints', 'describeEndpointsResponse_endpoints' - An @Endpoints@ object is returned when a @DescribeEndpoints@ request is
-- made.
newDescribeEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointsResponse
newDescribeEndpointsResponse pHttpStatus_ =
  DescribeEndpointsResponse'
    { httpStatus =
        pHttpStatus_,
      endpoints = Prelude.mempty
    }

-- | The response's http status code.
describeEndpointsResponse_httpStatus :: Lens.Lens' DescribeEndpointsResponse Prelude.Int
describeEndpointsResponse_httpStatus = Lens.lens (\DescribeEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeEndpointsResponse)

-- | An @Endpoints@ object is returned when a @DescribeEndpoints@ request is
-- made.
describeEndpointsResponse_endpoints :: Lens.Lens' DescribeEndpointsResponse [Endpoint]
describeEndpointsResponse_endpoints = Lens.lens (\DescribeEndpointsResponse' {endpoints} -> endpoints) (\s@DescribeEndpointsResponse' {} a -> s {endpoints = a} :: DescribeEndpointsResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeEndpointsResponse where
  rnf DescribeEndpointsResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf endpoints
