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
-- Module      : Amazonka.OpenSearch.DescribeVpcEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Amazon OpenSearch Service-managed VPC endpoints.
module Amazonka.OpenSearch.DescribeVpcEndpoints
  ( -- * Creating a Request
    DescribeVpcEndpoints (..),
    newDescribeVpcEndpoints,

    -- * Request Lenses
    describeVpcEndpoints_vpcEndpointIds,

    -- * Destructuring the Response
    DescribeVpcEndpointsResponse (..),
    newDescribeVpcEndpointsResponse,

    -- * Response Lenses
    describeVpcEndpointsResponse_httpStatus,
    describeVpcEndpointsResponse_vpcEndpoints,
    describeVpcEndpointsResponse_vpcEndpointErrors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVpcEndpoints' smart constructor.
data DescribeVpcEndpoints = DescribeVpcEndpoints'
  { -- | The unique identifiers of the endpoints to get information about.
    vpcEndpointIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcEndpointIds', 'describeVpcEndpoints_vpcEndpointIds' - The unique identifiers of the endpoints to get information about.
newDescribeVpcEndpoints ::
  DescribeVpcEndpoints
newDescribeVpcEndpoints =
  DescribeVpcEndpoints'
    { vpcEndpointIds =
        Prelude.mempty
    }

-- | The unique identifiers of the endpoints to get information about.
describeVpcEndpoints_vpcEndpointIds :: Lens.Lens' DescribeVpcEndpoints [Prelude.Text]
describeVpcEndpoints_vpcEndpointIds = Lens.lens (\DescribeVpcEndpoints' {vpcEndpointIds} -> vpcEndpointIds) (\s@DescribeVpcEndpoints' {} a -> s {vpcEndpointIds = a} :: DescribeVpcEndpoints) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeVpcEndpoints where
  type
    AWSResponse DescribeVpcEndpoints =
      DescribeVpcEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVpcEndpointsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "VpcEndpoints" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "VpcEndpointErrors"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable DescribeVpcEndpoints where
  hashWithSalt _salt DescribeVpcEndpoints' {..} =
    _salt `Prelude.hashWithSalt` vpcEndpointIds

instance Prelude.NFData DescribeVpcEndpoints where
  rnf DescribeVpcEndpoints' {..} =
    Prelude.rnf vpcEndpointIds

instance Data.ToHeaders DescribeVpcEndpoints where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DescribeVpcEndpoints where
  toJSON DescribeVpcEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VpcEndpointIds" Data..= vpcEndpointIds)
          ]
      )

instance Data.ToPath DescribeVpcEndpoints where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/vpcEndpoints/describe"

instance Data.ToQuery DescribeVpcEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeVpcEndpointsResponse' smart constructor.
data DescribeVpcEndpointsResponse = DescribeVpcEndpointsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about each requested VPC endpoint.
    vpcEndpoints :: [VpcEndpoint],
    -- | Any errors associated with the request.
    vpcEndpointErrors :: [VpcEndpointError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeVpcEndpointsResponse_httpStatus' - The response's http status code.
--
-- 'vpcEndpoints', 'describeVpcEndpointsResponse_vpcEndpoints' - Information about each requested VPC endpoint.
--
-- 'vpcEndpointErrors', 'describeVpcEndpointsResponse_vpcEndpointErrors' - Any errors associated with the request.
newDescribeVpcEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVpcEndpointsResponse
newDescribeVpcEndpointsResponse pHttpStatus_ =
  DescribeVpcEndpointsResponse'
    { httpStatus =
        pHttpStatus_,
      vpcEndpoints = Prelude.mempty,
      vpcEndpointErrors = Prelude.mempty
    }

-- | The response's http status code.
describeVpcEndpointsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointsResponse Prelude.Int
describeVpcEndpointsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointsResponse)

-- | Information about each requested VPC endpoint.
describeVpcEndpointsResponse_vpcEndpoints :: Lens.Lens' DescribeVpcEndpointsResponse [VpcEndpoint]
describeVpcEndpointsResponse_vpcEndpoints = Lens.lens (\DescribeVpcEndpointsResponse' {vpcEndpoints} -> vpcEndpoints) (\s@DescribeVpcEndpointsResponse' {} a -> s {vpcEndpoints = a} :: DescribeVpcEndpointsResponse) Prelude.. Lens.coerced

-- | Any errors associated with the request.
describeVpcEndpointsResponse_vpcEndpointErrors :: Lens.Lens' DescribeVpcEndpointsResponse [VpcEndpointError]
describeVpcEndpointsResponse_vpcEndpointErrors = Lens.lens (\DescribeVpcEndpointsResponse' {vpcEndpointErrors} -> vpcEndpointErrors) (\s@DescribeVpcEndpointsResponse' {} a -> s {vpcEndpointErrors = a} :: DescribeVpcEndpointsResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeVpcEndpointsResponse where
  rnf DescribeVpcEndpointsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcEndpoints
      `Prelude.seq` Prelude.rnf vpcEndpointErrors
