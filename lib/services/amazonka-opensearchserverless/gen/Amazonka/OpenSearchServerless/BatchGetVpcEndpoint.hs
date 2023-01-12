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
-- Module      : Amazonka.OpenSearchServerless.BatchGetVpcEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns attributes for one or more VPC endpoints associated with the
-- current account. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-vpc.html Access Amazon OpenSearch Serverless using an interface endpoint>.
module Amazonka.OpenSearchServerless.BatchGetVpcEndpoint
  ( -- * Creating a Request
    BatchGetVpcEndpoint (..),
    newBatchGetVpcEndpoint,

    -- * Request Lenses
    batchGetVpcEndpoint_ids,

    -- * Destructuring the Response
    BatchGetVpcEndpointResponse (..),
    newBatchGetVpcEndpointResponse,

    -- * Response Lenses
    batchGetVpcEndpointResponse_vpcEndpointDetails,
    batchGetVpcEndpointResponse_vpcEndpointErrorDetails,
    batchGetVpcEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetVpcEndpoint' smart constructor.
data BatchGetVpcEndpoint = BatchGetVpcEndpoint'
  { -- | A list of VPC endpoint identifiers.
    ids :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetVpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'batchGetVpcEndpoint_ids' - A list of VPC endpoint identifiers.
newBatchGetVpcEndpoint ::
  -- | 'ids'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetVpcEndpoint
newBatchGetVpcEndpoint pIds_ =
  BatchGetVpcEndpoint'
    { ids =
        Lens.coerced Lens.# pIds_
    }

-- | A list of VPC endpoint identifiers.
batchGetVpcEndpoint_ids :: Lens.Lens' BatchGetVpcEndpoint (Prelude.NonEmpty Prelude.Text)
batchGetVpcEndpoint_ids = Lens.lens (\BatchGetVpcEndpoint' {ids} -> ids) (\s@BatchGetVpcEndpoint' {} a -> s {ids = a} :: BatchGetVpcEndpoint) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetVpcEndpoint where
  type
    AWSResponse BatchGetVpcEndpoint =
      BatchGetVpcEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetVpcEndpointResponse'
            Prelude.<$> ( x Data..?> "vpcEndpointDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "vpcEndpointErrorDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetVpcEndpoint where
  hashWithSalt _salt BatchGetVpcEndpoint' {..} =
    _salt `Prelude.hashWithSalt` ids

instance Prelude.NFData BatchGetVpcEndpoint where
  rnf BatchGetVpcEndpoint' {..} = Prelude.rnf ids

instance Data.ToHeaders BatchGetVpcEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.BatchGetVpcEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetVpcEndpoint where
  toJSON BatchGetVpcEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ids" Data..= ids)]
      )

instance Data.ToPath BatchGetVpcEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetVpcEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetVpcEndpointResponse' smart constructor.
data BatchGetVpcEndpointResponse = BatchGetVpcEndpointResponse'
  { -- | Details about the specified VPC endpoint.
    vpcEndpointDetails :: Prelude.Maybe [VpcEndpointDetail],
    -- | Error information for a failed request.
    vpcEndpointErrorDetails :: Prelude.Maybe [VpcEndpointErrorDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetVpcEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcEndpointDetails', 'batchGetVpcEndpointResponse_vpcEndpointDetails' - Details about the specified VPC endpoint.
--
-- 'vpcEndpointErrorDetails', 'batchGetVpcEndpointResponse_vpcEndpointErrorDetails' - Error information for a failed request.
--
-- 'httpStatus', 'batchGetVpcEndpointResponse_httpStatus' - The response's http status code.
newBatchGetVpcEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetVpcEndpointResponse
newBatchGetVpcEndpointResponse pHttpStatus_ =
  BatchGetVpcEndpointResponse'
    { vpcEndpointDetails =
        Prelude.Nothing,
      vpcEndpointErrorDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the specified VPC endpoint.
batchGetVpcEndpointResponse_vpcEndpointDetails :: Lens.Lens' BatchGetVpcEndpointResponse (Prelude.Maybe [VpcEndpointDetail])
batchGetVpcEndpointResponse_vpcEndpointDetails = Lens.lens (\BatchGetVpcEndpointResponse' {vpcEndpointDetails} -> vpcEndpointDetails) (\s@BatchGetVpcEndpointResponse' {} a -> s {vpcEndpointDetails = a} :: BatchGetVpcEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | Error information for a failed request.
batchGetVpcEndpointResponse_vpcEndpointErrorDetails :: Lens.Lens' BatchGetVpcEndpointResponse (Prelude.Maybe [VpcEndpointErrorDetail])
batchGetVpcEndpointResponse_vpcEndpointErrorDetails = Lens.lens (\BatchGetVpcEndpointResponse' {vpcEndpointErrorDetails} -> vpcEndpointErrorDetails) (\s@BatchGetVpcEndpointResponse' {} a -> s {vpcEndpointErrorDetails = a} :: BatchGetVpcEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetVpcEndpointResponse_httpStatus :: Lens.Lens' BatchGetVpcEndpointResponse Prelude.Int
batchGetVpcEndpointResponse_httpStatus = Lens.lens (\BatchGetVpcEndpointResponse' {httpStatus} -> httpStatus) (\s@BatchGetVpcEndpointResponse' {} a -> s {httpStatus = a} :: BatchGetVpcEndpointResponse)

instance Prelude.NFData BatchGetVpcEndpointResponse where
  rnf BatchGetVpcEndpointResponse' {..} =
    Prelude.rnf vpcEndpointDetails
      `Prelude.seq` Prelude.rnf vpcEndpointErrorDetails
      `Prelude.seq` Prelude.rnf httpStatus
