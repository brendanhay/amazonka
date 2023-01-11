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
-- Module      : Amazonka.DirectConnect.DescribeCustomerMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get and view a list of customer agreements, along with their signed
-- status and whether the customer is an NNIPartner, NNIPartnerV2, or a
-- nonPartner.
module Amazonka.DirectConnect.DescribeCustomerMetadata
  ( -- * Creating a Request
    DescribeCustomerMetadata (..),
    newDescribeCustomerMetadata,

    -- * Destructuring the Response
    DescribeCustomerMetadataResponse (..),
    newDescribeCustomerMetadataResponse,

    -- * Response Lenses
    describeCustomerMetadataResponse_agreements,
    describeCustomerMetadataResponse_nniPartnerType,
    describeCustomerMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomerMetadata' smart constructor.
data DescribeCustomerMetadata = DescribeCustomerMetadata'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomerMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeCustomerMetadata ::
  DescribeCustomerMetadata
newDescribeCustomerMetadata =
  DescribeCustomerMetadata'

instance Core.AWSRequest DescribeCustomerMetadata where
  type
    AWSResponse DescribeCustomerMetadata =
      DescribeCustomerMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomerMetadataResponse'
            Prelude.<$> (x Data..?> "agreements" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nniPartnerType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCustomerMetadata where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeCustomerMetadata where
  rnf _ = ()

instance Data.ToHeaders DescribeCustomerMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DescribeCustomerMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCustomerMetadata where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeCustomerMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCustomerMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomerMetadataResponse' smart constructor.
data DescribeCustomerMetadataResponse = DescribeCustomerMetadataResponse'
  { -- | The list of customer agreements.
    agreements :: Prelude.Maybe [CustomerAgreement],
    -- | The type of network-to-network interface (NNI) partner. The partner type
    -- will be one of the following:
    --
    -- -   V1: This partner can only allocate 50Mbps, 100Mbps, 200Mbps,
    --     300Mbps, 400Mbps, or 500Mbps subgigabit connections.
    --
    -- -   V2: This partner can only allocate 1GB, 2GB, 5GB, or 10GB hosted
    --     connections.
    --
    -- -   nonPartner: The customer is not a partner.
    nniPartnerType :: Prelude.Maybe NniPartnerType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomerMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agreements', 'describeCustomerMetadataResponse_agreements' - The list of customer agreements.
--
-- 'nniPartnerType', 'describeCustomerMetadataResponse_nniPartnerType' - The type of network-to-network interface (NNI) partner. The partner type
-- will be one of the following:
--
-- -   V1: This partner can only allocate 50Mbps, 100Mbps, 200Mbps,
--     300Mbps, 400Mbps, or 500Mbps subgigabit connections.
--
-- -   V2: This partner can only allocate 1GB, 2GB, 5GB, or 10GB hosted
--     connections.
--
-- -   nonPartner: The customer is not a partner.
--
-- 'httpStatus', 'describeCustomerMetadataResponse_httpStatus' - The response's http status code.
newDescribeCustomerMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomerMetadataResponse
newDescribeCustomerMetadataResponse pHttpStatus_ =
  DescribeCustomerMetadataResponse'
    { agreements =
        Prelude.Nothing,
      nniPartnerType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of customer agreements.
describeCustomerMetadataResponse_agreements :: Lens.Lens' DescribeCustomerMetadataResponse (Prelude.Maybe [CustomerAgreement])
describeCustomerMetadataResponse_agreements = Lens.lens (\DescribeCustomerMetadataResponse' {agreements} -> agreements) (\s@DescribeCustomerMetadataResponse' {} a -> s {agreements = a} :: DescribeCustomerMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The type of network-to-network interface (NNI) partner. The partner type
-- will be one of the following:
--
-- -   V1: This partner can only allocate 50Mbps, 100Mbps, 200Mbps,
--     300Mbps, 400Mbps, or 500Mbps subgigabit connections.
--
-- -   V2: This partner can only allocate 1GB, 2GB, 5GB, or 10GB hosted
--     connections.
--
-- -   nonPartner: The customer is not a partner.
describeCustomerMetadataResponse_nniPartnerType :: Lens.Lens' DescribeCustomerMetadataResponse (Prelude.Maybe NniPartnerType)
describeCustomerMetadataResponse_nniPartnerType = Lens.lens (\DescribeCustomerMetadataResponse' {nniPartnerType} -> nniPartnerType) (\s@DescribeCustomerMetadataResponse' {} a -> s {nniPartnerType = a} :: DescribeCustomerMetadataResponse)

-- | The response's http status code.
describeCustomerMetadataResponse_httpStatus :: Lens.Lens' DescribeCustomerMetadataResponse Prelude.Int
describeCustomerMetadataResponse_httpStatus = Lens.lens (\DescribeCustomerMetadataResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomerMetadataResponse' {} a -> s {httpStatus = a} :: DescribeCustomerMetadataResponse)

instance
  Prelude.NFData
    DescribeCustomerMetadataResponse
  where
  rnf DescribeCustomerMetadataResponse' {..} =
    Prelude.rnf agreements
      `Prelude.seq` Prelude.rnf nniPartnerType
      `Prelude.seq` Prelude.rnf httpStatus
