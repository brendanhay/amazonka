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
-- Module      : Amazonka.ServiceCatalog.DescribePortfolioShareStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified portfolio share operation. This API can
-- only be called by the management account in the organization or by a
-- delegated admin.
module Amazonka.ServiceCatalog.DescribePortfolioShareStatus
  ( -- * Creating a Request
    DescribePortfolioShareStatus (..),
    newDescribePortfolioShareStatus,

    -- * Request Lenses
    describePortfolioShareStatus_portfolioShareToken,

    -- * Destructuring the Response
    DescribePortfolioShareStatusResponse (..),
    newDescribePortfolioShareStatusResponse,

    -- * Response Lenses
    describePortfolioShareStatusResponse_organizationNodeValue,
    describePortfolioShareStatusResponse_portfolioId,
    describePortfolioShareStatusResponse_portfolioShareToken,
    describePortfolioShareStatusResponse_shareDetails,
    describePortfolioShareStatusResponse_status,
    describePortfolioShareStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribePortfolioShareStatus' smart constructor.
data DescribePortfolioShareStatus = DescribePortfolioShareStatus'
  { -- | The token for the portfolio share operation. This token is returned
    -- either by CreatePortfolioShare or by DeletePortfolioShare.
    portfolioShareToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePortfolioShareStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioShareToken', 'describePortfolioShareStatus_portfolioShareToken' - The token for the portfolio share operation. This token is returned
-- either by CreatePortfolioShare or by DeletePortfolioShare.
newDescribePortfolioShareStatus ::
  -- | 'portfolioShareToken'
  Prelude.Text ->
  DescribePortfolioShareStatus
newDescribePortfolioShareStatus pPortfolioShareToken_ =
  DescribePortfolioShareStatus'
    { portfolioShareToken =
        pPortfolioShareToken_
    }

-- | The token for the portfolio share operation. This token is returned
-- either by CreatePortfolioShare or by DeletePortfolioShare.
describePortfolioShareStatus_portfolioShareToken :: Lens.Lens' DescribePortfolioShareStatus Prelude.Text
describePortfolioShareStatus_portfolioShareToken = Lens.lens (\DescribePortfolioShareStatus' {portfolioShareToken} -> portfolioShareToken) (\s@DescribePortfolioShareStatus' {} a -> s {portfolioShareToken = a} :: DescribePortfolioShareStatus)

instance Core.AWSRequest DescribePortfolioShareStatus where
  type
    AWSResponse DescribePortfolioShareStatus =
      DescribePortfolioShareStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePortfolioShareStatusResponse'
            Prelude.<$> (x Data..?> "OrganizationNodeValue")
            Prelude.<*> (x Data..?> "PortfolioId")
            Prelude.<*> (x Data..?> "PortfolioShareToken")
            Prelude.<*> (x Data..?> "ShareDetails")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePortfolioShareStatus
  where
  hashWithSalt _salt DescribePortfolioShareStatus' {..} =
    _salt `Prelude.hashWithSalt` portfolioShareToken

instance Prelude.NFData DescribePortfolioShareStatus where
  rnf DescribePortfolioShareStatus' {..} =
    Prelude.rnf portfolioShareToken

instance Data.ToHeaders DescribePortfolioShareStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribePortfolioShareStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePortfolioShareStatus where
  toJSON DescribePortfolioShareStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PortfolioShareToken" Data..= portfolioShareToken)
          ]
      )

instance Data.ToPath DescribePortfolioShareStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePortfolioShareStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePortfolioShareStatusResponse' smart constructor.
data DescribePortfolioShareStatusResponse = DescribePortfolioShareStatusResponse'
  { -- | Organization node identifier. It can be either account id,
    -- organizational unit id or organization id.
    organizationNodeValue :: Prelude.Maybe Prelude.Text,
    -- | The portfolio identifier.
    portfolioId :: Prelude.Maybe Prelude.Text,
    -- | The token for the portfolio share operation. For example,
    -- @share-6v24abcdefghi@.
    portfolioShareToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the portfolio share operation.
    shareDetails :: Prelude.Maybe ShareDetails,
    -- | Status of the portfolio share operation.
    status :: Prelude.Maybe ShareStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePortfolioShareStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationNodeValue', 'describePortfolioShareStatusResponse_organizationNodeValue' - Organization node identifier. It can be either account id,
-- organizational unit id or organization id.
--
-- 'portfolioId', 'describePortfolioShareStatusResponse_portfolioId' - The portfolio identifier.
--
-- 'portfolioShareToken', 'describePortfolioShareStatusResponse_portfolioShareToken' - The token for the portfolio share operation. For example,
-- @share-6v24abcdefghi@.
--
-- 'shareDetails', 'describePortfolioShareStatusResponse_shareDetails' - Information about the portfolio share operation.
--
-- 'status', 'describePortfolioShareStatusResponse_status' - Status of the portfolio share operation.
--
-- 'httpStatus', 'describePortfolioShareStatusResponse_httpStatus' - The response's http status code.
newDescribePortfolioShareStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePortfolioShareStatusResponse
newDescribePortfolioShareStatusResponse pHttpStatus_ =
  DescribePortfolioShareStatusResponse'
    { organizationNodeValue =
        Prelude.Nothing,
      portfolioId = Prelude.Nothing,
      portfolioShareToken = Prelude.Nothing,
      shareDetails = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Organization node identifier. It can be either account id,
-- organizational unit id or organization id.
describePortfolioShareStatusResponse_organizationNodeValue :: Lens.Lens' DescribePortfolioShareStatusResponse (Prelude.Maybe Prelude.Text)
describePortfolioShareStatusResponse_organizationNodeValue = Lens.lens (\DescribePortfolioShareStatusResponse' {organizationNodeValue} -> organizationNodeValue) (\s@DescribePortfolioShareStatusResponse' {} a -> s {organizationNodeValue = a} :: DescribePortfolioShareStatusResponse)

-- | The portfolio identifier.
describePortfolioShareStatusResponse_portfolioId :: Lens.Lens' DescribePortfolioShareStatusResponse (Prelude.Maybe Prelude.Text)
describePortfolioShareStatusResponse_portfolioId = Lens.lens (\DescribePortfolioShareStatusResponse' {portfolioId} -> portfolioId) (\s@DescribePortfolioShareStatusResponse' {} a -> s {portfolioId = a} :: DescribePortfolioShareStatusResponse)

-- | The token for the portfolio share operation. For example,
-- @share-6v24abcdefghi@.
describePortfolioShareStatusResponse_portfolioShareToken :: Lens.Lens' DescribePortfolioShareStatusResponse (Prelude.Maybe Prelude.Text)
describePortfolioShareStatusResponse_portfolioShareToken = Lens.lens (\DescribePortfolioShareStatusResponse' {portfolioShareToken} -> portfolioShareToken) (\s@DescribePortfolioShareStatusResponse' {} a -> s {portfolioShareToken = a} :: DescribePortfolioShareStatusResponse)

-- | Information about the portfolio share operation.
describePortfolioShareStatusResponse_shareDetails :: Lens.Lens' DescribePortfolioShareStatusResponse (Prelude.Maybe ShareDetails)
describePortfolioShareStatusResponse_shareDetails = Lens.lens (\DescribePortfolioShareStatusResponse' {shareDetails} -> shareDetails) (\s@DescribePortfolioShareStatusResponse' {} a -> s {shareDetails = a} :: DescribePortfolioShareStatusResponse)

-- | Status of the portfolio share operation.
describePortfolioShareStatusResponse_status :: Lens.Lens' DescribePortfolioShareStatusResponse (Prelude.Maybe ShareStatus)
describePortfolioShareStatusResponse_status = Lens.lens (\DescribePortfolioShareStatusResponse' {status} -> status) (\s@DescribePortfolioShareStatusResponse' {} a -> s {status = a} :: DescribePortfolioShareStatusResponse)

-- | The response's http status code.
describePortfolioShareStatusResponse_httpStatus :: Lens.Lens' DescribePortfolioShareStatusResponse Prelude.Int
describePortfolioShareStatusResponse_httpStatus = Lens.lens (\DescribePortfolioShareStatusResponse' {httpStatus} -> httpStatus) (\s@DescribePortfolioShareStatusResponse' {} a -> s {httpStatus = a} :: DescribePortfolioShareStatusResponse)

instance
  Prelude.NFData
    DescribePortfolioShareStatusResponse
  where
  rnf DescribePortfolioShareStatusResponse' {..} =
    Prelude.rnf organizationNodeValue `Prelude.seq`
      Prelude.rnf portfolioId `Prelude.seq`
        Prelude.rnf portfolioShareToken `Prelude.seq`
          Prelude.rnf shareDetails `Prelude.seq`
            Prelude.rnf status `Prelude.seq`
              Prelude.rnf httpStatus
