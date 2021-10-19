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
-- Module      : Network.AWS.ServiceCatalog.DescribePortfolioShareStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified portfolio share operation. This API can
-- only be called by the management account in the organization or by a
-- delegated admin.
module Network.AWS.ServiceCatalog.DescribePortfolioShareStatus
  ( -- * Creating a Request
    DescribePortfolioShareStatus (..),
    newDescribePortfolioShareStatus,

    -- * Request Lenses
    describePortfolioShareStatus_portfolioShareToken,

    -- * Destructuring the Response
    DescribePortfolioShareStatusResponse (..),
    newDescribePortfolioShareStatusResponse,

    -- * Response Lenses
    describePortfolioShareStatusResponse_status,
    describePortfolioShareStatusResponse_portfolioShareToken,
    describePortfolioShareStatusResponse_shareDetails,
    describePortfolioShareStatusResponse_portfolioId,
    describePortfolioShareStatusResponse_organizationNodeValue,
    describePortfolioShareStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePortfolioShareStatusResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "PortfolioShareToken")
            Prelude.<*> (x Core..?> "ShareDetails")
            Prelude.<*> (x Core..?> "PortfolioId")
            Prelude.<*> (x Core..?> "OrganizationNodeValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePortfolioShareStatus

instance Prelude.NFData DescribePortfolioShareStatus

instance Core.ToHeaders DescribePortfolioShareStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribePortfolioShareStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribePortfolioShareStatus where
  toJSON DescribePortfolioShareStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PortfolioShareToken" Core..= portfolioShareToken)
          ]
      )

instance Core.ToPath DescribePortfolioShareStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePortfolioShareStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePortfolioShareStatusResponse' smart constructor.
data DescribePortfolioShareStatusResponse = DescribePortfolioShareStatusResponse'
  { -- | Status of the portfolio share operation.
    status :: Prelude.Maybe ShareStatus,
    -- | The token for the portfolio share operation. For example,
    -- @share-6v24abcdefghi@.
    portfolioShareToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the portfolio share operation.
    shareDetails :: Prelude.Maybe ShareDetails,
    -- | The portfolio identifier.
    portfolioId :: Prelude.Maybe Prelude.Text,
    -- | Organization node identifier. It can be either account id,
    -- organizational unit id or organization id.
    organizationNodeValue :: Prelude.Maybe Prelude.Text,
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
-- 'status', 'describePortfolioShareStatusResponse_status' - Status of the portfolio share operation.
--
-- 'portfolioShareToken', 'describePortfolioShareStatusResponse_portfolioShareToken' - The token for the portfolio share operation. For example,
-- @share-6v24abcdefghi@.
--
-- 'shareDetails', 'describePortfolioShareStatusResponse_shareDetails' - Information about the portfolio share operation.
--
-- 'portfolioId', 'describePortfolioShareStatusResponse_portfolioId' - The portfolio identifier.
--
-- 'organizationNodeValue', 'describePortfolioShareStatusResponse_organizationNodeValue' - Organization node identifier. It can be either account id,
-- organizational unit id or organization id.
--
-- 'httpStatus', 'describePortfolioShareStatusResponse_httpStatus' - The response's http status code.
newDescribePortfolioShareStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePortfolioShareStatusResponse
newDescribePortfolioShareStatusResponse pHttpStatus_ =
  DescribePortfolioShareStatusResponse'
    { status =
        Prelude.Nothing,
      portfolioShareToken = Prelude.Nothing,
      shareDetails = Prelude.Nothing,
      portfolioId = Prelude.Nothing,
      organizationNodeValue =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Status of the portfolio share operation.
describePortfolioShareStatusResponse_status :: Lens.Lens' DescribePortfolioShareStatusResponse (Prelude.Maybe ShareStatus)
describePortfolioShareStatusResponse_status = Lens.lens (\DescribePortfolioShareStatusResponse' {status} -> status) (\s@DescribePortfolioShareStatusResponse' {} a -> s {status = a} :: DescribePortfolioShareStatusResponse)

-- | The token for the portfolio share operation. For example,
-- @share-6v24abcdefghi@.
describePortfolioShareStatusResponse_portfolioShareToken :: Lens.Lens' DescribePortfolioShareStatusResponse (Prelude.Maybe Prelude.Text)
describePortfolioShareStatusResponse_portfolioShareToken = Lens.lens (\DescribePortfolioShareStatusResponse' {portfolioShareToken} -> portfolioShareToken) (\s@DescribePortfolioShareStatusResponse' {} a -> s {portfolioShareToken = a} :: DescribePortfolioShareStatusResponse)

-- | Information about the portfolio share operation.
describePortfolioShareStatusResponse_shareDetails :: Lens.Lens' DescribePortfolioShareStatusResponse (Prelude.Maybe ShareDetails)
describePortfolioShareStatusResponse_shareDetails = Lens.lens (\DescribePortfolioShareStatusResponse' {shareDetails} -> shareDetails) (\s@DescribePortfolioShareStatusResponse' {} a -> s {shareDetails = a} :: DescribePortfolioShareStatusResponse)

-- | The portfolio identifier.
describePortfolioShareStatusResponse_portfolioId :: Lens.Lens' DescribePortfolioShareStatusResponse (Prelude.Maybe Prelude.Text)
describePortfolioShareStatusResponse_portfolioId = Lens.lens (\DescribePortfolioShareStatusResponse' {portfolioId} -> portfolioId) (\s@DescribePortfolioShareStatusResponse' {} a -> s {portfolioId = a} :: DescribePortfolioShareStatusResponse)

-- | Organization node identifier. It can be either account id,
-- organizational unit id or organization id.
describePortfolioShareStatusResponse_organizationNodeValue :: Lens.Lens' DescribePortfolioShareStatusResponse (Prelude.Maybe Prelude.Text)
describePortfolioShareStatusResponse_organizationNodeValue = Lens.lens (\DescribePortfolioShareStatusResponse' {organizationNodeValue} -> organizationNodeValue) (\s@DescribePortfolioShareStatusResponse' {} a -> s {organizationNodeValue = a} :: DescribePortfolioShareStatusResponse)

-- | The response's http status code.
describePortfolioShareStatusResponse_httpStatus :: Lens.Lens' DescribePortfolioShareStatusResponse Prelude.Int
describePortfolioShareStatusResponse_httpStatus = Lens.lens (\DescribePortfolioShareStatusResponse' {httpStatus} -> httpStatus) (\s@DescribePortfolioShareStatusResponse' {} a -> s {httpStatus = a} :: DescribePortfolioShareStatusResponse)

instance
  Prelude.NFData
    DescribePortfolioShareStatusResponse
