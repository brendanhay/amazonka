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
    describePortfolioShareStatusResponse_shareDetails,
    describePortfolioShareStatusResponse_status,
    describePortfolioShareStatusResponse_portfolioId,
    describePortfolioShareStatusResponse_portfolioShareToken,
    describePortfolioShareStatusResponse_organizationNodeValue,
    describePortfolioShareStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribePortfolioShareStatus' smart constructor.
data DescribePortfolioShareStatus = DescribePortfolioShareStatus'
  { -- | The token for the portfolio share operation. This token is returned
    -- either by CreatePortfolioShare or by DeletePortfolioShare.
    portfolioShareToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribePortfolioShareStatus
newDescribePortfolioShareStatus pPortfolioShareToken_ =
  DescribePortfolioShareStatus'
    { portfolioShareToken =
        pPortfolioShareToken_
    }

-- | The token for the portfolio share operation. This token is returned
-- either by CreatePortfolioShare or by DeletePortfolioShare.
describePortfolioShareStatus_portfolioShareToken :: Lens.Lens' DescribePortfolioShareStatus Core.Text
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
            Core.<$> (x Core..?> "ShareDetails")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "PortfolioId")
            Core.<*> (x Core..?> "PortfolioShareToken")
            Core.<*> (x Core..?> "OrganizationNodeValue")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePortfolioShareStatus

instance Core.NFData DescribePortfolioShareStatus

instance Core.ToHeaders DescribePortfolioShareStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribePortfolioShareStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribePortfolioShareStatus where
  toJSON DescribePortfolioShareStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("PortfolioShareToken" Core..= portfolioShareToken)
          ]
      )

instance Core.ToPath DescribePortfolioShareStatus where
  toPath = Core.const "/"

instance Core.ToQuery DescribePortfolioShareStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePortfolioShareStatusResponse' smart constructor.
data DescribePortfolioShareStatusResponse = DescribePortfolioShareStatusResponse'
  { -- | Information about the portfolio share operation.
    shareDetails :: Core.Maybe ShareDetails,
    -- | Status of the portfolio share operation.
    status :: Core.Maybe ShareStatus,
    -- | The portfolio identifier.
    portfolioId :: Core.Maybe Core.Text,
    -- | The token for the portfolio share operation. For example,
    -- @share-6v24abcdefghi@.
    portfolioShareToken :: Core.Maybe Core.Text,
    -- | Organization node identifier. It can be either account id,
    -- organizational unit id or organization id.
    organizationNodeValue :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePortfolioShareStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareDetails', 'describePortfolioShareStatusResponse_shareDetails' - Information about the portfolio share operation.
--
-- 'status', 'describePortfolioShareStatusResponse_status' - Status of the portfolio share operation.
--
-- 'portfolioId', 'describePortfolioShareStatusResponse_portfolioId' - The portfolio identifier.
--
-- 'portfolioShareToken', 'describePortfolioShareStatusResponse_portfolioShareToken' - The token for the portfolio share operation. For example,
-- @share-6v24abcdefghi@.
--
-- 'organizationNodeValue', 'describePortfolioShareStatusResponse_organizationNodeValue' - Organization node identifier. It can be either account id,
-- organizational unit id or organization id.
--
-- 'httpStatus', 'describePortfolioShareStatusResponse_httpStatus' - The response's http status code.
newDescribePortfolioShareStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePortfolioShareStatusResponse
newDescribePortfolioShareStatusResponse pHttpStatus_ =
  DescribePortfolioShareStatusResponse'
    { shareDetails =
        Core.Nothing,
      status = Core.Nothing,
      portfolioId = Core.Nothing,
      portfolioShareToken = Core.Nothing,
      organizationNodeValue = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the portfolio share operation.
describePortfolioShareStatusResponse_shareDetails :: Lens.Lens' DescribePortfolioShareStatusResponse (Core.Maybe ShareDetails)
describePortfolioShareStatusResponse_shareDetails = Lens.lens (\DescribePortfolioShareStatusResponse' {shareDetails} -> shareDetails) (\s@DescribePortfolioShareStatusResponse' {} a -> s {shareDetails = a} :: DescribePortfolioShareStatusResponse)

-- | Status of the portfolio share operation.
describePortfolioShareStatusResponse_status :: Lens.Lens' DescribePortfolioShareStatusResponse (Core.Maybe ShareStatus)
describePortfolioShareStatusResponse_status = Lens.lens (\DescribePortfolioShareStatusResponse' {status} -> status) (\s@DescribePortfolioShareStatusResponse' {} a -> s {status = a} :: DescribePortfolioShareStatusResponse)

-- | The portfolio identifier.
describePortfolioShareStatusResponse_portfolioId :: Lens.Lens' DescribePortfolioShareStatusResponse (Core.Maybe Core.Text)
describePortfolioShareStatusResponse_portfolioId = Lens.lens (\DescribePortfolioShareStatusResponse' {portfolioId} -> portfolioId) (\s@DescribePortfolioShareStatusResponse' {} a -> s {portfolioId = a} :: DescribePortfolioShareStatusResponse)

-- | The token for the portfolio share operation. For example,
-- @share-6v24abcdefghi@.
describePortfolioShareStatusResponse_portfolioShareToken :: Lens.Lens' DescribePortfolioShareStatusResponse (Core.Maybe Core.Text)
describePortfolioShareStatusResponse_portfolioShareToken = Lens.lens (\DescribePortfolioShareStatusResponse' {portfolioShareToken} -> portfolioShareToken) (\s@DescribePortfolioShareStatusResponse' {} a -> s {portfolioShareToken = a} :: DescribePortfolioShareStatusResponse)

-- | Organization node identifier. It can be either account id,
-- organizational unit id or organization id.
describePortfolioShareStatusResponse_organizationNodeValue :: Lens.Lens' DescribePortfolioShareStatusResponse (Core.Maybe Core.Text)
describePortfolioShareStatusResponse_organizationNodeValue = Lens.lens (\DescribePortfolioShareStatusResponse' {organizationNodeValue} -> organizationNodeValue) (\s@DescribePortfolioShareStatusResponse' {} a -> s {organizationNodeValue = a} :: DescribePortfolioShareStatusResponse)

-- | The response's http status code.
describePortfolioShareStatusResponse_httpStatus :: Lens.Lens' DescribePortfolioShareStatusResponse Core.Int
describePortfolioShareStatusResponse_httpStatus = Lens.lens (\DescribePortfolioShareStatusResponse' {httpStatus} -> httpStatus) (\s@DescribePortfolioShareStatusResponse' {} a -> s {httpStatus = a} :: DescribePortfolioShareStatusResponse)

instance
  Core.NFData
    DescribePortfolioShareStatusResponse
