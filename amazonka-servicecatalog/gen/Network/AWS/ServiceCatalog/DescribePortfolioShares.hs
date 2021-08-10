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
-- Module      : Network.AWS.ServiceCatalog.DescribePortfolioShares
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a summary of each of the portfolio shares that were created for
-- the specified portfolio.
--
-- You can use this API to determine which accounts or organizational nodes
-- this portfolio have been shared, whether the recipient entity has
-- imported the share, and whether TagOptions are included with the share.
--
-- The @PortfolioId@ and @Type@ parameters are both required.
module Network.AWS.ServiceCatalog.DescribePortfolioShares
  ( -- * Creating a Request
    DescribePortfolioShares (..),
    newDescribePortfolioShares,

    -- * Request Lenses
    describePortfolioShares_pageSize,
    describePortfolioShares_pageToken,
    describePortfolioShares_portfolioId,
    describePortfolioShares_type,

    -- * Destructuring the Response
    DescribePortfolioSharesResponse (..),
    newDescribePortfolioSharesResponse,

    -- * Response Lenses
    describePortfolioSharesResponse_nextPageToken,
    describePortfolioSharesResponse_portfolioShareDetails,
    describePortfolioSharesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribePortfolioShares' smart constructor.
data DescribePortfolioShares = DescribePortfolioShares'
  { -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the portfolio for which shares will be
    -- retrieved.
    portfolioId :: Prelude.Text,
    -- | The type of portfolio share to summarize. This field acts as a filter on
    -- the type of portfolio share, which can be one of the following:
    --
    -- 1. @ACCOUNT@ - Represents an external account to account share.
    --
    -- 2. @ORGANIZATION@ - Represents a share to an organization. This share is
    -- available to every account in the organization.
    --
    -- 3. @ORGANIZATIONAL_UNIT@ - Represents a share to an organizational unit.
    --
    -- 4. @ORGANIZATION_MEMBER_ACCOUNT@ - Represents a share to an account in
    -- the organization.
    type' :: DescribePortfolioShareType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePortfolioShares' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'describePortfolioShares_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'describePortfolioShares_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'portfolioId', 'describePortfolioShares_portfolioId' - The unique identifier of the portfolio for which shares will be
-- retrieved.
--
-- 'type'', 'describePortfolioShares_type' - The type of portfolio share to summarize. This field acts as a filter on
-- the type of portfolio share, which can be one of the following:
--
-- 1. @ACCOUNT@ - Represents an external account to account share.
--
-- 2. @ORGANIZATION@ - Represents a share to an organization. This share is
-- available to every account in the organization.
--
-- 3. @ORGANIZATIONAL_UNIT@ - Represents a share to an organizational unit.
--
-- 4. @ORGANIZATION_MEMBER_ACCOUNT@ - Represents a share to an account in
-- the organization.
newDescribePortfolioShares ::
  -- | 'portfolioId'
  Prelude.Text ->
  -- | 'type''
  DescribePortfolioShareType ->
  DescribePortfolioShares
newDescribePortfolioShares pPortfolioId_ pType_ =
  DescribePortfolioShares'
    { pageSize =
        Prelude.Nothing,
      pageToken = Prelude.Nothing,
      portfolioId = pPortfolioId_,
      type' = pType_
    }

-- | The maximum number of items to return with this call.
describePortfolioShares_pageSize :: Lens.Lens' DescribePortfolioShares (Prelude.Maybe Prelude.Natural)
describePortfolioShares_pageSize = Lens.lens (\DescribePortfolioShares' {pageSize} -> pageSize) (\s@DescribePortfolioShares' {} a -> s {pageSize = a} :: DescribePortfolioShares)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
describePortfolioShares_pageToken :: Lens.Lens' DescribePortfolioShares (Prelude.Maybe Prelude.Text)
describePortfolioShares_pageToken = Lens.lens (\DescribePortfolioShares' {pageToken} -> pageToken) (\s@DescribePortfolioShares' {} a -> s {pageToken = a} :: DescribePortfolioShares)

-- | The unique identifier of the portfolio for which shares will be
-- retrieved.
describePortfolioShares_portfolioId :: Lens.Lens' DescribePortfolioShares Prelude.Text
describePortfolioShares_portfolioId = Lens.lens (\DescribePortfolioShares' {portfolioId} -> portfolioId) (\s@DescribePortfolioShares' {} a -> s {portfolioId = a} :: DescribePortfolioShares)

-- | The type of portfolio share to summarize. This field acts as a filter on
-- the type of portfolio share, which can be one of the following:
--
-- 1. @ACCOUNT@ - Represents an external account to account share.
--
-- 2. @ORGANIZATION@ - Represents a share to an organization. This share is
-- available to every account in the organization.
--
-- 3. @ORGANIZATIONAL_UNIT@ - Represents a share to an organizational unit.
--
-- 4. @ORGANIZATION_MEMBER_ACCOUNT@ - Represents a share to an account in
-- the organization.
describePortfolioShares_type :: Lens.Lens' DescribePortfolioShares DescribePortfolioShareType
describePortfolioShares_type = Lens.lens (\DescribePortfolioShares' {type'} -> type') (\s@DescribePortfolioShares' {} a -> s {type' = a} :: DescribePortfolioShares)

instance Core.AWSRequest DescribePortfolioShares where
  type
    AWSResponse DescribePortfolioShares =
      DescribePortfolioSharesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePortfolioSharesResponse'
            Prelude.<$> (x Core..?> "NextPageToken")
            Prelude.<*> ( x Core..?> "PortfolioShareDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePortfolioShares

instance Prelude.NFData DescribePortfolioShares

instance Core.ToHeaders DescribePortfolioShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribePortfolioShares" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribePortfolioShares where
  toJSON DescribePortfolioShares' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageSize" Core..=) Prelude.<$> pageSize,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            Prelude.Just ("PortfolioId" Core..= portfolioId),
            Prelude.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath DescribePortfolioShares where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePortfolioShares where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePortfolioSharesResponse' smart constructor.
data DescribePortfolioSharesResponse = DescribePortfolioSharesResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Summaries about each of the portfolio shares.
    portfolioShareDetails :: Prelude.Maybe [PortfolioShareDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePortfolioSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'describePortfolioSharesResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'portfolioShareDetails', 'describePortfolioSharesResponse_portfolioShareDetails' - Summaries about each of the portfolio shares.
--
-- 'httpStatus', 'describePortfolioSharesResponse_httpStatus' - The response's http status code.
newDescribePortfolioSharesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePortfolioSharesResponse
newDescribePortfolioSharesResponse pHttpStatus_ =
  DescribePortfolioSharesResponse'
    { nextPageToken =
        Prelude.Nothing,
      portfolioShareDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
describePortfolioSharesResponse_nextPageToken :: Lens.Lens' DescribePortfolioSharesResponse (Prelude.Maybe Prelude.Text)
describePortfolioSharesResponse_nextPageToken = Lens.lens (\DescribePortfolioSharesResponse' {nextPageToken} -> nextPageToken) (\s@DescribePortfolioSharesResponse' {} a -> s {nextPageToken = a} :: DescribePortfolioSharesResponse)

-- | Summaries about each of the portfolio shares.
describePortfolioSharesResponse_portfolioShareDetails :: Lens.Lens' DescribePortfolioSharesResponse (Prelude.Maybe [PortfolioShareDetail])
describePortfolioSharesResponse_portfolioShareDetails = Lens.lens (\DescribePortfolioSharesResponse' {portfolioShareDetails} -> portfolioShareDetails) (\s@DescribePortfolioSharesResponse' {} a -> s {portfolioShareDetails = a} :: DescribePortfolioSharesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePortfolioSharesResponse_httpStatus :: Lens.Lens' DescribePortfolioSharesResponse Prelude.Int
describePortfolioSharesResponse_httpStatus = Lens.lens (\DescribePortfolioSharesResponse' {httpStatus} -> httpStatus) (\s@DescribePortfolioSharesResponse' {} a -> s {httpStatus = a} :: DescribePortfolioSharesResponse)

instance
  Prelude.NFData
    DescribePortfolioSharesResponse
