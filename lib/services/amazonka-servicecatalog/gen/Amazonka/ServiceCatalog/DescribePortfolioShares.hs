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
-- Module      : Amazonka.ServiceCatalog.DescribePortfolioShares
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ServiceCatalog.DescribePortfolioShares
  ( -- * Creating a Request
    DescribePortfolioShares (..),
    newDescribePortfolioShares,

    -- * Request Lenses
    describePortfolioShares_pageToken,
    describePortfolioShares_pageSize,
    describePortfolioShares_portfolioId,
    describePortfolioShares_type,

    -- * Destructuring the Response
    DescribePortfolioSharesResponse (..),
    newDescribePortfolioSharesResponse,

    -- * Response Lenses
    describePortfolioSharesResponse_portfolioShareDetails,
    describePortfolioSharesResponse_nextPageToken,
    describePortfolioSharesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribePortfolioShares' smart constructor.
data DescribePortfolioShares = DescribePortfolioShares'
  { -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
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
-- 'pageToken', 'describePortfolioShares_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'pageSize', 'describePortfolioShares_pageSize' - The maximum number of items to return with this call.
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
    { pageToken =
        Prelude.Nothing,
      pageSize = Prelude.Nothing,
      portfolioId = pPortfolioId_,
      type' = pType_
    }

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
describePortfolioShares_pageToken :: Lens.Lens' DescribePortfolioShares (Prelude.Maybe Prelude.Text)
describePortfolioShares_pageToken = Lens.lens (\DescribePortfolioShares' {pageToken} -> pageToken) (\s@DescribePortfolioShares' {} a -> s {pageToken = a} :: DescribePortfolioShares)

-- | The maximum number of items to return with this call.
describePortfolioShares_pageSize :: Lens.Lens' DescribePortfolioShares (Prelude.Maybe Prelude.Natural)
describePortfolioShares_pageSize = Lens.lens (\DescribePortfolioShares' {pageSize} -> pageSize) (\s@DescribePortfolioShares' {} a -> s {pageSize = a} :: DescribePortfolioShares)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePortfolioSharesResponse'
            Prelude.<$> ( x Data..?> "PortfolioShareDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePortfolioShares where
  hashWithSalt _salt DescribePortfolioShares' {..} =
    _salt `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` portfolioId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DescribePortfolioShares where
  rnf DescribePortfolioShares' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf portfolioId
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders DescribePortfolioShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribePortfolioShares" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePortfolioShares where
  toJSON DescribePortfolioShares' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PageToken" Data..=) Prelude.<$> pageToken,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            Prelude.Just ("PortfolioId" Data..= portfolioId),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath DescribePortfolioShares where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePortfolioShares where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePortfolioSharesResponse' smart constructor.
data DescribePortfolioSharesResponse = DescribePortfolioSharesResponse'
  { -- | Summaries about each of the portfolio shares.
    portfolioShareDetails :: Prelude.Maybe [PortfolioShareDetail],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
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
-- 'portfolioShareDetails', 'describePortfolioSharesResponse_portfolioShareDetails' - Summaries about each of the portfolio shares.
--
-- 'nextPageToken', 'describePortfolioSharesResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'describePortfolioSharesResponse_httpStatus' - The response's http status code.
newDescribePortfolioSharesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePortfolioSharesResponse
newDescribePortfolioSharesResponse pHttpStatus_ =
  DescribePortfolioSharesResponse'
    { portfolioShareDetails =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summaries about each of the portfolio shares.
describePortfolioSharesResponse_portfolioShareDetails :: Lens.Lens' DescribePortfolioSharesResponse (Prelude.Maybe [PortfolioShareDetail])
describePortfolioSharesResponse_portfolioShareDetails = Lens.lens (\DescribePortfolioSharesResponse' {portfolioShareDetails} -> portfolioShareDetails) (\s@DescribePortfolioSharesResponse' {} a -> s {portfolioShareDetails = a} :: DescribePortfolioSharesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
describePortfolioSharesResponse_nextPageToken :: Lens.Lens' DescribePortfolioSharesResponse (Prelude.Maybe Prelude.Text)
describePortfolioSharesResponse_nextPageToken = Lens.lens (\DescribePortfolioSharesResponse' {nextPageToken} -> nextPageToken) (\s@DescribePortfolioSharesResponse' {} a -> s {nextPageToken = a} :: DescribePortfolioSharesResponse)

-- | The response's http status code.
describePortfolioSharesResponse_httpStatus :: Lens.Lens' DescribePortfolioSharesResponse Prelude.Int
describePortfolioSharesResponse_httpStatus = Lens.lens (\DescribePortfolioSharesResponse' {httpStatus} -> httpStatus) (\s@DescribePortfolioSharesResponse' {} a -> s {httpStatus = a} :: DescribePortfolioSharesResponse)

instance
  Prelude.NFData
    DescribePortfolioSharesResponse
  where
  rnf DescribePortfolioSharesResponse' {..} =
    Prelude.rnf portfolioShareDetails
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
