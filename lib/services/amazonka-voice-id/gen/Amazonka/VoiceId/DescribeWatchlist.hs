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
-- Module      : Amazonka.VoiceId.DescribeWatchlist
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified watchlist.
module Amazonka.VoiceId.DescribeWatchlist
  ( -- * Creating a Request
    DescribeWatchlist (..),
    newDescribeWatchlist,

    -- * Request Lenses
    describeWatchlist_domainId,
    describeWatchlist_watchlistId,

    -- * Destructuring the Response
    DescribeWatchlistResponse (..),
    newDescribeWatchlistResponse,

    -- * Response Lenses
    describeWatchlistResponse_watchlist,
    describeWatchlistResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newDescribeWatchlist' smart constructor.
data DescribeWatchlist = DescribeWatchlist'
  { -- | The identifier of the domain that contains the watchlist.
    domainId :: Prelude.Text,
    -- | The identifier of the watchlist that you are describing.
    watchlistId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWatchlist' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'describeWatchlist_domainId' - The identifier of the domain that contains the watchlist.
--
-- 'watchlistId', 'describeWatchlist_watchlistId' - The identifier of the watchlist that you are describing.
newDescribeWatchlist ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'watchlistId'
  Prelude.Text ->
  DescribeWatchlist
newDescribeWatchlist pDomainId_ pWatchlistId_ =
  DescribeWatchlist'
    { domainId = pDomainId_,
      watchlistId = pWatchlistId_
    }

-- | The identifier of the domain that contains the watchlist.
describeWatchlist_domainId :: Lens.Lens' DescribeWatchlist Prelude.Text
describeWatchlist_domainId = Lens.lens (\DescribeWatchlist' {domainId} -> domainId) (\s@DescribeWatchlist' {} a -> s {domainId = a} :: DescribeWatchlist)

-- | The identifier of the watchlist that you are describing.
describeWatchlist_watchlistId :: Lens.Lens' DescribeWatchlist Prelude.Text
describeWatchlist_watchlistId = Lens.lens (\DescribeWatchlist' {watchlistId} -> watchlistId) (\s@DescribeWatchlist' {} a -> s {watchlistId = a} :: DescribeWatchlist)

instance Core.AWSRequest DescribeWatchlist where
  type
    AWSResponse DescribeWatchlist =
      DescribeWatchlistResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWatchlistResponse'
            Prelude.<$> (x Data..?> "Watchlist")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWatchlist where
  hashWithSalt _salt DescribeWatchlist' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` watchlistId

instance Prelude.NFData DescribeWatchlist where
  rnf DescribeWatchlist' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf watchlistId

instance Data.ToHeaders DescribeWatchlist where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.DescribeWatchlist" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeWatchlist where
  toJSON DescribeWatchlist' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("WatchlistId" Data..= watchlistId)
          ]
      )

instance Data.ToPath DescribeWatchlist where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeWatchlist where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWatchlistResponse' smart constructor.
data DescribeWatchlistResponse = DescribeWatchlistResponse'
  { -- | Information about the specified watchlist.
    watchlist :: Prelude.Maybe Watchlist,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWatchlistResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'watchlist', 'describeWatchlistResponse_watchlist' - Information about the specified watchlist.
--
-- 'httpStatus', 'describeWatchlistResponse_httpStatus' - The response's http status code.
newDescribeWatchlistResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWatchlistResponse
newDescribeWatchlistResponse pHttpStatus_ =
  DescribeWatchlistResponse'
    { watchlist =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified watchlist.
describeWatchlistResponse_watchlist :: Lens.Lens' DescribeWatchlistResponse (Prelude.Maybe Watchlist)
describeWatchlistResponse_watchlist = Lens.lens (\DescribeWatchlistResponse' {watchlist} -> watchlist) (\s@DescribeWatchlistResponse' {} a -> s {watchlist = a} :: DescribeWatchlistResponse)

-- | The response's http status code.
describeWatchlistResponse_httpStatus :: Lens.Lens' DescribeWatchlistResponse Prelude.Int
describeWatchlistResponse_httpStatus = Lens.lens (\DescribeWatchlistResponse' {httpStatus} -> httpStatus) (\s@DescribeWatchlistResponse' {} a -> s {httpStatus = a} :: DescribeWatchlistResponse)

instance Prelude.NFData DescribeWatchlistResponse where
  rnf DescribeWatchlistResponse' {..} =
    Prelude.rnf watchlist
      `Prelude.seq` Prelude.rnf httpStatus
