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
-- Module      : Amazonka.VoiceId.UpdateWatchlist
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified watchlist. Every domain has a default watchlist
-- which cannot be updated.
module Amazonka.VoiceId.UpdateWatchlist
  ( -- * Creating a Request
    UpdateWatchlist (..),
    newUpdateWatchlist,

    -- * Request Lenses
    updateWatchlist_description,
    updateWatchlist_name,
    updateWatchlist_domainId,
    updateWatchlist_watchlistId,

    -- * Destructuring the Response
    UpdateWatchlistResponse (..),
    newUpdateWatchlistResponse,

    -- * Response Lenses
    updateWatchlistResponse_watchlist,
    updateWatchlistResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newUpdateWatchlist' smart constructor.
data UpdateWatchlist = UpdateWatchlist'
  { -- | A brief description about this watchlist.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the watchlist.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The identifier of the domain that contains the watchlist.
    domainId :: Prelude.Text,
    -- | The identifier of the watchlist to be updated.
    watchlistId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWatchlist' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateWatchlist_description' - A brief description about this watchlist.
--
-- 'name', 'updateWatchlist_name' - The name of the watchlist.
--
-- 'domainId', 'updateWatchlist_domainId' - The identifier of the domain that contains the watchlist.
--
-- 'watchlistId', 'updateWatchlist_watchlistId' - The identifier of the watchlist to be updated.
newUpdateWatchlist ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'watchlistId'
  Prelude.Text ->
  UpdateWatchlist
newUpdateWatchlist pDomainId_ pWatchlistId_ =
  UpdateWatchlist'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      domainId = pDomainId_,
      watchlistId = pWatchlistId_
    }

-- | A brief description about this watchlist.
updateWatchlist_description :: Lens.Lens' UpdateWatchlist (Prelude.Maybe Prelude.Text)
updateWatchlist_description = Lens.lens (\UpdateWatchlist' {description} -> description) (\s@UpdateWatchlist' {} a -> s {description = a} :: UpdateWatchlist) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the watchlist.
updateWatchlist_name :: Lens.Lens' UpdateWatchlist (Prelude.Maybe Prelude.Text)
updateWatchlist_name = Lens.lens (\UpdateWatchlist' {name} -> name) (\s@UpdateWatchlist' {} a -> s {name = a} :: UpdateWatchlist) Prelude.. Lens.mapping Data._Sensitive

-- | The identifier of the domain that contains the watchlist.
updateWatchlist_domainId :: Lens.Lens' UpdateWatchlist Prelude.Text
updateWatchlist_domainId = Lens.lens (\UpdateWatchlist' {domainId} -> domainId) (\s@UpdateWatchlist' {} a -> s {domainId = a} :: UpdateWatchlist)

-- | The identifier of the watchlist to be updated.
updateWatchlist_watchlistId :: Lens.Lens' UpdateWatchlist Prelude.Text
updateWatchlist_watchlistId = Lens.lens (\UpdateWatchlist' {watchlistId} -> watchlistId) (\s@UpdateWatchlist' {} a -> s {watchlistId = a} :: UpdateWatchlist)

instance Core.AWSRequest UpdateWatchlist where
  type
    AWSResponse UpdateWatchlist =
      UpdateWatchlistResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWatchlistResponse'
            Prelude.<$> (x Data..?> "Watchlist")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWatchlist where
  hashWithSalt _salt UpdateWatchlist' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` watchlistId

instance Prelude.NFData UpdateWatchlist where
  rnf UpdateWatchlist' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf watchlistId

instance Data.ToHeaders UpdateWatchlist where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.UpdateWatchlist" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWatchlist where
  toJSON UpdateWatchlist' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("WatchlistId" Data..= watchlistId)
          ]
      )

instance Data.ToPath UpdateWatchlist where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateWatchlist where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWatchlistResponse' smart constructor.
data UpdateWatchlistResponse = UpdateWatchlistResponse'
  { -- | Details about the updated watchlist.
    watchlist :: Prelude.Maybe Watchlist,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWatchlistResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'watchlist', 'updateWatchlistResponse_watchlist' - Details about the updated watchlist.
--
-- 'httpStatus', 'updateWatchlistResponse_httpStatus' - The response's http status code.
newUpdateWatchlistResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWatchlistResponse
newUpdateWatchlistResponse pHttpStatus_ =
  UpdateWatchlistResponse'
    { watchlist =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the updated watchlist.
updateWatchlistResponse_watchlist :: Lens.Lens' UpdateWatchlistResponse (Prelude.Maybe Watchlist)
updateWatchlistResponse_watchlist = Lens.lens (\UpdateWatchlistResponse' {watchlist} -> watchlist) (\s@UpdateWatchlistResponse' {} a -> s {watchlist = a} :: UpdateWatchlistResponse)

-- | The response's http status code.
updateWatchlistResponse_httpStatus :: Lens.Lens' UpdateWatchlistResponse Prelude.Int
updateWatchlistResponse_httpStatus = Lens.lens (\UpdateWatchlistResponse' {httpStatus} -> httpStatus) (\s@UpdateWatchlistResponse' {} a -> s {httpStatus = a} :: UpdateWatchlistResponse)

instance Prelude.NFData UpdateWatchlistResponse where
  rnf UpdateWatchlistResponse' {..} =
    Prelude.rnf watchlist
      `Prelude.seq` Prelude.rnf httpStatus
