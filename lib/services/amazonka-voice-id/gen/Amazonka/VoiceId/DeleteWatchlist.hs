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
-- Module      : Amazonka.VoiceId.DeleteWatchlist
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified watchlist from Voice ID. This API throws an
-- exception when there are fraudsters in the watchlist that you are trying
-- to delete. You must delete the fraudsters, and then delete the
-- watchlist. Every domain has a default watchlist which cannot be deleted.
module Amazonka.VoiceId.DeleteWatchlist
  ( -- * Creating a Request
    DeleteWatchlist (..),
    newDeleteWatchlist,

    -- * Request Lenses
    deleteWatchlist_domainId,
    deleteWatchlist_watchlistId,

    -- * Destructuring the Response
    DeleteWatchlistResponse (..),
    newDeleteWatchlistResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newDeleteWatchlist' smart constructor.
data DeleteWatchlist = DeleteWatchlist'
  { -- | The identifier of the domain that contains the watchlist.
    domainId :: Prelude.Text,
    -- | The identifier of the watchlist to be deleted.
    watchlistId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWatchlist' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'deleteWatchlist_domainId' - The identifier of the domain that contains the watchlist.
--
-- 'watchlistId', 'deleteWatchlist_watchlistId' - The identifier of the watchlist to be deleted.
newDeleteWatchlist ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'watchlistId'
  Prelude.Text ->
  DeleteWatchlist
newDeleteWatchlist pDomainId_ pWatchlistId_ =
  DeleteWatchlist'
    { domainId = pDomainId_,
      watchlistId = pWatchlistId_
    }

-- | The identifier of the domain that contains the watchlist.
deleteWatchlist_domainId :: Lens.Lens' DeleteWatchlist Prelude.Text
deleteWatchlist_domainId = Lens.lens (\DeleteWatchlist' {domainId} -> domainId) (\s@DeleteWatchlist' {} a -> s {domainId = a} :: DeleteWatchlist)

-- | The identifier of the watchlist to be deleted.
deleteWatchlist_watchlistId :: Lens.Lens' DeleteWatchlist Prelude.Text
deleteWatchlist_watchlistId = Lens.lens (\DeleteWatchlist' {watchlistId} -> watchlistId) (\s@DeleteWatchlist' {} a -> s {watchlistId = a} :: DeleteWatchlist)

instance Core.AWSRequest DeleteWatchlist where
  type
    AWSResponse DeleteWatchlist =
      DeleteWatchlistResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteWatchlistResponse'

instance Prelude.Hashable DeleteWatchlist where
  hashWithSalt _salt DeleteWatchlist' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` watchlistId

instance Prelude.NFData DeleteWatchlist where
  rnf DeleteWatchlist' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf watchlistId

instance Data.ToHeaders DeleteWatchlist where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.DeleteWatchlist" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWatchlist where
  toJSON DeleteWatchlist' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("WatchlistId" Data..= watchlistId)
          ]
      )

instance Data.ToPath DeleteWatchlist where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWatchlist where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWatchlistResponse' smart constructor.
data DeleteWatchlistResponse = DeleteWatchlistResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWatchlistResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteWatchlistResponse ::
  DeleteWatchlistResponse
newDeleteWatchlistResponse = DeleteWatchlistResponse'

instance Prelude.NFData DeleteWatchlistResponse where
  rnf _ = ()
