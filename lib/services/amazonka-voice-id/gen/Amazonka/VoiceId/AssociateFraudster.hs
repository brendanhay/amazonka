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
-- Module      : Amazonka.VoiceId.AssociateFraudster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the fraudsters with the watchlist specified in the same
-- domain.
module Amazonka.VoiceId.AssociateFraudster
  ( -- * Creating a Request
    AssociateFraudster (..),
    newAssociateFraudster,

    -- * Request Lenses
    associateFraudster_domainId,
    associateFraudster_fraudsterId,
    associateFraudster_watchlistId,

    -- * Destructuring the Response
    AssociateFraudsterResponse (..),
    newAssociateFraudsterResponse,

    -- * Response Lenses
    associateFraudsterResponse_fraudster,
    associateFraudsterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newAssociateFraudster' smart constructor.
data AssociateFraudster = AssociateFraudster'
  { -- | The identifier of the domain that contains the fraudster.
    domainId :: Prelude.Text,
    -- | The identifier of the fraudster to be associated with the watchlist.
    fraudsterId :: Data.Sensitive Prelude.Text,
    -- | The identifier of the watchlist you want to associate with the
    -- fraudster.
    watchlistId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFraudster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'associateFraudster_domainId' - The identifier of the domain that contains the fraudster.
--
-- 'fraudsterId', 'associateFraudster_fraudsterId' - The identifier of the fraudster to be associated with the watchlist.
--
-- 'watchlistId', 'associateFraudster_watchlistId' - The identifier of the watchlist you want to associate with the
-- fraudster.
newAssociateFraudster ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'fraudsterId'
  Prelude.Text ->
  -- | 'watchlistId'
  Prelude.Text ->
  AssociateFraudster
newAssociateFraudster
  pDomainId_
  pFraudsterId_
  pWatchlistId_ =
    AssociateFraudster'
      { domainId = pDomainId_,
        fraudsterId = Data._Sensitive Lens.# pFraudsterId_,
        watchlistId = pWatchlistId_
      }

-- | The identifier of the domain that contains the fraudster.
associateFraudster_domainId :: Lens.Lens' AssociateFraudster Prelude.Text
associateFraudster_domainId = Lens.lens (\AssociateFraudster' {domainId} -> domainId) (\s@AssociateFraudster' {} a -> s {domainId = a} :: AssociateFraudster)

-- | The identifier of the fraudster to be associated with the watchlist.
associateFraudster_fraudsterId :: Lens.Lens' AssociateFraudster Prelude.Text
associateFraudster_fraudsterId = Lens.lens (\AssociateFraudster' {fraudsterId} -> fraudsterId) (\s@AssociateFraudster' {} a -> s {fraudsterId = a} :: AssociateFraudster) Prelude.. Data._Sensitive

-- | The identifier of the watchlist you want to associate with the
-- fraudster.
associateFraudster_watchlistId :: Lens.Lens' AssociateFraudster Prelude.Text
associateFraudster_watchlistId = Lens.lens (\AssociateFraudster' {watchlistId} -> watchlistId) (\s@AssociateFraudster' {} a -> s {watchlistId = a} :: AssociateFraudster)

instance Core.AWSRequest AssociateFraudster where
  type
    AWSResponse AssociateFraudster =
      AssociateFraudsterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateFraudsterResponse'
            Prelude.<$> (x Data..?> "Fraudster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateFraudster where
  hashWithSalt _salt AssociateFraudster' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` fraudsterId
      `Prelude.hashWithSalt` watchlistId

instance Prelude.NFData AssociateFraudster where
  rnf AssociateFraudster' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fraudsterId
      `Prelude.seq` Prelude.rnf watchlistId

instance Data.ToHeaders AssociateFraudster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.AssociateFraudster" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateFraudster where
  toJSON AssociateFraudster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("FraudsterId" Data..= fraudsterId),
            Prelude.Just ("WatchlistId" Data..= watchlistId)
          ]
      )

instance Data.ToPath AssociateFraudster where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateFraudster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateFraudsterResponse' smart constructor.
data AssociateFraudsterResponse = AssociateFraudsterResponse'
  { fraudster :: Prelude.Maybe Fraudster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFraudsterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fraudster', 'associateFraudsterResponse_fraudster' - Undocumented member.
--
-- 'httpStatus', 'associateFraudsterResponse_httpStatus' - The response's http status code.
newAssociateFraudsterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateFraudsterResponse
newAssociateFraudsterResponse pHttpStatus_ =
  AssociateFraudsterResponse'
    { fraudster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
associateFraudsterResponse_fraudster :: Lens.Lens' AssociateFraudsterResponse (Prelude.Maybe Fraudster)
associateFraudsterResponse_fraudster = Lens.lens (\AssociateFraudsterResponse' {fraudster} -> fraudster) (\s@AssociateFraudsterResponse' {} a -> s {fraudster = a} :: AssociateFraudsterResponse)

-- | The response's http status code.
associateFraudsterResponse_httpStatus :: Lens.Lens' AssociateFraudsterResponse Prelude.Int
associateFraudsterResponse_httpStatus = Lens.lens (\AssociateFraudsterResponse' {httpStatus} -> httpStatus) (\s@AssociateFraudsterResponse' {} a -> s {httpStatus = a} :: AssociateFraudsterResponse)

instance Prelude.NFData AssociateFraudsterResponse where
  rnf AssociateFraudsterResponse' {..} =
    Prelude.rnf fraudster
      `Prelude.seq` Prelude.rnf httpStatus
