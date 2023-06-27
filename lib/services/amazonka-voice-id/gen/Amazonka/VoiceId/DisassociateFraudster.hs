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
-- Module      : Amazonka.VoiceId.DisassociateFraudster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the fraudsters from the watchlist specified. Voice ID
-- always expects a fraudster to be a part of at least one watchlist. If
-- you try to disassociate a fraudster from its only watchlist, a
-- @ValidationException@ is thrown.
module Amazonka.VoiceId.DisassociateFraudster
  ( -- * Creating a Request
    DisassociateFraudster (..),
    newDisassociateFraudster,

    -- * Request Lenses
    disassociateFraudster_domainId,
    disassociateFraudster_fraudsterId,
    disassociateFraudster_watchlistId,

    -- * Destructuring the Response
    DisassociateFraudsterResponse (..),
    newDisassociateFraudsterResponse,

    -- * Response Lenses
    disassociateFraudsterResponse_fraudster,
    disassociateFraudsterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newDisassociateFraudster' smart constructor.
data DisassociateFraudster = DisassociateFraudster'
  { -- | The identifier of the domain that contains the fraudster.
    domainId :: Prelude.Text,
    -- | The identifier of the fraudster to be disassociated from the watchlist.
    fraudsterId :: Data.Sensitive Prelude.Text,
    -- | The identifier of the watchlist that you want to disassociate from the
    -- fraudster.
    watchlistId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFraudster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'disassociateFraudster_domainId' - The identifier of the domain that contains the fraudster.
--
-- 'fraudsterId', 'disassociateFraudster_fraudsterId' - The identifier of the fraudster to be disassociated from the watchlist.
--
-- 'watchlistId', 'disassociateFraudster_watchlistId' - The identifier of the watchlist that you want to disassociate from the
-- fraudster.
newDisassociateFraudster ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'fraudsterId'
  Prelude.Text ->
  -- | 'watchlistId'
  Prelude.Text ->
  DisassociateFraudster
newDisassociateFraudster
  pDomainId_
  pFraudsterId_
  pWatchlistId_ =
    DisassociateFraudster'
      { domainId = pDomainId_,
        fraudsterId = Data._Sensitive Lens.# pFraudsterId_,
        watchlistId = pWatchlistId_
      }

-- | The identifier of the domain that contains the fraudster.
disassociateFraudster_domainId :: Lens.Lens' DisassociateFraudster Prelude.Text
disassociateFraudster_domainId = Lens.lens (\DisassociateFraudster' {domainId} -> domainId) (\s@DisassociateFraudster' {} a -> s {domainId = a} :: DisassociateFraudster)

-- | The identifier of the fraudster to be disassociated from the watchlist.
disassociateFraudster_fraudsterId :: Lens.Lens' DisassociateFraudster Prelude.Text
disassociateFraudster_fraudsterId = Lens.lens (\DisassociateFraudster' {fraudsterId} -> fraudsterId) (\s@DisassociateFraudster' {} a -> s {fraudsterId = a} :: DisassociateFraudster) Prelude.. Data._Sensitive

-- | The identifier of the watchlist that you want to disassociate from the
-- fraudster.
disassociateFraudster_watchlistId :: Lens.Lens' DisassociateFraudster Prelude.Text
disassociateFraudster_watchlistId = Lens.lens (\DisassociateFraudster' {watchlistId} -> watchlistId) (\s@DisassociateFraudster' {} a -> s {watchlistId = a} :: DisassociateFraudster)

instance Core.AWSRequest DisassociateFraudster where
  type
    AWSResponse DisassociateFraudster =
      DisassociateFraudsterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateFraudsterResponse'
            Prelude.<$> (x Data..?> "Fraudster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateFraudster where
  hashWithSalt _salt DisassociateFraudster' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` fraudsterId
      `Prelude.hashWithSalt` watchlistId

instance Prelude.NFData DisassociateFraudster where
  rnf DisassociateFraudster' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fraudsterId
      `Prelude.seq` Prelude.rnf watchlistId

instance Data.ToHeaders DisassociateFraudster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VoiceID.DisassociateFraudster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateFraudster where
  toJSON DisassociateFraudster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("FraudsterId" Data..= fraudsterId),
            Prelude.Just ("WatchlistId" Data..= watchlistId)
          ]
      )

instance Data.ToPath DisassociateFraudster where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateFraudster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateFraudsterResponse' smart constructor.
data DisassociateFraudsterResponse = DisassociateFraudsterResponse'
  { fraudster :: Prelude.Maybe Fraudster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFraudsterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fraudster', 'disassociateFraudsterResponse_fraudster' - Undocumented member.
--
-- 'httpStatus', 'disassociateFraudsterResponse_httpStatus' - The response's http status code.
newDisassociateFraudsterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateFraudsterResponse
newDisassociateFraudsterResponse pHttpStatus_ =
  DisassociateFraudsterResponse'
    { fraudster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
disassociateFraudsterResponse_fraudster :: Lens.Lens' DisassociateFraudsterResponse (Prelude.Maybe Fraudster)
disassociateFraudsterResponse_fraudster = Lens.lens (\DisassociateFraudsterResponse' {fraudster} -> fraudster) (\s@DisassociateFraudsterResponse' {} a -> s {fraudster = a} :: DisassociateFraudsterResponse)

-- | The response's http status code.
disassociateFraudsterResponse_httpStatus :: Lens.Lens' DisassociateFraudsterResponse Prelude.Int
disassociateFraudsterResponse_httpStatus = Lens.lens (\DisassociateFraudsterResponse' {httpStatus} -> httpStatus) (\s@DisassociateFraudsterResponse' {} a -> s {httpStatus = a} :: DisassociateFraudsterResponse)

instance Prelude.NFData DisassociateFraudsterResponse where
  rnf DisassociateFraudsterResponse' {..} =
    Prelude.rnf fraudster
      `Prelude.seq` Prelude.rnf httpStatus
