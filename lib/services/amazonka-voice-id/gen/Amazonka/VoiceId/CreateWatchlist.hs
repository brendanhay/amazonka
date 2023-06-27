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
-- Module      : Amazonka.VoiceId.CreateWatchlist
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a watchlist that fraudsters can be a part of.
module Amazonka.VoiceId.CreateWatchlist
  ( -- * Creating a Request
    CreateWatchlist (..),
    newCreateWatchlist,

    -- * Request Lenses
    createWatchlist_clientToken,
    createWatchlist_description,
    createWatchlist_domainId,
    createWatchlist_name,

    -- * Destructuring the Response
    CreateWatchlistResponse (..),
    newCreateWatchlistResponse,

    -- * Response Lenses
    createWatchlistResponse_watchlist,
    createWatchlistResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newCreateWatchlist' smart constructor.
data CreateWatchlist = CreateWatchlist'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A brief description of this watchlist.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The identifier of the domain that contains the watchlist.
    domainId :: Prelude.Text,
    -- | The name of the watchlist.
    name :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWatchlist' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createWatchlist_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'description', 'createWatchlist_description' - A brief description of this watchlist.
--
-- 'domainId', 'createWatchlist_domainId' - The identifier of the domain that contains the watchlist.
--
-- 'name', 'createWatchlist_name' - The name of the watchlist.
newCreateWatchlist ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateWatchlist
newCreateWatchlist pDomainId_ pName_ =
  CreateWatchlist'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      domainId = pDomainId_,
      name = Data._Sensitive Lens.# pName_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
createWatchlist_clientToken :: Lens.Lens' CreateWatchlist (Prelude.Maybe Prelude.Text)
createWatchlist_clientToken = Lens.lens (\CreateWatchlist' {clientToken} -> clientToken) (\s@CreateWatchlist' {} a -> s {clientToken = a} :: CreateWatchlist)

-- | A brief description of this watchlist.
createWatchlist_description :: Lens.Lens' CreateWatchlist (Prelude.Maybe Prelude.Text)
createWatchlist_description = Lens.lens (\CreateWatchlist' {description} -> description) (\s@CreateWatchlist' {} a -> s {description = a} :: CreateWatchlist) Prelude.. Lens.mapping Data._Sensitive

-- | The identifier of the domain that contains the watchlist.
createWatchlist_domainId :: Lens.Lens' CreateWatchlist Prelude.Text
createWatchlist_domainId = Lens.lens (\CreateWatchlist' {domainId} -> domainId) (\s@CreateWatchlist' {} a -> s {domainId = a} :: CreateWatchlist)

-- | The name of the watchlist.
createWatchlist_name :: Lens.Lens' CreateWatchlist Prelude.Text
createWatchlist_name = Lens.lens (\CreateWatchlist' {name} -> name) (\s@CreateWatchlist' {} a -> s {name = a} :: CreateWatchlist) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateWatchlist where
  type
    AWSResponse CreateWatchlist =
      CreateWatchlistResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWatchlistResponse'
            Prelude.<$> (x Data..?> "Watchlist")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWatchlist where
  hashWithSalt _salt CreateWatchlist' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateWatchlist where
  rnf CreateWatchlist' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateWatchlist where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.CreateWatchlist" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWatchlist where
  toJSON CreateWatchlist' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateWatchlist where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateWatchlist where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWatchlistResponse' smart constructor.
data CreateWatchlistResponse = CreateWatchlistResponse'
  { -- | Information about the newly created watchlist.
    watchlist :: Prelude.Maybe Watchlist,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWatchlistResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'watchlist', 'createWatchlistResponse_watchlist' - Information about the newly created watchlist.
--
-- 'httpStatus', 'createWatchlistResponse_httpStatus' - The response's http status code.
newCreateWatchlistResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWatchlistResponse
newCreateWatchlistResponse pHttpStatus_ =
  CreateWatchlistResponse'
    { watchlist =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the newly created watchlist.
createWatchlistResponse_watchlist :: Lens.Lens' CreateWatchlistResponse (Prelude.Maybe Watchlist)
createWatchlistResponse_watchlist = Lens.lens (\CreateWatchlistResponse' {watchlist} -> watchlist) (\s@CreateWatchlistResponse' {} a -> s {watchlist = a} :: CreateWatchlistResponse)

-- | The response's http status code.
createWatchlistResponse_httpStatus :: Lens.Lens' CreateWatchlistResponse Prelude.Int
createWatchlistResponse_httpStatus = Lens.lens (\CreateWatchlistResponse' {httpStatus} -> httpStatus) (\s@CreateWatchlistResponse' {} a -> s {httpStatus = a} :: CreateWatchlistResponse)

instance Prelude.NFData CreateWatchlistResponse where
  rnf CreateWatchlistResponse' {..} =
    Prelude.rnf watchlist
      `Prelude.seq` Prelude.rnf httpStatus
