{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QLDBSession.Types.FetchPageRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.FetchPageRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the details of the page to be fetched.
--
-- /See:/ 'newFetchPageRequest' smart constructor.
data FetchPageRequest = FetchPageRequest'
  { -- | Specifies the transaction ID of the page to be fetched.
    transactionId :: Prelude.Text,
    -- | Specifies the next page token of the page to be fetched.
    nextPageToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FetchPageRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionId', 'fetchPageRequest_transactionId' - Specifies the transaction ID of the page to be fetched.
--
-- 'nextPageToken', 'fetchPageRequest_nextPageToken' - Specifies the next page token of the page to be fetched.
newFetchPageRequest ::
  -- | 'transactionId'
  Prelude.Text ->
  -- | 'nextPageToken'
  Prelude.Text ->
  FetchPageRequest
newFetchPageRequest pTransactionId_ pNextPageToken_ =
  FetchPageRequest'
    { transactionId = pTransactionId_,
      nextPageToken = pNextPageToken_
    }

-- | Specifies the transaction ID of the page to be fetched.
fetchPageRequest_transactionId :: Lens.Lens' FetchPageRequest Prelude.Text
fetchPageRequest_transactionId = Lens.lens (\FetchPageRequest' {transactionId} -> transactionId) (\s@FetchPageRequest' {} a -> s {transactionId = a} :: FetchPageRequest)

-- | Specifies the next page token of the page to be fetched.
fetchPageRequest_nextPageToken :: Lens.Lens' FetchPageRequest Prelude.Text
fetchPageRequest_nextPageToken = Lens.lens (\FetchPageRequest' {nextPageToken} -> nextPageToken) (\s@FetchPageRequest' {} a -> s {nextPageToken = a} :: FetchPageRequest)

instance Prelude.Hashable FetchPageRequest where
  hashWithSalt _salt FetchPageRequest' {..} =
    _salt
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` nextPageToken

instance Prelude.NFData FetchPageRequest where
  rnf FetchPageRequest' {..} =
    Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf nextPageToken

instance Data.ToJSON FetchPageRequest where
  toJSON FetchPageRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransactionId" Data..= transactionId),
            Prelude.Just
              ("NextPageToken" Data..= nextPageToken)
          ]
      )
