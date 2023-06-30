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
-- Module      : Amazonka.QLDBSession.Types.CommitTransactionRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.CommitTransactionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of the transaction to commit.
--
-- /See:/ 'newCommitTransactionRequest' smart constructor.
data CommitTransactionRequest = CommitTransactionRequest'
  { -- | Specifies the transaction ID of the transaction to commit.
    transactionId :: Prelude.Text,
    -- | Specifies the commit digest for the transaction to commit. For every
    -- active transaction, the commit digest must be passed. QLDB validates
    -- @CommitDigest@ and rejects the commit with an error if the digest
    -- computed on the client does not match the digest computed by QLDB.
    --
    -- The purpose of the @CommitDigest@ parameter is to ensure that QLDB
    -- commits a transaction if and only if the server has processed the exact
    -- set of statements sent by the client, in the same order that client sent
    -- them, and with no duplicates.
    commitDigest :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommitTransactionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionId', 'commitTransactionRequest_transactionId' - Specifies the transaction ID of the transaction to commit.
--
-- 'commitDigest', 'commitTransactionRequest_commitDigest' - Specifies the commit digest for the transaction to commit. For every
-- active transaction, the commit digest must be passed. QLDB validates
-- @CommitDigest@ and rejects the commit with an error if the digest
-- computed on the client does not match the digest computed by QLDB.
--
-- The purpose of the @CommitDigest@ parameter is to ensure that QLDB
-- commits a transaction if and only if the server has processed the exact
-- set of statements sent by the client, in the same order that client sent
-- them, and with no duplicates.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newCommitTransactionRequest ::
  -- | 'transactionId'
  Prelude.Text ->
  -- | 'commitDigest'
  Prelude.ByteString ->
  CommitTransactionRequest
newCommitTransactionRequest
  pTransactionId_
  pCommitDigest_ =
    CommitTransactionRequest'
      { transactionId =
          pTransactionId_,
        commitDigest = Data._Base64 Lens.# pCommitDigest_
      }

-- | Specifies the transaction ID of the transaction to commit.
commitTransactionRequest_transactionId :: Lens.Lens' CommitTransactionRequest Prelude.Text
commitTransactionRequest_transactionId = Lens.lens (\CommitTransactionRequest' {transactionId} -> transactionId) (\s@CommitTransactionRequest' {} a -> s {transactionId = a} :: CommitTransactionRequest)

-- | Specifies the commit digest for the transaction to commit. For every
-- active transaction, the commit digest must be passed. QLDB validates
-- @CommitDigest@ and rejects the commit with an error if the digest
-- computed on the client does not match the digest computed by QLDB.
--
-- The purpose of the @CommitDigest@ parameter is to ensure that QLDB
-- commits a transaction if and only if the server has processed the exact
-- set of statements sent by the client, in the same order that client sent
-- them, and with no duplicates.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
commitTransactionRequest_commitDigest :: Lens.Lens' CommitTransactionRequest Prelude.ByteString
commitTransactionRequest_commitDigest = Lens.lens (\CommitTransactionRequest' {commitDigest} -> commitDigest) (\s@CommitTransactionRequest' {} a -> s {commitDigest = a} :: CommitTransactionRequest) Prelude.. Data._Base64

instance Prelude.Hashable CommitTransactionRequest where
  hashWithSalt _salt CommitTransactionRequest' {..} =
    _salt
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` commitDigest

instance Prelude.NFData CommitTransactionRequest where
  rnf CommitTransactionRequest' {..} =
    Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf commitDigest

instance Data.ToJSON CommitTransactionRequest where
  toJSON CommitTransactionRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransactionId" Data..= transactionId),
            Prelude.Just ("CommitDigest" Data..= commitDigest)
          ]
      )
