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
-- Module      : Amazonka.RDSData.BeginTransaction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a SQL transaction.
--
-- A transaction can run for a maximum of 24 hours. A transaction is
-- terminated and rolled back automatically after 24 hours.
--
-- A transaction times out if no calls use its transaction ID in three
-- minutes. If a transaction times out before it\'s committed, it\'s rolled
-- back automatically.
--
-- DDL statements inside a transaction cause an implicit commit. We
-- recommend that you run each DDL statement in a separate
-- @ExecuteStatement@ call with @continueAfterTimeout@ enabled.
module Amazonka.RDSData.BeginTransaction
  ( -- * Creating a Request
    BeginTransaction (..),
    newBeginTransaction,

    -- * Request Lenses
    beginTransaction_database,
    beginTransaction_schema,
    beginTransaction_resourceArn,
    beginTransaction_secretArn,

    -- * Destructuring the Response
    BeginTransactionResponse (..),
    newBeginTransactionResponse,

    -- * Response Lenses
    beginTransactionResponse_transactionId,
    beginTransactionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDSData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request parameters represent the input of a request to start a SQL
-- transaction.
--
-- /See:/ 'newBeginTransaction' smart constructor.
data BeginTransaction = BeginTransaction'
  { -- | The name of the database.
    database :: Prelude.Maybe Prelude.Text,
    -- | The name of the database schema.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
    resourceArn :: Prelude.Text,
    -- | The name or ARN of the secret that enables access to the DB cluster.
    secretArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BeginTransaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'beginTransaction_database' - The name of the database.
--
-- 'schema', 'beginTransaction_schema' - The name of the database schema.
--
-- 'resourceArn', 'beginTransaction_resourceArn' - The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
--
-- 'secretArn', 'beginTransaction_secretArn' - The name or ARN of the secret that enables access to the DB cluster.
newBeginTransaction ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  BeginTransaction
newBeginTransaction pResourceArn_ pSecretArn_ =
  BeginTransaction'
    { database = Prelude.Nothing,
      schema = Prelude.Nothing,
      resourceArn = pResourceArn_,
      secretArn = pSecretArn_
    }

-- | The name of the database.
beginTransaction_database :: Lens.Lens' BeginTransaction (Prelude.Maybe Prelude.Text)
beginTransaction_database = Lens.lens (\BeginTransaction' {database} -> database) (\s@BeginTransaction' {} a -> s {database = a} :: BeginTransaction)

-- | The name of the database schema.
beginTransaction_schema :: Lens.Lens' BeginTransaction (Prelude.Maybe Prelude.Text)
beginTransaction_schema = Lens.lens (\BeginTransaction' {schema} -> schema) (\s@BeginTransaction' {} a -> s {schema = a} :: BeginTransaction)

-- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
beginTransaction_resourceArn :: Lens.Lens' BeginTransaction Prelude.Text
beginTransaction_resourceArn = Lens.lens (\BeginTransaction' {resourceArn} -> resourceArn) (\s@BeginTransaction' {} a -> s {resourceArn = a} :: BeginTransaction)

-- | The name or ARN of the secret that enables access to the DB cluster.
beginTransaction_secretArn :: Lens.Lens' BeginTransaction Prelude.Text
beginTransaction_secretArn = Lens.lens (\BeginTransaction' {secretArn} -> secretArn) (\s@BeginTransaction' {} a -> s {secretArn = a} :: BeginTransaction)

instance Core.AWSRequest BeginTransaction where
  type
    AWSResponse BeginTransaction =
      BeginTransactionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BeginTransactionResponse'
            Prelude.<$> (x Data..?> "transactionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BeginTransaction where
  hashWithSalt _salt BeginTransaction' {..} =
    _salt
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` secretArn

instance Prelude.NFData BeginTransaction where
  rnf BeginTransaction' {..} =
    Prelude.rnf database
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf secretArn

instance Data.ToHeaders BeginTransaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BeginTransaction where
  toJSON BeginTransaction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("database" Data..=) Prelude.<$> database,
            ("schema" Data..=) Prelude.<$> schema,
            Prelude.Just ("resourceArn" Data..= resourceArn),
            Prelude.Just ("secretArn" Data..= secretArn)
          ]
      )

instance Data.ToPath BeginTransaction where
  toPath = Prelude.const "/BeginTransaction"

instance Data.ToQuery BeginTransaction where
  toQuery = Prelude.const Prelude.mempty

-- | The response elements represent the output of a request to start a SQL
-- transaction.
--
-- /See:/ 'newBeginTransactionResponse' smart constructor.
data BeginTransactionResponse = BeginTransactionResponse'
  { -- | The transaction ID of the transaction started by the call.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BeginTransactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionId', 'beginTransactionResponse_transactionId' - The transaction ID of the transaction started by the call.
--
-- 'httpStatus', 'beginTransactionResponse_httpStatus' - The response's http status code.
newBeginTransactionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BeginTransactionResponse
newBeginTransactionResponse pHttpStatus_ =
  BeginTransactionResponse'
    { transactionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The transaction ID of the transaction started by the call.
beginTransactionResponse_transactionId :: Lens.Lens' BeginTransactionResponse (Prelude.Maybe Prelude.Text)
beginTransactionResponse_transactionId = Lens.lens (\BeginTransactionResponse' {transactionId} -> transactionId) (\s@BeginTransactionResponse' {} a -> s {transactionId = a} :: BeginTransactionResponse)

-- | The response's http status code.
beginTransactionResponse_httpStatus :: Lens.Lens' BeginTransactionResponse Prelude.Int
beginTransactionResponse_httpStatus = Lens.lens (\BeginTransactionResponse' {httpStatus} -> httpStatus) (\s@BeginTransactionResponse' {} a -> s {httpStatus = a} :: BeginTransactionResponse)

instance Prelude.NFData BeginTransactionResponse where
  rnf BeginTransactionResponse' {..} =
    Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf httpStatus
