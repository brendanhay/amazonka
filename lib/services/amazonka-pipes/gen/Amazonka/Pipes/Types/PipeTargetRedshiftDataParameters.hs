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
-- Module      : Amazonka.Pipes.Types.PipeTargetRedshiftDataParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetRedshiftDataParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | These are custom parameters to be used when the target is a Amazon
-- Redshift cluster to invoke the Amazon Redshift Data API
-- ExecuteStatement.
--
-- /See:/ 'newPipeTargetRedshiftDataParameters' smart constructor.
data PipeTargetRedshiftDataParameters = PipeTargetRedshiftDataParameters'
  { -- | The database user name. Required when authenticating using temporary
    -- credentials.
    dbUser :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name or ARN of the secret that enables access to the database.
    -- Required when authenticating using SageMaker.
    secretManagerArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the SQL statement. You can name the SQL statement when you
    -- create it to identify the query.
    statementName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Indicates whether to send an event back to EventBridge after the SQL
    -- statement runs.
    withEvent :: Prelude.Maybe Prelude.Bool,
    -- | The name of the database. Required when authenticating using temporary
    -- credentials.
    database :: Data.Sensitive Prelude.Text,
    -- | The SQL statement text to run.
    sqls :: Prelude.NonEmpty (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetRedshiftDataParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbUser', 'pipeTargetRedshiftDataParameters_dbUser' - The database user name. Required when authenticating using temporary
-- credentials.
--
-- 'secretManagerArn', 'pipeTargetRedshiftDataParameters_secretManagerArn' - The name or ARN of the secret that enables access to the database.
-- Required when authenticating using SageMaker.
--
-- 'statementName', 'pipeTargetRedshiftDataParameters_statementName' - The name of the SQL statement. You can name the SQL statement when you
-- create it to identify the query.
--
-- 'withEvent', 'pipeTargetRedshiftDataParameters_withEvent' - Indicates whether to send an event back to EventBridge after the SQL
-- statement runs.
--
-- 'database', 'pipeTargetRedshiftDataParameters_database' - The name of the database. Required when authenticating using temporary
-- credentials.
--
-- 'sqls', 'pipeTargetRedshiftDataParameters_sqls' - The SQL statement text to run.
newPipeTargetRedshiftDataParameters ::
  -- | 'database'
  Prelude.Text ->
  -- | 'sqls'
  Prelude.NonEmpty Prelude.Text ->
  PipeTargetRedshiftDataParameters
newPipeTargetRedshiftDataParameters pDatabase_ pSqls_ =
  PipeTargetRedshiftDataParameters'
    { dbUser =
        Prelude.Nothing,
      secretManagerArn = Prelude.Nothing,
      statementName = Prelude.Nothing,
      withEvent = Prelude.Nothing,
      database =
        Data._Sensitive Lens.# pDatabase_,
      sqls = Lens.coerced Lens.# pSqls_
    }

-- | The database user name. Required when authenticating using temporary
-- credentials.
pipeTargetRedshiftDataParameters_dbUser :: Lens.Lens' PipeTargetRedshiftDataParameters (Prelude.Maybe Prelude.Text)
pipeTargetRedshiftDataParameters_dbUser = Lens.lens (\PipeTargetRedshiftDataParameters' {dbUser} -> dbUser) (\s@PipeTargetRedshiftDataParameters' {} a -> s {dbUser = a} :: PipeTargetRedshiftDataParameters) Prelude.. Lens.mapping Data._Sensitive

-- | The name or ARN of the secret that enables access to the database.
-- Required when authenticating using SageMaker.
pipeTargetRedshiftDataParameters_secretManagerArn :: Lens.Lens' PipeTargetRedshiftDataParameters (Prelude.Maybe Prelude.Text)
pipeTargetRedshiftDataParameters_secretManagerArn = Lens.lens (\PipeTargetRedshiftDataParameters' {secretManagerArn} -> secretManagerArn) (\s@PipeTargetRedshiftDataParameters' {} a -> s {secretManagerArn = a} :: PipeTargetRedshiftDataParameters)

-- | The name of the SQL statement. You can name the SQL statement when you
-- create it to identify the query.
pipeTargetRedshiftDataParameters_statementName :: Lens.Lens' PipeTargetRedshiftDataParameters (Prelude.Maybe Prelude.Text)
pipeTargetRedshiftDataParameters_statementName = Lens.lens (\PipeTargetRedshiftDataParameters' {statementName} -> statementName) (\s@PipeTargetRedshiftDataParameters' {} a -> s {statementName = a} :: PipeTargetRedshiftDataParameters) Prelude.. Lens.mapping Data._Sensitive

-- | Indicates whether to send an event back to EventBridge after the SQL
-- statement runs.
pipeTargetRedshiftDataParameters_withEvent :: Lens.Lens' PipeTargetRedshiftDataParameters (Prelude.Maybe Prelude.Bool)
pipeTargetRedshiftDataParameters_withEvent = Lens.lens (\PipeTargetRedshiftDataParameters' {withEvent} -> withEvent) (\s@PipeTargetRedshiftDataParameters' {} a -> s {withEvent = a} :: PipeTargetRedshiftDataParameters)

-- | The name of the database. Required when authenticating using temporary
-- credentials.
pipeTargetRedshiftDataParameters_database :: Lens.Lens' PipeTargetRedshiftDataParameters Prelude.Text
pipeTargetRedshiftDataParameters_database = Lens.lens (\PipeTargetRedshiftDataParameters' {database} -> database) (\s@PipeTargetRedshiftDataParameters' {} a -> s {database = a} :: PipeTargetRedshiftDataParameters) Prelude.. Data._Sensitive

-- | The SQL statement text to run.
pipeTargetRedshiftDataParameters_sqls :: Lens.Lens' PipeTargetRedshiftDataParameters (Prelude.NonEmpty Prelude.Text)
pipeTargetRedshiftDataParameters_sqls = Lens.lens (\PipeTargetRedshiftDataParameters' {sqls} -> sqls) (\s@PipeTargetRedshiftDataParameters' {} a -> s {sqls = a} :: PipeTargetRedshiftDataParameters) Prelude.. Lens.coerced

instance
  Data.FromJSON
    PipeTargetRedshiftDataParameters
  where
  parseJSON =
    Data.withObject
      "PipeTargetRedshiftDataParameters"
      ( \x ->
          PipeTargetRedshiftDataParameters'
            Prelude.<$> (x Data..:? "DbUser")
            Prelude.<*> (x Data..:? "SecretManagerArn")
            Prelude.<*> (x Data..:? "StatementName")
            Prelude.<*> (x Data..:? "WithEvent")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Sqls")
      )

instance
  Prelude.Hashable
    PipeTargetRedshiftDataParameters
  where
  hashWithSalt
    _salt
    PipeTargetRedshiftDataParameters' {..} =
      _salt `Prelude.hashWithSalt` dbUser
        `Prelude.hashWithSalt` secretManagerArn
        `Prelude.hashWithSalt` statementName
        `Prelude.hashWithSalt` withEvent
        `Prelude.hashWithSalt` database
        `Prelude.hashWithSalt` sqls

instance
  Prelude.NFData
    PipeTargetRedshiftDataParameters
  where
  rnf PipeTargetRedshiftDataParameters' {..} =
    Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf secretManagerArn
      `Prelude.seq` Prelude.rnf statementName
      `Prelude.seq` Prelude.rnf withEvent
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf sqls

instance Data.ToJSON PipeTargetRedshiftDataParameters where
  toJSON PipeTargetRedshiftDataParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DbUser" Data..=) Prelude.<$> dbUser,
            ("SecretManagerArn" Data..=)
              Prelude.<$> secretManagerArn,
            ("StatementName" Data..=) Prelude.<$> statementName,
            ("WithEvent" Data..=) Prelude.<$> withEvent,
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Sqls" Data..= sqls)
          ]
      )
