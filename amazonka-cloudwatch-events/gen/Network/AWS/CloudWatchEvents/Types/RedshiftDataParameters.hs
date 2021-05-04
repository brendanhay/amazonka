{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | These are custom parameters to be used when the target is a Redshift
-- cluster to invoke the Redshift Data API ExecuteStatement based on
-- EventBridge events.
--
-- /See:/ 'newRedshiftDataParameters' smart constructor.
data RedshiftDataParameters = RedshiftDataParameters'
  { -- | The database user name. Required when authenticating using temporary
    -- credentials.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the secret that enables access to the database.
    -- Required when authenticating using AWS Secrets Manager.
    secretManagerArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the SQL statement. You can name the SQL statement when you
    -- create it to identify the query.
    statementName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to send an event back to EventBridge after the SQL
    -- statement runs.
    withEvent :: Prelude.Maybe Prelude.Bool,
    -- | The name of the database. Required when authenticating using temporary
    -- credentials.
    database :: Prelude.Text,
    -- | The SQL statement text to run.
    sql :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDataParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbUser', 'redshiftDataParameters_dbUser' - The database user name. Required when authenticating using temporary
-- credentials.
--
-- 'secretManagerArn', 'redshiftDataParameters_secretManagerArn' - The name or ARN of the secret that enables access to the database.
-- Required when authenticating using AWS Secrets Manager.
--
-- 'statementName', 'redshiftDataParameters_statementName' - The name of the SQL statement. You can name the SQL statement when you
-- create it to identify the query.
--
-- 'withEvent', 'redshiftDataParameters_withEvent' - Indicates whether to send an event back to EventBridge after the SQL
-- statement runs.
--
-- 'database', 'redshiftDataParameters_database' - The name of the database. Required when authenticating using temporary
-- credentials.
--
-- 'sql', 'redshiftDataParameters_sql' - The SQL statement text to run.
newRedshiftDataParameters ::
  -- | 'database'
  Prelude.Text ->
  -- | 'sql'
  Prelude.Text ->
  RedshiftDataParameters
newRedshiftDataParameters pDatabase_ pSql_ =
  RedshiftDataParameters'
    { dbUser = Prelude.Nothing,
      secretManagerArn = Prelude.Nothing,
      statementName = Prelude.Nothing,
      withEvent = Prelude.Nothing,
      database = pDatabase_,
      sql = pSql_
    }

-- | The database user name. Required when authenticating using temporary
-- credentials.
redshiftDataParameters_dbUser :: Lens.Lens' RedshiftDataParameters (Prelude.Maybe Prelude.Text)
redshiftDataParameters_dbUser = Lens.lens (\RedshiftDataParameters' {dbUser} -> dbUser) (\s@RedshiftDataParameters' {} a -> s {dbUser = a} :: RedshiftDataParameters)

-- | The name or ARN of the secret that enables access to the database.
-- Required when authenticating using AWS Secrets Manager.
redshiftDataParameters_secretManagerArn :: Lens.Lens' RedshiftDataParameters (Prelude.Maybe Prelude.Text)
redshiftDataParameters_secretManagerArn = Lens.lens (\RedshiftDataParameters' {secretManagerArn} -> secretManagerArn) (\s@RedshiftDataParameters' {} a -> s {secretManagerArn = a} :: RedshiftDataParameters)

-- | The name of the SQL statement. You can name the SQL statement when you
-- create it to identify the query.
redshiftDataParameters_statementName :: Lens.Lens' RedshiftDataParameters (Prelude.Maybe Prelude.Text)
redshiftDataParameters_statementName = Lens.lens (\RedshiftDataParameters' {statementName} -> statementName) (\s@RedshiftDataParameters' {} a -> s {statementName = a} :: RedshiftDataParameters)

-- | Indicates whether to send an event back to EventBridge after the SQL
-- statement runs.
redshiftDataParameters_withEvent :: Lens.Lens' RedshiftDataParameters (Prelude.Maybe Prelude.Bool)
redshiftDataParameters_withEvent = Lens.lens (\RedshiftDataParameters' {withEvent} -> withEvent) (\s@RedshiftDataParameters' {} a -> s {withEvent = a} :: RedshiftDataParameters)

-- | The name of the database. Required when authenticating using temporary
-- credentials.
redshiftDataParameters_database :: Lens.Lens' RedshiftDataParameters Prelude.Text
redshiftDataParameters_database = Lens.lens (\RedshiftDataParameters' {database} -> database) (\s@RedshiftDataParameters' {} a -> s {database = a} :: RedshiftDataParameters)

-- | The SQL statement text to run.
redshiftDataParameters_sql :: Lens.Lens' RedshiftDataParameters Prelude.Text
redshiftDataParameters_sql = Lens.lens (\RedshiftDataParameters' {sql} -> sql) (\s@RedshiftDataParameters' {} a -> s {sql = a} :: RedshiftDataParameters)

instance Prelude.FromJSON RedshiftDataParameters where
  parseJSON =
    Prelude.withObject
      "RedshiftDataParameters"
      ( \x ->
          RedshiftDataParameters'
            Prelude.<$> (x Prelude..:? "DbUser")
            Prelude.<*> (x Prelude..:? "SecretManagerArn")
            Prelude.<*> (x Prelude..:? "StatementName")
            Prelude.<*> (x Prelude..:? "WithEvent")
            Prelude.<*> (x Prelude..: "Database")
            Prelude.<*> (x Prelude..: "Sql")
      )

instance Prelude.Hashable RedshiftDataParameters

instance Prelude.NFData RedshiftDataParameters

instance Prelude.ToJSON RedshiftDataParameters where
  toJSON RedshiftDataParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DbUser" Prelude..=) Prelude.<$> dbUser,
            ("SecretManagerArn" Prelude..=)
              Prelude.<$> secretManagerArn,
            ("StatementName" Prelude..=)
              Prelude.<$> statementName,
            ("WithEvent" Prelude..=) Prelude.<$> withEvent,
            Prelude.Just ("Database" Prelude..= database),
            Prelude.Just ("Sql" Prelude..= sql)
          ]
      )
