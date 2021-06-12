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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | These are custom parameters to be used when the target is a Redshift
-- cluster to invoke the Redshift Data API ExecuteStatement based on
-- EventBridge events.
--
-- /See:/ 'newRedshiftDataParameters' smart constructor.
data RedshiftDataParameters = RedshiftDataParameters'
  { -- | The database user name. Required when authenticating using temporary
    -- credentials.
    dbUser :: Core.Maybe Core.Text,
    -- | The name or ARN of the secret that enables access to the database.
    -- Required when authenticating using AWS Secrets Manager.
    secretManagerArn :: Core.Maybe Core.Text,
    -- | The name of the SQL statement. You can name the SQL statement when you
    -- create it to identify the query.
    statementName :: Core.Maybe Core.Text,
    -- | Indicates whether to send an event back to EventBridge after the SQL
    -- statement runs.
    withEvent :: Core.Maybe Core.Bool,
    -- | The name of the database. Required when authenticating using temporary
    -- credentials.
    database :: Core.Text,
    -- | The SQL statement text to run.
    sql :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'sql'
  Core.Text ->
  RedshiftDataParameters
newRedshiftDataParameters pDatabase_ pSql_ =
  RedshiftDataParameters'
    { dbUser = Core.Nothing,
      secretManagerArn = Core.Nothing,
      statementName = Core.Nothing,
      withEvent = Core.Nothing,
      database = pDatabase_,
      sql = pSql_
    }

-- | The database user name. Required when authenticating using temporary
-- credentials.
redshiftDataParameters_dbUser :: Lens.Lens' RedshiftDataParameters (Core.Maybe Core.Text)
redshiftDataParameters_dbUser = Lens.lens (\RedshiftDataParameters' {dbUser} -> dbUser) (\s@RedshiftDataParameters' {} a -> s {dbUser = a} :: RedshiftDataParameters)

-- | The name or ARN of the secret that enables access to the database.
-- Required when authenticating using AWS Secrets Manager.
redshiftDataParameters_secretManagerArn :: Lens.Lens' RedshiftDataParameters (Core.Maybe Core.Text)
redshiftDataParameters_secretManagerArn = Lens.lens (\RedshiftDataParameters' {secretManagerArn} -> secretManagerArn) (\s@RedshiftDataParameters' {} a -> s {secretManagerArn = a} :: RedshiftDataParameters)

-- | The name of the SQL statement. You can name the SQL statement when you
-- create it to identify the query.
redshiftDataParameters_statementName :: Lens.Lens' RedshiftDataParameters (Core.Maybe Core.Text)
redshiftDataParameters_statementName = Lens.lens (\RedshiftDataParameters' {statementName} -> statementName) (\s@RedshiftDataParameters' {} a -> s {statementName = a} :: RedshiftDataParameters)

-- | Indicates whether to send an event back to EventBridge after the SQL
-- statement runs.
redshiftDataParameters_withEvent :: Lens.Lens' RedshiftDataParameters (Core.Maybe Core.Bool)
redshiftDataParameters_withEvent = Lens.lens (\RedshiftDataParameters' {withEvent} -> withEvent) (\s@RedshiftDataParameters' {} a -> s {withEvent = a} :: RedshiftDataParameters)

-- | The name of the database. Required when authenticating using temporary
-- credentials.
redshiftDataParameters_database :: Lens.Lens' RedshiftDataParameters Core.Text
redshiftDataParameters_database = Lens.lens (\RedshiftDataParameters' {database} -> database) (\s@RedshiftDataParameters' {} a -> s {database = a} :: RedshiftDataParameters)

-- | The SQL statement text to run.
redshiftDataParameters_sql :: Lens.Lens' RedshiftDataParameters Core.Text
redshiftDataParameters_sql = Lens.lens (\RedshiftDataParameters' {sql} -> sql) (\s@RedshiftDataParameters' {} a -> s {sql = a} :: RedshiftDataParameters)

instance Core.FromJSON RedshiftDataParameters where
  parseJSON =
    Core.withObject
      "RedshiftDataParameters"
      ( \x ->
          RedshiftDataParameters'
            Core.<$> (x Core..:? "DbUser")
            Core.<*> (x Core..:? "SecretManagerArn")
            Core.<*> (x Core..:? "StatementName")
            Core.<*> (x Core..:? "WithEvent")
            Core.<*> (x Core..: "Database")
            Core.<*> (x Core..: "Sql")
      )

instance Core.Hashable RedshiftDataParameters

instance Core.NFData RedshiftDataParameters

instance Core.ToJSON RedshiftDataParameters where
  toJSON RedshiftDataParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DbUser" Core..=) Core.<$> dbUser,
            ("SecretManagerArn" Core..=)
              Core.<$> secretManagerArn,
            ("StatementName" Core..=) Core.<$> statementName,
            ("WithEvent" Core..=) Core.<$> withEvent,
            Core.Just ("Database" Core..= database),
            Core.Just ("Sql" Core..= sql)
          ]
      )
