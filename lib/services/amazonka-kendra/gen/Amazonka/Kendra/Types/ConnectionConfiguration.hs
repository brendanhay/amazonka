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
-- Module      : Amazonka.Kendra.Types.ConnectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConnectionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information that\'s required to connect to a
-- database.
--
-- /See:/ 'newConnectionConfiguration' smart constructor.
data ConnectionConfiguration = ConnectionConfiguration'
  { -- | The name of the host for the database. Can be either a string
    -- (host.subdomain.domain.tld) or an IPv4 or IPv6 address.
    databaseHost :: Prelude.Text,
    -- | The port that the database uses for connections.
    databasePort :: Prelude.Natural,
    -- | The name of the database containing the document data.
    databaseName :: Prelude.Text,
    -- | The name of the table that contains the document data.
    tableName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of credentials stored in Secrets Manager.
    -- The credentials should be a user\/password pair. For more information,
    -- see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-database.html Using a Database Data Source>.
    -- For more information about Secrets Manager, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html What Is Secrets Manager>
    -- in the /Secrets Manager/ user guide.
    secretArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseHost', 'connectionConfiguration_databaseHost' - The name of the host for the database. Can be either a string
-- (host.subdomain.domain.tld) or an IPv4 or IPv6 address.
--
-- 'databasePort', 'connectionConfiguration_databasePort' - The port that the database uses for connections.
--
-- 'databaseName', 'connectionConfiguration_databaseName' - The name of the database containing the document data.
--
-- 'tableName', 'connectionConfiguration_tableName' - The name of the table that contains the document data.
--
-- 'secretArn', 'connectionConfiguration_secretArn' - The Amazon Resource Name (ARN) of credentials stored in Secrets Manager.
-- The credentials should be a user\/password pair. For more information,
-- see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-database.html Using a Database Data Source>.
-- For more information about Secrets Manager, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html What Is Secrets Manager>
-- in the /Secrets Manager/ user guide.
newConnectionConfiguration ::
  -- | 'databaseHost'
  Prelude.Text ->
  -- | 'databasePort'
  Prelude.Natural ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  ConnectionConfiguration
newConnectionConfiguration
  pDatabaseHost_
  pDatabasePort_
  pDatabaseName_
  pTableName_
  pSecretArn_ =
    ConnectionConfiguration'
      { databaseHost =
          pDatabaseHost_,
        databasePort = pDatabasePort_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        secretArn = pSecretArn_
      }

-- | The name of the host for the database. Can be either a string
-- (host.subdomain.domain.tld) or an IPv4 or IPv6 address.
connectionConfiguration_databaseHost :: Lens.Lens' ConnectionConfiguration Prelude.Text
connectionConfiguration_databaseHost = Lens.lens (\ConnectionConfiguration' {databaseHost} -> databaseHost) (\s@ConnectionConfiguration' {} a -> s {databaseHost = a} :: ConnectionConfiguration)

-- | The port that the database uses for connections.
connectionConfiguration_databasePort :: Lens.Lens' ConnectionConfiguration Prelude.Natural
connectionConfiguration_databasePort = Lens.lens (\ConnectionConfiguration' {databasePort} -> databasePort) (\s@ConnectionConfiguration' {} a -> s {databasePort = a} :: ConnectionConfiguration)

-- | The name of the database containing the document data.
connectionConfiguration_databaseName :: Lens.Lens' ConnectionConfiguration Prelude.Text
connectionConfiguration_databaseName = Lens.lens (\ConnectionConfiguration' {databaseName} -> databaseName) (\s@ConnectionConfiguration' {} a -> s {databaseName = a} :: ConnectionConfiguration)

-- | The name of the table that contains the document data.
connectionConfiguration_tableName :: Lens.Lens' ConnectionConfiguration Prelude.Text
connectionConfiguration_tableName = Lens.lens (\ConnectionConfiguration' {tableName} -> tableName) (\s@ConnectionConfiguration' {} a -> s {tableName = a} :: ConnectionConfiguration)

-- | The Amazon Resource Name (ARN) of credentials stored in Secrets Manager.
-- The credentials should be a user\/password pair. For more information,
-- see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-database.html Using a Database Data Source>.
-- For more information about Secrets Manager, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html What Is Secrets Manager>
-- in the /Secrets Manager/ user guide.
connectionConfiguration_secretArn :: Lens.Lens' ConnectionConfiguration Prelude.Text
connectionConfiguration_secretArn = Lens.lens (\ConnectionConfiguration' {secretArn} -> secretArn) (\s@ConnectionConfiguration' {} a -> s {secretArn = a} :: ConnectionConfiguration)

instance Data.FromJSON ConnectionConfiguration where
  parseJSON =
    Data.withObject
      "ConnectionConfiguration"
      ( \x ->
          ConnectionConfiguration'
            Prelude.<$> (x Data..: "DatabaseHost")
            Prelude.<*> (x Data..: "DatabasePort")
            Prelude.<*> (x Data..: "DatabaseName")
            Prelude.<*> (x Data..: "TableName")
            Prelude.<*> (x Data..: "SecretArn")
      )

instance Prelude.Hashable ConnectionConfiguration where
  hashWithSalt _salt ConnectionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` databaseHost
      `Prelude.hashWithSalt` databasePort
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` secretArn

instance Prelude.NFData ConnectionConfiguration where
  rnf ConnectionConfiguration' {..} =
    Prelude.rnf databaseHost
      `Prelude.seq` Prelude.rnf databasePort
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf secretArn

instance Data.ToJSON ConnectionConfiguration where
  toJSON ConnectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatabaseHost" Data..= databaseHost),
            Prelude.Just ("DatabasePort" Data..= databasePort),
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("SecretArn" Data..= secretArn)
          ]
      )
