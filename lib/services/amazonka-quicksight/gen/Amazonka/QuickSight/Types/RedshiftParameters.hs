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
-- Module      : Amazonka.QuickSight.Types.RedshiftParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RedshiftParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for Amazon Redshift. The @ClusterId@ field can be blank
-- if @Host@ and @Port@ are both set. The @Host@ and @Port@ fields can be
-- blank if the @ClusterId@ field is set.
--
-- /See:/ 'newRedshiftParameters' smart constructor.
data RedshiftParameters = RedshiftParameters'
  { -- | Cluster ID. This field can be blank if the @Host@ and @Port@ are
    -- provided.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | Host. This field can be blank if @ClusterId@ is provided.
    host :: Prelude.Maybe Prelude.Text,
    -- | Port. This field can be blank if the @ClusterId@ is provided.
    port :: Prelude.Maybe Prelude.Natural,
    -- | Database.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'redshiftParameters_clusterId' - Cluster ID. This field can be blank if the @Host@ and @Port@ are
-- provided.
--
-- 'host', 'redshiftParameters_host' - Host. This field can be blank if @ClusterId@ is provided.
--
-- 'port', 'redshiftParameters_port' - Port. This field can be blank if the @ClusterId@ is provided.
--
-- 'database', 'redshiftParameters_database' - Database.
newRedshiftParameters ::
  -- | 'database'
  Prelude.Text ->
  RedshiftParameters
newRedshiftParameters pDatabase_ =
  RedshiftParameters'
    { clusterId = Prelude.Nothing,
      host = Prelude.Nothing,
      port = Prelude.Nothing,
      database = pDatabase_
    }

-- | Cluster ID. This field can be blank if the @Host@ and @Port@ are
-- provided.
redshiftParameters_clusterId :: Lens.Lens' RedshiftParameters (Prelude.Maybe Prelude.Text)
redshiftParameters_clusterId = Lens.lens (\RedshiftParameters' {clusterId} -> clusterId) (\s@RedshiftParameters' {} a -> s {clusterId = a} :: RedshiftParameters)

-- | Host. This field can be blank if @ClusterId@ is provided.
redshiftParameters_host :: Lens.Lens' RedshiftParameters (Prelude.Maybe Prelude.Text)
redshiftParameters_host = Lens.lens (\RedshiftParameters' {host} -> host) (\s@RedshiftParameters' {} a -> s {host = a} :: RedshiftParameters)

-- | Port. This field can be blank if the @ClusterId@ is provided.
redshiftParameters_port :: Lens.Lens' RedshiftParameters (Prelude.Maybe Prelude.Natural)
redshiftParameters_port = Lens.lens (\RedshiftParameters' {port} -> port) (\s@RedshiftParameters' {} a -> s {port = a} :: RedshiftParameters)

-- | Database.
redshiftParameters_database :: Lens.Lens' RedshiftParameters Prelude.Text
redshiftParameters_database = Lens.lens (\RedshiftParameters' {database} -> database) (\s@RedshiftParameters' {} a -> s {database = a} :: RedshiftParameters)

instance Data.FromJSON RedshiftParameters where
  parseJSON =
    Data.withObject
      "RedshiftParameters"
      ( \x ->
          RedshiftParameters'
            Prelude.<$> (x Data..:? "ClusterId")
            Prelude.<*> (x Data..:? "Host")
            Prelude.<*> (x Data..:? "Port")
            Prelude.<*> (x Data..: "Database")
      )

instance Prelude.Hashable RedshiftParameters where
  hashWithSalt _salt RedshiftParameters' {..} =
    _salt
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` database

instance Prelude.NFData RedshiftParameters where
  rnf RedshiftParameters' {..} =
    Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf host
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf database

instance Data.ToJSON RedshiftParameters where
  toJSON RedshiftParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterId" Data..=) Prelude.<$> clusterId,
            ("Host" Data..=) Prelude.<$> host,
            ("Port" Data..=) Prelude.<$> port,
            Prelude.Just ("Database" Data..= database)
          ]
      )
