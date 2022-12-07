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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | Port. This field can be blank if the @ClusterId@ is provided.
    port :: Prelude.Maybe Prelude.Natural,
    -- | Host. This field can be blank if @ClusterId@ is provided.
    host :: Prelude.Maybe Prelude.Text,
    -- | Cluster ID. This field can be blank if the @Host@ and @Port@ are
    -- provided.
    clusterId :: Prelude.Maybe Prelude.Text,
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
-- 'port', 'redshiftParameters_port' - Port. This field can be blank if the @ClusterId@ is provided.
--
-- 'host', 'redshiftParameters_host' - Host. This field can be blank if @ClusterId@ is provided.
--
-- 'clusterId', 'redshiftParameters_clusterId' - Cluster ID. This field can be blank if the @Host@ and @Port@ are
-- provided.
--
-- 'database', 'redshiftParameters_database' - Database.
newRedshiftParameters ::
  -- | 'database'
  Prelude.Text ->
  RedshiftParameters
newRedshiftParameters pDatabase_ =
  RedshiftParameters'
    { port = Prelude.Nothing,
      host = Prelude.Nothing,
      clusterId = Prelude.Nothing,
      database = pDatabase_
    }

-- | Port. This field can be blank if the @ClusterId@ is provided.
redshiftParameters_port :: Lens.Lens' RedshiftParameters (Prelude.Maybe Prelude.Natural)
redshiftParameters_port = Lens.lens (\RedshiftParameters' {port} -> port) (\s@RedshiftParameters' {} a -> s {port = a} :: RedshiftParameters)

-- | Host. This field can be blank if @ClusterId@ is provided.
redshiftParameters_host :: Lens.Lens' RedshiftParameters (Prelude.Maybe Prelude.Text)
redshiftParameters_host = Lens.lens (\RedshiftParameters' {host} -> host) (\s@RedshiftParameters' {} a -> s {host = a} :: RedshiftParameters)

-- | Cluster ID. This field can be blank if the @Host@ and @Port@ are
-- provided.
redshiftParameters_clusterId :: Lens.Lens' RedshiftParameters (Prelude.Maybe Prelude.Text)
redshiftParameters_clusterId = Lens.lens (\RedshiftParameters' {clusterId} -> clusterId) (\s@RedshiftParameters' {} a -> s {clusterId = a} :: RedshiftParameters)

-- | Database.
redshiftParameters_database :: Lens.Lens' RedshiftParameters Prelude.Text
redshiftParameters_database = Lens.lens (\RedshiftParameters' {database} -> database) (\s@RedshiftParameters' {} a -> s {database = a} :: RedshiftParameters)

instance Data.FromJSON RedshiftParameters where
  parseJSON =
    Data.withObject
      "RedshiftParameters"
      ( \x ->
          RedshiftParameters'
            Prelude.<$> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "Host")
            Prelude.<*> (x Data..:? "ClusterId")
            Prelude.<*> (x Data..: "Database")
      )

instance Prelude.Hashable RedshiftParameters where
  hashWithSalt _salt RedshiftParameters' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` database

instance Prelude.NFData RedshiftParameters where
  rnf RedshiftParameters' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf host
      `Prelude.seq` Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf database

instance Data.ToJSON RedshiftParameters where
  toJSON RedshiftParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Port" Data..=) Prelude.<$> port,
            ("Host" Data..=) Prelude.<$> host,
            ("ClusterId" Data..=) Prelude.<$> clusterId,
            Prelude.Just ("Database" Data..= database)
          ]
      )
