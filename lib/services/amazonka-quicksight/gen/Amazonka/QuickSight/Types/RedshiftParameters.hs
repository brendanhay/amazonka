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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RedshiftParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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

instance Core.FromJSON RedshiftParameters where
  parseJSON =
    Core.withObject
      "RedshiftParameters"
      ( \x ->
          RedshiftParameters'
            Prelude.<$> (x Core..:? "ClusterId")
            Prelude.<*> (x Core..:? "Host")
            Prelude.<*> (x Core..:? "Port")
            Prelude.<*> (x Core..: "Database")
      )

instance Prelude.Hashable RedshiftParameters where
  hashWithSalt salt' RedshiftParameters' {..} =
    salt' `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData RedshiftParameters where
  rnf RedshiftParameters' {..} =
    Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf host

instance Core.ToJSON RedshiftParameters where
  toJSON RedshiftParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClusterId" Core..=) Prelude.<$> clusterId,
            ("Host" Core..=) Prelude.<$> host,
            ("Port" Core..=) Prelude.<$> port,
            Prelude.Just ("Database" Core..= database)
          ]
      )
