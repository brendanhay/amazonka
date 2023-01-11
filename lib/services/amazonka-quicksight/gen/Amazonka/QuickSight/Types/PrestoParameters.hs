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
-- Module      : Amazonka.QuickSight.Types.PrestoParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PrestoParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for Presto.
--
-- /See:/ 'newPrestoParameters' smart constructor.
data PrestoParameters = PrestoParameters'
  { -- | Host.
    host :: Prelude.Text,
    -- | Port.
    port :: Prelude.Natural,
    -- | Catalog.
    catalog :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrestoParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'prestoParameters_host' - Host.
--
-- 'port', 'prestoParameters_port' - Port.
--
-- 'catalog', 'prestoParameters_catalog' - Catalog.
newPrestoParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  -- | 'catalog'
  Prelude.Text ->
  PrestoParameters
newPrestoParameters pHost_ pPort_ pCatalog_ =
  PrestoParameters'
    { host = pHost_,
      port = pPort_,
      catalog = pCatalog_
    }

-- | Host.
prestoParameters_host :: Lens.Lens' PrestoParameters Prelude.Text
prestoParameters_host = Lens.lens (\PrestoParameters' {host} -> host) (\s@PrestoParameters' {} a -> s {host = a} :: PrestoParameters)

-- | Port.
prestoParameters_port :: Lens.Lens' PrestoParameters Prelude.Natural
prestoParameters_port = Lens.lens (\PrestoParameters' {port} -> port) (\s@PrestoParameters' {} a -> s {port = a} :: PrestoParameters)

-- | Catalog.
prestoParameters_catalog :: Lens.Lens' PrestoParameters Prelude.Text
prestoParameters_catalog = Lens.lens (\PrestoParameters' {catalog} -> catalog) (\s@PrestoParameters' {} a -> s {catalog = a} :: PrestoParameters)

instance Data.FromJSON PrestoParameters where
  parseJSON =
    Data.withObject
      "PrestoParameters"
      ( \x ->
          PrestoParameters'
            Prelude.<$> (x Data..: "Host")
            Prelude.<*> (x Data..: "Port")
            Prelude.<*> (x Data..: "Catalog")
      )

instance Prelude.Hashable PrestoParameters where
  hashWithSalt _salt PrestoParameters' {..} =
    _salt `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` catalog

instance Prelude.NFData PrestoParameters where
  rnf PrestoParameters' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf catalog

instance Data.ToJSON PrestoParameters where
  toJSON PrestoParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Port" Data..= port),
            Prelude.Just ("Catalog" Data..= catalog)
          ]
      )
