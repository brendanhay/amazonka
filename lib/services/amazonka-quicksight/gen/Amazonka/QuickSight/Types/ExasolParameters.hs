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
-- Module      : Amazonka.QuickSight.Types.ExasolParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ExasolParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The required parameters for connecting to an Exasol data source.
--
-- /See:/ 'newExasolParameters' smart constructor.
data ExasolParameters = ExasolParameters'
  { -- | The hostname or IP address of the Exasol data source.
    host :: Prelude.Text,
    -- | The port for the Exasol data source.
    port :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExasolParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'exasolParameters_host' - The hostname or IP address of the Exasol data source.
--
-- 'port', 'exasolParameters_port' - The port for the Exasol data source.
newExasolParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  ExasolParameters
newExasolParameters pHost_ pPort_ =
  ExasolParameters' {host = pHost_, port = pPort_}

-- | The hostname or IP address of the Exasol data source.
exasolParameters_host :: Lens.Lens' ExasolParameters Prelude.Text
exasolParameters_host = Lens.lens (\ExasolParameters' {host} -> host) (\s@ExasolParameters' {} a -> s {host = a} :: ExasolParameters)

-- | The port for the Exasol data source.
exasolParameters_port :: Lens.Lens' ExasolParameters Prelude.Natural
exasolParameters_port = Lens.lens (\ExasolParameters' {port} -> port) (\s@ExasolParameters' {} a -> s {port = a} :: ExasolParameters)

instance Data.FromJSON ExasolParameters where
  parseJSON =
    Data.withObject
      "ExasolParameters"
      ( \x ->
          ExasolParameters'
            Prelude.<$> (x Data..: "Host") Prelude.<*> (x Data..: "Port")
      )

instance Prelude.Hashable ExasolParameters where
  hashWithSalt _salt ExasolParameters' {..} =
    _salt `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port

instance Prelude.NFData ExasolParameters where
  rnf ExasolParameters' {..} =
    Prelude.rnf host `Prelude.seq` Prelude.rnf port

instance Data.ToJSON ExasolParameters where
  toJSON ExasolParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Port" Data..= port)
          ]
      )
