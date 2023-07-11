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
-- Module      : Amazonka.DirectConnect.Types.Connections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.Connections where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.Connection
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newConnections' smart constructor.
data Connections = Connections'
  { -- | The connections.
    connections :: Prelude.Maybe [Connection]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Connections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connections', 'connections_connections' - The connections.
newConnections ::
  Connections
newConnections =
  Connections' {connections = Prelude.Nothing}

-- | The connections.
connections_connections :: Lens.Lens' Connections (Prelude.Maybe [Connection])
connections_connections = Lens.lens (\Connections' {connections} -> connections) (\s@Connections' {} a -> s {connections = a} :: Connections) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Connections where
  parseJSON =
    Data.withObject
      "Connections"
      ( \x ->
          Connections'
            Prelude.<$> (x Data..:? "connections" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Connections where
  hashWithSalt _salt Connections' {..} =
    _salt `Prelude.hashWithSalt` connections

instance Prelude.NFData Connections where
  rnf Connections' {..} = Prelude.rnf connections
