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
-- Module      : Amazonka.GamesParks.Types.Connection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.Connection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a WebSocket connection.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The date and time when the connection was created.
    created :: Prelude.Maybe Data.ISO8601,
    -- | The identifier used to indicate a specific WebSocket connection.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Connection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'created', 'connection_created' - The date and time when the connection was created.
--
-- 'id', 'connection_id' - The identifier used to indicate a specific WebSocket connection.
newConnection ::
  Connection
newConnection =
  Connection'
    { created = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The date and time when the connection was created.
connection_created :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_created = Lens.lens (\Connection' {created} -> created) (\s@Connection' {} a -> s {created = a} :: Connection) Prelude.. Lens.mapping Data._Time

-- | The identifier used to indicate a specific WebSocket connection.
connection_id :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_id = Lens.lens (\Connection' {id} -> id) (\s@Connection' {} a -> s {id = a} :: Connection)

instance Data.FromJSON Connection where
  parseJSON =
    Data.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Data..:? "Created")
            Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable Connection where
  hashWithSalt _salt Connection' {..} =
    _salt
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` id

instance Prelude.NFData Connection where
  rnf Connection' {..} =
    Prelude.rnf created `Prelude.seq` Prelude.rnf id
