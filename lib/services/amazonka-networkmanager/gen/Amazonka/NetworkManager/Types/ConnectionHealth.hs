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
-- Module      : Amazonka.NetworkManager.Types.ConnectionHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectionHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.ConnectionStatus
import Amazonka.NetworkManager.Types.ConnectionType
import qualified Amazonka.Prelude as Prelude

-- | Describes connection health.
--
-- /See:/ 'newConnectionHealth' smart constructor.
data ConnectionHealth = ConnectionHealth'
  { -- | The connection status.
    status :: Prelude.Maybe ConnectionStatus,
    -- | The time the status was last updated.
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | The connection type.
    type' :: Prelude.Maybe ConnectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'connectionHealth_status' - The connection status.
--
-- 'timestamp', 'connectionHealth_timestamp' - The time the status was last updated.
--
-- 'type'', 'connectionHealth_type' - The connection type.
newConnectionHealth ::
  ConnectionHealth
newConnectionHealth =
  ConnectionHealth'
    { status = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The connection status.
connectionHealth_status :: Lens.Lens' ConnectionHealth (Prelude.Maybe ConnectionStatus)
connectionHealth_status = Lens.lens (\ConnectionHealth' {status} -> status) (\s@ConnectionHealth' {} a -> s {status = a} :: ConnectionHealth)

-- | The time the status was last updated.
connectionHealth_timestamp :: Lens.Lens' ConnectionHealth (Prelude.Maybe Prelude.UTCTime)
connectionHealth_timestamp = Lens.lens (\ConnectionHealth' {timestamp} -> timestamp) (\s@ConnectionHealth' {} a -> s {timestamp = a} :: ConnectionHealth) Prelude.. Lens.mapping Data._Time

-- | The connection type.
connectionHealth_type :: Lens.Lens' ConnectionHealth (Prelude.Maybe ConnectionType)
connectionHealth_type = Lens.lens (\ConnectionHealth' {type'} -> type') (\s@ConnectionHealth' {} a -> s {type' = a} :: ConnectionHealth)

instance Data.FromJSON ConnectionHealth where
  parseJSON =
    Data.withObject
      "ConnectionHealth"
      ( \x ->
          ConnectionHealth'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Timestamp")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable ConnectionHealth where
  hashWithSalt _salt ConnectionHealth' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ConnectionHealth where
  rnf ConnectionHealth' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf type'
