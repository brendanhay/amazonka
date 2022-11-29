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
import Amazonka.NetworkManager.Types.ConnectionStatus
import Amazonka.NetworkManager.Types.ConnectionType
import qualified Amazonka.Prelude as Prelude

-- | Describes connection health.
--
-- /See:/ 'newConnectionHealth' smart constructor.
data ConnectionHealth = ConnectionHealth'
  { -- | The connection type.
    type' :: Prelude.Maybe ConnectionType,
    -- | The time the status was last updated.
    timestamp :: Prelude.Maybe Core.POSIX,
    -- | The connection status.
    status :: Prelude.Maybe ConnectionStatus
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
-- 'type'', 'connectionHealth_type' - The connection type.
--
-- 'timestamp', 'connectionHealth_timestamp' - The time the status was last updated.
--
-- 'status', 'connectionHealth_status' - The connection status.
newConnectionHealth ::
  ConnectionHealth
newConnectionHealth =
  ConnectionHealth'
    { type' = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The connection type.
connectionHealth_type :: Lens.Lens' ConnectionHealth (Prelude.Maybe ConnectionType)
connectionHealth_type = Lens.lens (\ConnectionHealth' {type'} -> type') (\s@ConnectionHealth' {} a -> s {type' = a} :: ConnectionHealth)

-- | The time the status was last updated.
connectionHealth_timestamp :: Lens.Lens' ConnectionHealth (Prelude.Maybe Prelude.UTCTime)
connectionHealth_timestamp = Lens.lens (\ConnectionHealth' {timestamp} -> timestamp) (\s@ConnectionHealth' {} a -> s {timestamp = a} :: ConnectionHealth) Prelude.. Lens.mapping Core._Time

-- | The connection status.
connectionHealth_status :: Lens.Lens' ConnectionHealth (Prelude.Maybe ConnectionStatus)
connectionHealth_status = Lens.lens (\ConnectionHealth' {status} -> status) (\s@ConnectionHealth' {} a -> s {status = a} :: ConnectionHealth)

instance Core.FromJSON ConnectionHealth where
  parseJSON =
    Core.withObject
      "ConnectionHealth"
      ( \x ->
          ConnectionHealth'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Timestamp")
            Prelude.<*> (x Core..:? "Status")
      )

instance Prelude.Hashable ConnectionHealth where
  hashWithSalt _salt ConnectionHealth' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` status

instance Prelude.NFData ConnectionHealth where
  rnf ConnectionHealth' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf status
