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
-- Module      : Amazonka.GuardDuty.Types.LocalPortDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.LocalPortDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the port for the local connection.
--
-- /See:/ 'newLocalPortDetails' smart constructor.
data LocalPortDetails = LocalPortDetails'
  { -- | The port name of the local connection.
    portName :: Prelude.Maybe Prelude.Text,
    -- | The port number of the local connection.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalPortDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portName', 'localPortDetails_portName' - The port name of the local connection.
--
-- 'port', 'localPortDetails_port' - The port number of the local connection.
newLocalPortDetails ::
  LocalPortDetails
newLocalPortDetails =
  LocalPortDetails'
    { portName = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The port name of the local connection.
localPortDetails_portName :: Lens.Lens' LocalPortDetails (Prelude.Maybe Prelude.Text)
localPortDetails_portName = Lens.lens (\LocalPortDetails' {portName} -> portName) (\s@LocalPortDetails' {} a -> s {portName = a} :: LocalPortDetails)

-- | The port number of the local connection.
localPortDetails_port :: Lens.Lens' LocalPortDetails (Prelude.Maybe Prelude.Int)
localPortDetails_port = Lens.lens (\LocalPortDetails' {port} -> port) (\s@LocalPortDetails' {} a -> s {port = a} :: LocalPortDetails)

instance Core.FromJSON LocalPortDetails where
  parseJSON =
    Core.withObject
      "LocalPortDetails"
      ( \x ->
          LocalPortDetails'
            Prelude.<$> (x Core..:? "portName")
            Prelude.<*> (x Core..:? "port")
      )

instance Prelude.Hashable LocalPortDetails where
  hashWithSalt salt' LocalPortDetails' {..} =
    salt' `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` portName

instance Prelude.NFData LocalPortDetails where
  rnf LocalPortDetails' {..} =
    Prelude.rnf portName `Prelude.seq` Prelude.rnf port
