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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.LocalPortDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the port for the local connection.
--
-- /See:/ 'newLocalPortDetails' smart constructor.
data LocalPortDetails = LocalPortDetails'
  { -- | The port number of the local connection.
    port :: Prelude.Maybe Prelude.Int,
    -- | The port name of the local connection.
    portName :: Prelude.Maybe Prelude.Text
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
-- 'port', 'localPortDetails_port' - The port number of the local connection.
--
-- 'portName', 'localPortDetails_portName' - The port name of the local connection.
newLocalPortDetails ::
  LocalPortDetails
newLocalPortDetails =
  LocalPortDetails'
    { port = Prelude.Nothing,
      portName = Prelude.Nothing
    }

-- | The port number of the local connection.
localPortDetails_port :: Lens.Lens' LocalPortDetails (Prelude.Maybe Prelude.Int)
localPortDetails_port = Lens.lens (\LocalPortDetails' {port} -> port) (\s@LocalPortDetails' {} a -> s {port = a} :: LocalPortDetails)

-- | The port name of the local connection.
localPortDetails_portName :: Lens.Lens' LocalPortDetails (Prelude.Maybe Prelude.Text)
localPortDetails_portName = Lens.lens (\LocalPortDetails' {portName} -> portName) (\s@LocalPortDetails' {} a -> s {portName = a} :: LocalPortDetails)

instance Data.FromJSON LocalPortDetails where
  parseJSON =
    Data.withObject
      "LocalPortDetails"
      ( \x ->
          LocalPortDetails'
            Prelude.<$> (x Data..:? "port")
            Prelude.<*> (x Data..:? "portName")
      )

instance Prelude.Hashable LocalPortDetails where
  hashWithSalt _salt LocalPortDetails' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` portName

instance Prelude.NFData LocalPortDetails where
  rnf LocalPortDetails' {..} =
    Prelude.rnf port `Prelude.seq` Prelude.rnf portName
