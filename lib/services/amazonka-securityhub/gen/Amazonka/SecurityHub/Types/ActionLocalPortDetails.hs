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
-- Module      : Amazonka.SecurityHub.Types.ActionLocalPortDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ActionLocalPortDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | For @NetworkConnectionAction@ and @PortProbeDetails@, @LocalPortDetails@
-- provides information about the local port that was involved in the
-- action.
--
-- /See:/ 'newActionLocalPortDetails' smart constructor.
data ActionLocalPortDetails = ActionLocalPortDetails'
  { -- | The port name of the local connection.
    portName :: Prelude.Maybe Prelude.Text,
    -- | The number of the port.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionLocalPortDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portName', 'actionLocalPortDetails_portName' - The port name of the local connection.
--
-- 'port', 'actionLocalPortDetails_port' - The number of the port.
newActionLocalPortDetails ::
  ActionLocalPortDetails
newActionLocalPortDetails =
  ActionLocalPortDetails'
    { portName = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The port name of the local connection.
actionLocalPortDetails_portName :: Lens.Lens' ActionLocalPortDetails (Prelude.Maybe Prelude.Text)
actionLocalPortDetails_portName = Lens.lens (\ActionLocalPortDetails' {portName} -> portName) (\s@ActionLocalPortDetails' {} a -> s {portName = a} :: ActionLocalPortDetails)

-- | The number of the port.
actionLocalPortDetails_port :: Lens.Lens' ActionLocalPortDetails (Prelude.Maybe Prelude.Int)
actionLocalPortDetails_port = Lens.lens (\ActionLocalPortDetails' {port} -> port) (\s@ActionLocalPortDetails' {} a -> s {port = a} :: ActionLocalPortDetails)

instance Core.FromJSON ActionLocalPortDetails where
  parseJSON =
    Core.withObject
      "ActionLocalPortDetails"
      ( \x ->
          ActionLocalPortDetails'
            Prelude.<$> (x Core..:? "PortName")
            Prelude.<*> (x Core..:? "Port")
      )

instance Prelude.Hashable ActionLocalPortDetails where
  hashWithSalt salt' ActionLocalPortDetails' {..} =
    salt' `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` portName

instance Prelude.NFData ActionLocalPortDetails where
  rnf ActionLocalPortDetails' {..} =
    Prelude.rnf portName `Prelude.seq` Prelude.rnf port

instance Core.ToJSON ActionLocalPortDetails where
  toJSON ActionLocalPortDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PortName" Core..=) Prelude.<$> portName,
            ("Port" Core..=) Prelude.<$> port
          ]
      )
