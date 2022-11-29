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
-- Module      : Amazonka.SecurityHub.Types.ActionRemotePortDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ActionRemotePortDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the remote port that was involved in an
-- attempted network connection.
--
-- /See:/ 'newActionRemotePortDetails' smart constructor.
data ActionRemotePortDetails = ActionRemotePortDetails'
  { -- | The number of the port.
    port :: Prelude.Maybe Prelude.Int,
    -- | The port name of the remote connection.
    portName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionRemotePortDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'actionRemotePortDetails_port' - The number of the port.
--
-- 'portName', 'actionRemotePortDetails_portName' - The port name of the remote connection.
newActionRemotePortDetails ::
  ActionRemotePortDetails
newActionRemotePortDetails =
  ActionRemotePortDetails'
    { port = Prelude.Nothing,
      portName = Prelude.Nothing
    }

-- | The number of the port.
actionRemotePortDetails_port :: Lens.Lens' ActionRemotePortDetails (Prelude.Maybe Prelude.Int)
actionRemotePortDetails_port = Lens.lens (\ActionRemotePortDetails' {port} -> port) (\s@ActionRemotePortDetails' {} a -> s {port = a} :: ActionRemotePortDetails)

-- | The port name of the remote connection.
actionRemotePortDetails_portName :: Lens.Lens' ActionRemotePortDetails (Prelude.Maybe Prelude.Text)
actionRemotePortDetails_portName = Lens.lens (\ActionRemotePortDetails' {portName} -> portName) (\s@ActionRemotePortDetails' {} a -> s {portName = a} :: ActionRemotePortDetails)

instance Core.FromJSON ActionRemotePortDetails where
  parseJSON =
    Core.withObject
      "ActionRemotePortDetails"
      ( \x ->
          ActionRemotePortDetails'
            Prelude.<$> (x Core..:? "Port")
            Prelude.<*> (x Core..:? "PortName")
      )

instance Prelude.Hashable ActionRemotePortDetails where
  hashWithSalt _salt ActionRemotePortDetails' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` portName

instance Prelude.NFData ActionRemotePortDetails where
  rnf ActionRemotePortDetails' {..} =
    Prelude.rnf port `Prelude.seq` Prelude.rnf portName

instance Core.ToJSON ActionRemotePortDetails where
  toJSON ActionRemotePortDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Port" Core..=) Prelude.<$> port,
            ("PortName" Core..=) Prelude.<$> portName
          ]
      )
