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
-- Module      : Network.AWS.GuardDuty.Types.LocalPortDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.LocalPortDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the port for the local connection.
--
-- /See:/ 'newLocalPortDetails' smart constructor.
data LocalPortDetails = LocalPortDetails'
  { -- | The port name of the local connection.
    portName :: Core.Maybe Core.Text,
    -- | The port number of the local connection.
    port :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { portName = Core.Nothing,
      port = Core.Nothing
    }

-- | The port name of the local connection.
localPortDetails_portName :: Lens.Lens' LocalPortDetails (Core.Maybe Core.Text)
localPortDetails_portName = Lens.lens (\LocalPortDetails' {portName} -> portName) (\s@LocalPortDetails' {} a -> s {portName = a} :: LocalPortDetails)

-- | The port number of the local connection.
localPortDetails_port :: Lens.Lens' LocalPortDetails (Core.Maybe Core.Int)
localPortDetails_port = Lens.lens (\LocalPortDetails' {port} -> port) (\s@LocalPortDetails' {} a -> s {port = a} :: LocalPortDetails)

instance Core.FromJSON LocalPortDetails where
  parseJSON =
    Core.withObject
      "LocalPortDetails"
      ( \x ->
          LocalPortDetails'
            Core.<$> (x Core..:? "portName") Core.<*> (x Core..:? "port")
      )

instance Core.Hashable LocalPortDetails

instance Core.NFData LocalPortDetails
