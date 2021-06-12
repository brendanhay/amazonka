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
-- Module      : Network.AWS.DirectConnect.Types.Connections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Connections where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.Connection
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newConnections' smart constructor.
data Connections = Connections'
  { -- | The connections.
    connections :: Core.Maybe [Connection]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Connections' {connections = Core.Nothing}

-- | The connections.
connections_connections :: Lens.Lens' Connections (Core.Maybe [Connection])
connections_connections = Lens.lens (\Connections' {connections} -> connections) (\s@Connections' {} a -> s {connections = a} :: Connections) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Connections where
  parseJSON =
    Core.withObject
      "Connections"
      ( \x ->
          Connections'
            Core.<$> (x Core..:? "connections" Core..!= Core.mempty)
      )

instance Core.Hashable Connections

instance Core.NFData Connections
