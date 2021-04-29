{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectConnect.Types.Connection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newConnections' smart constructor.
data Connections = Connections'
  { -- | The connections.
    connections :: Prelude.Maybe [Connection]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
connections_connections = Lens.lens (\Connections' {connections} -> connections) (\s@Connections' {} a -> s {connections = a} :: Connections) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Connections where
  parseJSON =
    Prelude.withObject
      "Connections"
      ( \x ->
          Connections'
            Prelude.<$> ( x Prelude..:? "connections"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Connections

instance Prelude.NFData Connections
