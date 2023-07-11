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
-- Module      : Amazonka.AppMesh.Types.TcpRouteMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TcpRouteMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the TCP route to match.
--
-- /See:/ 'newTcpRouteMatch' smart constructor.
data TcpRouteMatch = TcpRouteMatch'
  { -- | The port number to match on.
    port :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TcpRouteMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'tcpRouteMatch_port' - The port number to match on.
newTcpRouteMatch ::
  TcpRouteMatch
newTcpRouteMatch =
  TcpRouteMatch' {port = Prelude.Nothing}

-- | The port number to match on.
tcpRouteMatch_port :: Lens.Lens' TcpRouteMatch (Prelude.Maybe Prelude.Natural)
tcpRouteMatch_port = Lens.lens (\TcpRouteMatch' {port} -> port) (\s@TcpRouteMatch' {} a -> s {port = a} :: TcpRouteMatch)

instance Data.FromJSON TcpRouteMatch where
  parseJSON =
    Data.withObject
      "TcpRouteMatch"
      ( \x ->
          TcpRouteMatch' Prelude.<$> (x Data..:? "port")
      )

instance Prelude.Hashable TcpRouteMatch where
  hashWithSalt _salt TcpRouteMatch' {..} =
    _salt `Prelude.hashWithSalt` port

instance Prelude.NFData TcpRouteMatch where
  rnf TcpRouteMatch' {..} = Prelude.rnf port

instance Data.ToJSON TcpRouteMatch where
  toJSON TcpRouteMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [("port" Data..=) Prelude.<$> port]
      )
