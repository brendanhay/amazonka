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
-- Module      : Amazonka.AppMesh.Types.TcpRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TcpRoute where

import Amazonka.AppMesh.Types.TcpRouteAction
import Amazonka.AppMesh.Types.TcpTimeout
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a TCP route type.
--
-- /See:/ 'newTcpRoute' smart constructor.
data TcpRoute = TcpRoute'
  { -- | An object that represents types of timeouts.
    timeout :: Prelude.Maybe TcpTimeout,
    -- | The action to take if a match is determined.
    action :: TcpRouteAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TcpRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeout', 'tcpRoute_timeout' - An object that represents types of timeouts.
--
-- 'action', 'tcpRoute_action' - The action to take if a match is determined.
newTcpRoute ::
  -- | 'action'
  TcpRouteAction ->
  TcpRoute
newTcpRoute pAction_ =
  TcpRoute'
    { timeout = Prelude.Nothing,
      action = pAction_
    }

-- | An object that represents types of timeouts.
tcpRoute_timeout :: Lens.Lens' TcpRoute (Prelude.Maybe TcpTimeout)
tcpRoute_timeout = Lens.lens (\TcpRoute' {timeout} -> timeout) (\s@TcpRoute' {} a -> s {timeout = a} :: TcpRoute)

-- | The action to take if a match is determined.
tcpRoute_action :: Lens.Lens' TcpRoute TcpRouteAction
tcpRoute_action = Lens.lens (\TcpRoute' {action} -> action) (\s@TcpRoute' {} a -> s {action = a} :: TcpRoute)

instance Core.FromJSON TcpRoute where
  parseJSON =
    Core.withObject
      "TcpRoute"
      ( \x ->
          TcpRoute'
            Prelude.<$> (x Core..:? "timeout")
            Prelude.<*> (x Core..: "action")
      )

instance Prelude.Hashable TcpRoute

instance Prelude.NFData TcpRoute

instance Core.ToJSON TcpRoute where
  toJSON TcpRoute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("timeout" Core..=) Prelude.<$> timeout,
            Prelude.Just ("action" Core..= action)
          ]
      )
