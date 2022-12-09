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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TcpRoute where

import Amazonka.AppMesh.Types.TcpRouteAction
import Amazonka.AppMesh.Types.TcpRouteMatch
import Amazonka.AppMesh.Types.TcpTimeout
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a TCP route type.
--
-- /See:/ 'newTcpRoute' smart constructor.
data TcpRoute = TcpRoute'
  { -- | An object that represents the criteria for determining a request match.
    match :: Prelude.Maybe TcpRouteMatch,
    -- | An object that represents types of timeouts.
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
-- 'match', 'tcpRoute_match' - An object that represents the criteria for determining a request match.
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
    { match = Prelude.Nothing,
      timeout = Prelude.Nothing,
      action = pAction_
    }

-- | An object that represents the criteria for determining a request match.
tcpRoute_match :: Lens.Lens' TcpRoute (Prelude.Maybe TcpRouteMatch)
tcpRoute_match = Lens.lens (\TcpRoute' {match} -> match) (\s@TcpRoute' {} a -> s {match = a} :: TcpRoute)

-- | An object that represents types of timeouts.
tcpRoute_timeout :: Lens.Lens' TcpRoute (Prelude.Maybe TcpTimeout)
tcpRoute_timeout = Lens.lens (\TcpRoute' {timeout} -> timeout) (\s@TcpRoute' {} a -> s {timeout = a} :: TcpRoute)

-- | The action to take if a match is determined.
tcpRoute_action :: Lens.Lens' TcpRoute TcpRouteAction
tcpRoute_action = Lens.lens (\TcpRoute' {action} -> action) (\s@TcpRoute' {} a -> s {action = a} :: TcpRoute)

instance Data.FromJSON TcpRoute where
  parseJSON =
    Data.withObject
      "TcpRoute"
      ( \x ->
          TcpRoute'
            Prelude.<$> (x Data..:? "match")
            Prelude.<*> (x Data..:? "timeout")
            Prelude.<*> (x Data..: "action")
      )

instance Prelude.Hashable TcpRoute where
  hashWithSalt _salt TcpRoute' {..} =
    _salt `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` action

instance Prelude.NFData TcpRoute where
  rnf TcpRoute' {..} =
    Prelude.rnf match
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf action

instance Data.ToJSON TcpRoute where
  toJSON TcpRoute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("match" Data..=) Prelude.<$> match,
            ("timeout" Data..=) Prelude.<$> timeout,
            Prelude.Just ("action" Data..= action)
          ]
      )
