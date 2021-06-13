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
-- Module      : Network.AWS.ELB.Types.ConnectionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ConnectionSettings where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the @ConnectionSettings@ attribute.
--
-- /See:/ 'newConnectionSettings' smart constructor.
data ConnectionSettings = ConnectionSettings'
  { -- | The time, in seconds, that the connection is allowed to be idle (no data
    -- has been sent over the connection) before it is closed by the load
    -- balancer.
    idleTimeout :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idleTimeout', 'connectionSettings_idleTimeout' - The time, in seconds, that the connection is allowed to be idle (no data
-- has been sent over the connection) before it is closed by the load
-- balancer.
newConnectionSettings ::
  -- | 'idleTimeout'
  Prelude.Natural ->
  ConnectionSettings
newConnectionSettings pIdleTimeout_ =
  ConnectionSettings' {idleTimeout = pIdleTimeout_}

-- | The time, in seconds, that the connection is allowed to be idle (no data
-- has been sent over the connection) before it is closed by the load
-- balancer.
connectionSettings_idleTimeout :: Lens.Lens' ConnectionSettings Prelude.Natural
connectionSettings_idleTimeout = Lens.lens (\ConnectionSettings' {idleTimeout} -> idleTimeout) (\s@ConnectionSettings' {} a -> s {idleTimeout = a} :: ConnectionSettings)

instance Core.FromXML ConnectionSettings where
  parseXML x =
    ConnectionSettings'
      Prelude.<$> (x Core..@ "IdleTimeout")

instance Prelude.Hashable ConnectionSettings

instance Prelude.NFData ConnectionSettings

instance Core.ToQuery ConnectionSettings where
  toQuery ConnectionSettings' {..} =
    Prelude.mconcat ["IdleTimeout" Core.=: idleTimeout]
