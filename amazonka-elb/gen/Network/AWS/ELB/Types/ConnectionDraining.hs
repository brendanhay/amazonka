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
-- Module      : Network.AWS.ELB.Types.ConnectionDraining
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ConnectionDraining where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the @ConnectionDraining@ attribute.
--
-- /See:/ 'newConnectionDraining' smart constructor.
data ConnectionDraining = ConnectionDraining'
  { -- | The maximum time, in seconds, to keep the existing connections open
    -- before deregistering the instances.
    timeout :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether connection draining is enabled for the load balancer.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConnectionDraining' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeout', 'connectionDraining_timeout' - The maximum time, in seconds, to keep the existing connections open
-- before deregistering the instances.
--
-- 'enabled', 'connectionDraining_enabled' - Specifies whether connection draining is enabled for the load balancer.
newConnectionDraining ::
  -- | 'enabled'
  Prelude.Bool ->
  ConnectionDraining
newConnectionDraining pEnabled_ =
  ConnectionDraining'
    { timeout = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | The maximum time, in seconds, to keep the existing connections open
-- before deregistering the instances.
connectionDraining_timeout :: Lens.Lens' ConnectionDraining (Prelude.Maybe Prelude.Int)
connectionDraining_timeout = Lens.lens (\ConnectionDraining' {timeout} -> timeout) (\s@ConnectionDraining' {} a -> s {timeout = a} :: ConnectionDraining)

-- | Specifies whether connection draining is enabled for the load balancer.
connectionDraining_enabled :: Lens.Lens' ConnectionDraining Prelude.Bool
connectionDraining_enabled = Lens.lens (\ConnectionDraining' {enabled} -> enabled) (\s@ConnectionDraining' {} a -> s {enabled = a} :: ConnectionDraining)

instance Prelude.FromXML ConnectionDraining where
  parseXML x =
    ConnectionDraining'
      Prelude.<$> (x Prelude..@? "Timeout")
      Prelude.<*> (x Prelude..@ "Enabled")

instance Prelude.Hashable ConnectionDraining

instance Prelude.NFData ConnectionDraining

instance Prelude.ToQuery ConnectionDraining where
  toQuery ConnectionDraining' {..} =
    Prelude.mconcat
      [ "Timeout" Prelude.=: timeout,
        "Enabled" Prelude.=: enabled
      ]
