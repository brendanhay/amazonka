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
-- Module      : Network.AWS.ElastiCache.Types.CustomerNodeEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CustomerNodeEndpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The endpoint from which data should be migrated.
--
-- /See:/ 'newCustomerNodeEndpoint' smart constructor.
data CustomerNodeEndpoint = CustomerNodeEndpoint'
  { -- | The address of the node endpoint
    address :: Core.Maybe Core.Text,
    -- | The port of the node endpoint
    port :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CustomerNodeEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'customerNodeEndpoint_address' - The address of the node endpoint
--
-- 'port', 'customerNodeEndpoint_port' - The port of the node endpoint
newCustomerNodeEndpoint ::
  CustomerNodeEndpoint
newCustomerNodeEndpoint =
  CustomerNodeEndpoint'
    { address = Core.Nothing,
      port = Core.Nothing
    }

-- | The address of the node endpoint
customerNodeEndpoint_address :: Lens.Lens' CustomerNodeEndpoint (Core.Maybe Core.Text)
customerNodeEndpoint_address = Lens.lens (\CustomerNodeEndpoint' {address} -> address) (\s@CustomerNodeEndpoint' {} a -> s {address = a} :: CustomerNodeEndpoint)

-- | The port of the node endpoint
customerNodeEndpoint_port :: Lens.Lens' CustomerNodeEndpoint (Core.Maybe Core.Int)
customerNodeEndpoint_port = Lens.lens (\CustomerNodeEndpoint' {port} -> port) (\s@CustomerNodeEndpoint' {} a -> s {port = a} :: CustomerNodeEndpoint)

instance Core.Hashable CustomerNodeEndpoint

instance Core.NFData CustomerNodeEndpoint

instance Core.ToQuery CustomerNodeEndpoint where
  toQuery CustomerNodeEndpoint' {..} =
    Core.mconcat
      ["Address" Core.=: address, "Port" Core.=: port]
