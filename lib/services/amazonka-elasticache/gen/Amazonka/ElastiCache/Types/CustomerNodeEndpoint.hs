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
-- Module      : Amazonka.ElastiCache.Types.CustomerNodeEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CustomerNodeEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The endpoint from which data should be migrated.
--
-- /See:/ 'newCustomerNodeEndpoint' smart constructor.
data CustomerNodeEndpoint = CustomerNodeEndpoint'
  { -- | The address of the node endpoint
    address :: Prelude.Maybe Prelude.Text,
    -- | The port of the node endpoint
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { address = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The address of the node endpoint
customerNodeEndpoint_address :: Lens.Lens' CustomerNodeEndpoint (Prelude.Maybe Prelude.Text)
customerNodeEndpoint_address = Lens.lens (\CustomerNodeEndpoint' {address} -> address) (\s@CustomerNodeEndpoint' {} a -> s {address = a} :: CustomerNodeEndpoint)

-- | The port of the node endpoint
customerNodeEndpoint_port :: Lens.Lens' CustomerNodeEndpoint (Prelude.Maybe Prelude.Int)
customerNodeEndpoint_port = Lens.lens (\CustomerNodeEndpoint' {port} -> port) (\s@CustomerNodeEndpoint' {} a -> s {port = a} :: CustomerNodeEndpoint)

instance Prelude.Hashable CustomerNodeEndpoint where
  hashWithSalt _salt CustomerNodeEndpoint' {..} =
    _salt `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` port

instance Prelude.NFData CustomerNodeEndpoint where
  rnf CustomerNodeEndpoint' {..} =
    Prelude.rnf address `Prelude.seq` Prelude.rnf port

instance Data.ToQuery CustomerNodeEndpoint where
  toQuery CustomerNodeEndpoint' {..} =
    Prelude.mconcat
      ["Address" Data.=: address, "Port" Data.=: port]
