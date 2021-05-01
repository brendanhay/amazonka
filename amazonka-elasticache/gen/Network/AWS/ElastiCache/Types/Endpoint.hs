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
-- Module      : Network.AWS.ElastiCache.Types.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Endpoint where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the information required for client programs to connect to a
-- cache node.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | The DNS hostname of the cache node.
    address :: Prelude.Maybe Prelude.Text,
    -- | The port number that the cache engine is listening on.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'endpoint_address' - The DNS hostname of the cache node.
--
-- 'port', 'endpoint_port' - The port number that the cache engine is listening on.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { address = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The DNS hostname of the cache node.
endpoint_address :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_address = Lens.lens (\Endpoint' {address} -> address) (\s@Endpoint' {} a -> s {address = a} :: Endpoint)

-- | The port number that the cache engine is listening on.
endpoint_port :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Int)
endpoint_port = Lens.lens (\Endpoint' {port} -> port) (\s@Endpoint' {} a -> s {port = a} :: Endpoint)

instance Prelude.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Prelude.<$> (x Prelude..@? "Address")
      Prelude.<*> (x Prelude..@? "Port")

instance Prelude.Hashable Endpoint

instance Prelude.NFData Endpoint
