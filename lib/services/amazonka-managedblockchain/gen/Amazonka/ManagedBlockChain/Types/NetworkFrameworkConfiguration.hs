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
-- Module      : Amazonka.ManagedBlockChain.Types.NetworkFrameworkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NetworkFrameworkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.NetworkFabricConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration properties relevant to the network for the blockchain
-- framework that the network uses.
--
-- /See:/ 'newNetworkFrameworkConfiguration' smart constructor.
data NetworkFrameworkConfiguration = NetworkFrameworkConfiguration'
  { -- | Hyperledger Fabric configuration properties for a Managed Blockchain
    -- network that uses Hyperledger Fabric.
    fabric :: Prelude.Maybe NetworkFabricConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFrameworkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fabric', 'networkFrameworkConfiguration_fabric' - Hyperledger Fabric configuration properties for a Managed Blockchain
-- network that uses Hyperledger Fabric.
newNetworkFrameworkConfiguration ::
  NetworkFrameworkConfiguration
newNetworkFrameworkConfiguration =
  NetworkFrameworkConfiguration'
    { fabric =
        Prelude.Nothing
    }

-- | Hyperledger Fabric configuration properties for a Managed Blockchain
-- network that uses Hyperledger Fabric.
networkFrameworkConfiguration_fabric :: Lens.Lens' NetworkFrameworkConfiguration (Prelude.Maybe NetworkFabricConfiguration)
networkFrameworkConfiguration_fabric = Lens.lens (\NetworkFrameworkConfiguration' {fabric} -> fabric) (\s@NetworkFrameworkConfiguration' {} a -> s {fabric = a} :: NetworkFrameworkConfiguration)

instance
  Prelude.Hashable
    NetworkFrameworkConfiguration
  where
  hashWithSalt _salt NetworkFrameworkConfiguration' {..} =
    _salt `Prelude.hashWithSalt` fabric

instance Prelude.NFData NetworkFrameworkConfiguration where
  rnf NetworkFrameworkConfiguration' {..} =
    Prelude.rnf fabric

instance Data.ToJSON NetworkFrameworkConfiguration where
  toJSON NetworkFrameworkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Fabric" Data..=) Prelude.<$> fabric]
      )
