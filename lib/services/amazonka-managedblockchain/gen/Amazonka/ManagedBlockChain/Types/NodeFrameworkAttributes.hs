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
-- Module      : Amazonka.ManagedBlockChain.Types.NodeFrameworkAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NodeFrameworkAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.NodeEthereumAttributes
import Amazonka.ManagedBlockChain.Types.NodeFabricAttributes
import qualified Amazonka.Prelude as Prelude

-- | Attributes relevant to a node on a Managed Blockchain network for the
-- blockchain framework that the network uses.
--
-- /See:/ 'newNodeFrameworkAttributes' smart constructor.
data NodeFrameworkAttributes = NodeFrameworkAttributes'
  { -- | Attributes of Ethereum for a node on a Managed Blockchain network that
    -- uses Ethereum.
    ethereum :: Prelude.Maybe NodeEthereumAttributes,
    -- | Attributes of Hyperledger Fabric for a peer node on a Managed Blockchain
    -- network that uses Hyperledger Fabric.
    fabric :: Prelude.Maybe NodeFabricAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeFrameworkAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ethereum', 'nodeFrameworkAttributes_ethereum' - Attributes of Ethereum for a node on a Managed Blockchain network that
-- uses Ethereum.
--
-- 'fabric', 'nodeFrameworkAttributes_fabric' - Attributes of Hyperledger Fabric for a peer node on a Managed Blockchain
-- network that uses Hyperledger Fabric.
newNodeFrameworkAttributes ::
  NodeFrameworkAttributes
newNodeFrameworkAttributes =
  NodeFrameworkAttributes'
    { ethereum =
        Prelude.Nothing,
      fabric = Prelude.Nothing
    }

-- | Attributes of Ethereum for a node on a Managed Blockchain network that
-- uses Ethereum.
nodeFrameworkAttributes_ethereum :: Lens.Lens' NodeFrameworkAttributes (Prelude.Maybe NodeEthereumAttributes)
nodeFrameworkAttributes_ethereum = Lens.lens (\NodeFrameworkAttributes' {ethereum} -> ethereum) (\s@NodeFrameworkAttributes' {} a -> s {ethereum = a} :: NodeFrameworkAttributes)

-- | Attributes of Hyperledger Fabric for a peer node on a Managed Blockchain
-- network that uses Hyperledger Fabric.
nodeFrameworkAttributes_fabric :: Lens.Lens' NodeFrameworkAttributes (Prelude.Maybe NodeFabricAttributes)
nodeFrameworkAttributes_fabric = Lens.lens (\NodeFrameworkAttributes' {fabric} -> fabric) (\s@NodeFrameworkAttributes' {} a -> s {fabric = a} :: NodeFrameworkAttributes)

instance Data.FromJSON NodeFrameworkAttributes where
  parseJSON =
    Data.withObject
      "NodeFrameworkAttributes"
      ( \x ->
          NodeFrameworkAttributes'
            Prelude.<$> (x Data..:? "Ethereum")
            Prelude.<*> (x Data..:? "Fabric")
      )

instance Prelude.Hashable NodeFrameworkAttributes where
  hashWithSalt _salt NodeFrameworkAttributes' {..} =
    _salt `Prelude.hashWithSalt` ethereum
      `Prelude.hashWithSalt` fabric

instance Prelude.NFData NodeFrameworkAttributes where
  rnf NodeFrameworkAttributes' {..} =
    Prelude.rnf ethereum
      `Prelude.seq` Prelude.rnf fabric
