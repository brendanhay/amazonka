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
-- Module      : Amazonka.ManagedBlockChain.Types.NetworkEthereumAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NetworkEthereumAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Attributes of Ethereum for a network.
--
-- /See:/ 'newNetworkEthereumAttributes' smart constructor.
data NetworkEthereumAttributes = NetworkEthereumAttributes'
  { -- | The Ethereum @CHAIN_ID@ associated with the Ethereum network. Chain IDs
    -- are as follows:
    --
    -- -   mainnet = @1@
    --
    -- -   goerli = @5@
    --
    -- -   rinkeby = @4@
    --
    -- -   ropsten = @3@
    chainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkEthereumAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chainId', 'networkEthereumAttributes_chainId' - The Ethereum @CHAIN_ID@ associated with the Ethereum network. Chain IDs
-- are as follows:
--
-- -   mainnet = @1@
--
-- -   goerli = @5@
--
-- -   rinkeby = @4@
--
-- -   ropsten = @3@
newNetworkEthereumAttributes ::
  NetworkEthereumAttributes
newNetworkEthereumAttributes =
  NetworkEthereumAttributes'
    { chainId =
        Prelude.Nothing
    }

-- | The Ethereum @CHAIN_ID@ associated with the Ethereum network. Chain IDs
-- are as follows:
--
-- -   mainnet = @1@
--
-- -   goerli = @5@
--
-- -   rinkeby = @4@
--
-- -   ropsten = @3@
networkEthereumAttributes_chainId :: Lens.Lens' NetworkEthereumAttributes (Prelude.Maybe Prelude.Text)
networkEthereumAttributes_chainId = Lens.lens (\NetworkEthereumAttributes' {chainId} -> chainId) (\s@NetworkEthereumAttributes' {} a -> s {chainId = a} :: NetworkEthereumAttributes)

instance Data.FromJSON NetworkEthereumAttributes where
  parseJSON =
    Data.withObject
      "NetworkEthereumAttributes"
      ( \x ->
          NetworkEthereumAttributes'
            Prelude.<$> (x Data..:? "ChainId")
      )

instance Prelude.Hashable NetworkEthereumAttributes where
  hashWithSalt _salt NetworkEthereumAttributes' {..} =
    _salt `Prelude.hashWithSalt` chainId

instance Prelude.NFData NetworkEthereumAttributes where
  rnf NetworkEthereumAttributes' {..} =
    Prelude.rnf chainId
