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
-- Module      : Amazonka.ManagedBlockChain.Types.NetworkFabricAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NetworkFabricAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.Edition
import qualified Amazonka.Prelude as Prelude

-- | Attributes of Hyperledger Fabric for a network.
--
-- /See:/ 'newNetworkFabricAttributes' smart constructor.
data NetworkFabricAttributes = NetworkFabricAttributes'
  { -- | The edition of Amazon Managed Blockchain that Hyperledger Fabric uses.
    -- For more information, see
    -- <http://aws.amazon.com/managed-blockchain/pricing/ Amazon Managed Blockchain Pricing>.
    edition :: Prelude.Maybe Edition,
    -- | The endpoint of the ordering service for the network.
    orderingServiceEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFabricAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edition', 'networkFabricAttributes_edition' - The edition of Amazon Managed Blockchain that Hyperledger Fabric uses.
-- For more information, see
-- <http://aws.amazon.com/managed-blockchain/pricing/ Amazon Managed Blockchain Pricing>.
--
-- 'orderingServiceEndpoint', 'networkFabricAttributes_orderingServiceEndpoint' - The endpoint of the ordering service for the network.
newNetworkFabricAttributes ::
  NetworkFabricAttributes
newNetworkFabricAttributes =
  NetworkFabricAttributes'
    { edition = Prelude.Nothing,
      orderingServiceEndpoint = Prelude.Nothing
    }

-- | The edition of Amazon Managed Blockchain that Hyperledger Fabric uses.
-- For more information, see
-- <http://aws.amazon.com/managed-blockchain/pricing/ Amazon Managed Blockchain Pricing>.
networkFabricAttributes_edition :: Lens.Lens' NetworkFabricAttributes (Prelude.Maybe Edition)
networkFabricAttributes_edition = Lens.lens (\NetworkFabricAttributes' {edition} -> edition) (\s@NetworkFabricAttributes' {} a -> s {edition = a} :: NetworkFabricAttributes)

-- | The endpoint of the ordering service for the network.
networkFabricAttributes_orderingServiceEndpoint :: Lens.Lens' NetworkFabricAttributes (Prelude.Maybe Prelude.Text)
networkFabricAttributes_orderingServiceEndpoint = Lens.lens (\NetworkFabricAttributes' {orderingServiceEndpoint} -> orderingServiceEndpoint) (\s@NetworkFabricAttributes' {} a -> s {orderingServiceEndpoint = a} :: NetworkFabricAttributes)

instance Data.FromJSON NetworkFabricAttributes where
  parseJSON =
    Data.withObject
      "NetworkFabricAttributes"
      ( \x ->
          NetworkFabricAttributes'
            Prelude.<$> (x Data..:? "Edition")
            Prelude.<*> (x Data..:? "OrderingServiceEndpoint")
      )

instance Prelude.Hashable NetworkFabricAttributes where
  hashWithSalt _salt NetworkFabricAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` edition
      `Prelude.hashWithSalt` orderingServiceEndpoint

instance Prelude.NFData NetworkFabricAttributes where
  rnf NetworkFabricAttributes' {..} =
    Prelude.rnf edition `Prelude.seq`
      Prelude.rnf orderingServiceEndpoint
