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
-- Module      : Network.AWS.ManagedBlockChain.Types.NetworkFabricAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ManagedBlockChain.Types.NetworkFabricAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ManagedBlockChain.Types.Edition
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON NetworkFabricAttributes where
  parseJSON =
    Core.withObject
      "NetworkFabricAttributes"
      ( \x ->
          NetworkFabricAttributes'
            Prelude.<$> (x Core..:? "Edition")
            Prelude.<*> (x Core..:? "OrderingServiceEndpoint")
      )

instance Prelude.Hashable NetworkFabricAttributes

instance Prelude.NFData NetworkFabricAttributes
