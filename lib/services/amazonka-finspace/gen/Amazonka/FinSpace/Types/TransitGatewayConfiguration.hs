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
-- Module      : Amazonka.FinSpace.Types.TransitGatewayConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.TransitGatewayConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The structure of the transit gateway and network configuration that is
-- used to connect the kdb environment to an internal network.
--
-- /See:/ 'newTransitGatewayConfiguration' smart constructor.
data TransitGatewayConfiguration = TransitGatewayConfiguration'
  { -- | The identifier of the transit gateway created by the customer to connect
    -- outbound traffics from kdb network to your internal network.
    transitGatewayID :: Prelude.Text,
    -- | The routing CIDR on behalf of kdb environment. It could be any \"\/26
    -- range in the 100.64.0.0 CIDR space. After providing, it will be added to
    -- the customer\'s transit gateway routing table so that the traffics could
    -- be routed to kdb network.
    routableCIDRSpace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayID', 'transitGatewayConfiguration_transitGatewayID' - The identifier of the transit gateway created by the customer to connect
-- outbound traffics from kdb network to your internal network.
--
-- 'routableCIDRSpace', 'transitGatewayConfiguration_routableCIDRSpace' - The routing CIDR on behalf of kdb environment. It could be any \"\/26
-- range in the 100.64.0.0 CIDR space. After providing, it will be added to
-- the customer\'s transit gateway routing table so that the traffics could
-- be routed to kdb network.
newTransitGatewayConfiguration ::
  -- | 'transitGatewayID'
  Prelude.Text ->
  -- | 'routableCIDRSpace'
  Prelude.Text ->
  TransitGatewayConfiguration
newTransitGatewayConfiguration
  pTransitGatewayID_
  pRoutableCIDRSpace_ =
    TransitGatewayConfiguration'
      { transitGatewayID =
          pTransitGatewayID_,
        routableCIDRSpace = pRoutableCIDRSpace_
      }

-- | The identifier of the transit gateway created by the customer to connect
-- outbound traffics from kdb network to your internal network.
transitGatewayConfiguration_transitGatewayID :: Lens.Lens' TransitGatewayConfiguration Prelude.Text
transitGatewayConfiguration_transitGatewayID = Lens.lens (\TransitGatewayConfiguration' {transitGatewayID} -> transitGatewayID) (\s@TransitGatewayConfiguration' {} a -> s {transitGatewayID = a} :: TransitGatewayConfiguration)

-- | The routing CIDR on behalf of kdb environment. It could be any \"\/26
-- range in the 100.64.0.0 CIDR space. After providing, it will be added to
-- the customer\'s transit gateway routing table so that the traffics could
-- be routed to kdb network.
transitGatewayConfiguration_routableCIDRSpace :: Lens.Lens' TransitGatewayConfiguration Prelude.Text
transitGatewayConfiguration_routableCIDRSpace = Lens.lens (\TransitGatewayConfiguration' {routableCIDRSpace} -> routableCIDRSpace) (\s@TransitGatewayConfiguration' {} a -> s {routableCIDRSpace = a} :: TransitGatewayConfiguration)

instance Data.FromJSON TransitGatewayConfiguration where
  parseJSON =
    Data.withObject
      "TransitGatewayConfiguration"
      ( \x ->
          TransitGatewayConfiguration'
            Prelude.<$> (x Data..: "transitGatewayID")
            Prelude.<*> (x Data..: "routableCIDRSpace")
      )

instance Prelude.Hashable TransitGatewayConfiguration where
  hashWithSalt _salt TransitGatewayConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` transitGatewayID
      `Prelude.hashWithSalt` routableCIDRSpace

instance Prelude.NFData TransitGatewayConfiguration where
  rnf TransitGatewayConfiguration' {..} =
    Prelude.rnf transitGatewayID
      `Prelude.seq` Prelude.rnf routableCIDRSpace

instance Data.ToJSON TransitGatewayConfiguration where
  toJSON TransitGatewayConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("transitGatewayID" Data..= transitGatewayID),
            Prelude.Just
              ("routableCIDRSpace" Data..= routableCIDRSpace)
          ]
      )
