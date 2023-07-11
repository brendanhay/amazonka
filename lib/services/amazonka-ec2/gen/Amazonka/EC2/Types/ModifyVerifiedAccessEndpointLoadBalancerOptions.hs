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
-- Module      : Amazonka.EC2.Types.ModifyVerifiedAccessEndpointLoadBalancerOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ModifyVerifiedAccessEndpointLoadBalancerOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessEndpointProtocol
import qualified Amazonka.Prelude as Prelude

-- | Describes a load balancer when creating an Amazon Web Services Verified
-- Access endpoint using the @load-balancer@ type.
--
-- /See:/ 'newModifyVerifiedAccessEndpointLoadBalancerOptions' smart constructor.
data ModifyVerifiedAccessEndpointLoadBalancerOptions = ModifyVerifiedAccessEndpointLoadBalancerOptions'
  { -- | The IP port number.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The IP protocol.
    protocol :: Prelude.Maybe VerifiedAccessEndpointProtocol,
    -- | The IDs of the subnets.
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessEndpointLoadBalancerOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'modifyVerifiedAccessEndpointLoadBalancerOptions_port' - The IP port number.
--
-- 'protocol', 'modifyVerifiedAccessEndpointLoadBalancerOptions_protocol' - The IP protocol.
--
-- 'subnetIds', 'modifyVerifiedAccessEndpointLoadBalancerOptions_subnetIds' - The IDs of the subnets.
newModifyVerifiedAccessEndpointLoadBalancerOptions ::
  ModifyVerifiedAccessEndpointLoadBalancerOptions
newModifyVerifiedAccessEndpointLoadBalancerOptions =
  ModifyVerifiedAccessEndpointLoadBalancerOptions'
    { port =
        Prelude.Nothing,
      protocol = Prelude.Nothing,
      subnetIds =
        Prelude.Nothing
    }

-- | The IP port number.
modifyVerifiedAccessEndpointLoadBalancerOptions_port :: Lens.Lens' ModifyVerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe Prelude.Natural)
modifyVerifiedAccessEndpointLoadBalancerOptions_port = Lens.lens (\ModifyVerifiedAccessEndpointLoadBalancerOptions' {port} -> port) (\s@ModifyVerifiedAccessEndpointLoadBalancerOptions' {} a -> s {port = a} :: ModifyVerifiedAccessEndpointLoadBalancerOptions)

-- | The IP protocol.
modifyVerifiedAccessEndpointLoadBalancerOptions_protocol :: Lens.Lens' ModifyVerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe VerifiedAccessEndpointProtocol)
modifyVerifiedAccessEndpointLoadBalancerOptions_protocol = Lens.lens (\ModifyVerifiedAccessEndpointLoadBalancerOptions' {protocol} -> protocol) (\s@ModifyVerifiedAccessEndpointLoadBalancerOptions' {} a -> s {protocol = a} :: ModifyVerifiedAccessEndpointLoadBalancerOptions)

-- | The IDs of the subnets.
modifyVerifiedAccessEndpointLoadBalancerOptions_subnetIds :: Lens.Lens' ModifyVerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe [Prelude.Text])
modifyVerifiedAccessEndpointLoadBalancerOptions_subnetIds = Lens.lens (\ModifyVerifiedAccessEndpointLoadBalancerOptions' {subnetIds} -> subnetIds) (\s@ModifyVerifiedAccessEndpointLoadBalancerOptions' {} a -> s {subnetIds = a} :: ModifyVerifiedAccessEndpointLoadBalancerOptions) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    ModifyVerifiedAccessEndpointLoadBalancerOptions
  where
  hashWithSalt
    _salt
    ModifyVerifiedAccessEndpointLoadBalancerOptions' {..} =
      _salt
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` protocol
        `Prelude.hashWithSalt` subnetIds

instance
  Prelude.NFData
    ModifyVerifiedAccessEndpointLoadBalancerOptions
  where
  rnf
    ModifyVerifiedAccessEndpointLoadBalancerOptions' {..} =
      Prelude.rnf port
        `Prelude.seq` Prelude.rnf protocol
        `Prelude.seq` Prelude.rnf subnetIds

instance
  Data.ToQuery
    ModifyVerifiedAccessEndpointLoadBalancerOptions
  where
  toQuery
    ModifyVerifiedAccessEndpointLoadBalancerOptions' {..} =
      Prelude.mconcat
        [ "Port" Data.=: port,
          "Protocol" Data.=: protocol,
          Data.toQuery
            (Data.toQueryList "SubnetId" Prelude.<$> subnetIds)
        ]
