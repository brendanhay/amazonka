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
-- Module      : Amazonka.EC2.Types.CreateVerifiedAccessEndpointLoadBalancerOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreateVerifiedAccessEndpointLoadBalancerOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessEndpointProtocol
import qualified Amazonka.Prelude as Prelude

-- | Describes a load balancer when creating an Amazon Web Services Verified
-- Access endpoint using the @load-balancer@ type.
--
-- /See:/ 'newCreateVerifiedAccessEndpointLoadBalancerOptions' smart constructor.
data CreateVerifiedAccessEndpointLoadBalancerOptions = CreateVerifiedAccessEndpointLoadBalancerOptions'
  { -- | The ARN of the load balancer.
    loadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | The IP port number.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The IP protocol.
    protocol :: Prelude.Maybe VerifiedAccessEndpointProtocol,
    -- | The IDs of the subnets.
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessEndpointLoadBalancerOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerArn', 'createVerifiedAccessEndpointLoadBalancerOptions_loadBalancerArn' - The ARN of the load balancer.
--
-- 'port', 'createVerifiedAccessEndpointLoadBalancerOptions_port' - The IP port number.
--
-- 'protocol', 'createVerifiedAccessEndpointLoadBalancerOptions_protocol' - The IP protocol.
--
-- 'subnetIds', 'createVerifiedAccessEndpointLoadBalancerOptions_subnetIds' - The IDs of the subnets.
newCreateVerifiedAccessEndpointLoadBalancerOptions ::
  CreateVerifiedAccessEndpointLoadBalancerOptions
newCreateVerifiedAccessEndpointLoadBalancerOptions =
  CreateVerifiedAccessEndpointLoadBalancerOptions'
    { loadBalancerArn =
        Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing,
      subnetIds =
        Prelude.Nothing
    }

-- | The ARN of the load balancer.
createVerifiedAccessEndpointLoadBalancerOptions_loadBalancerArn :: Lens.Lens' CreateVerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe Prelude.Text)
createVerifiedAccessEndpointLoadBalancerOptions_loadBalancerArn = Lens.lens (\CreateVerifiedAccessEndpointLoadBalancerOptions' {loadBalancerArn} -> loadBalancerArn) (\s@CreateVerifiedAccessEndpointLoadBalancerOptions' {} a -> s {loadBalancerArn = a} :: CreateVerifiedAccessEndpointLoadBalancerOptions)

-- | The IP port number.
createVerifiedAccessEndpointLoadBalancerOptions_port :: Lens.Lens' CreateVerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe Prelude.Natural)
createVerifiedAccessEndpointLoadBalancerOptions_port = Lens.lens (\CreateVerifiedAccessEndpointLoadBalancerOptions' {port} -> port) (\s@CreateVerifiedAccessEndpointLoadBalancerOptions' {} a -> s {port = a} :: CreateVerifiedAccessEndpointLoadBalancerOptions)

-- | The IP protocol.
createVerifiedAccessEndpointLoadBalancerOptions_protocol :: Lens.Lens' CreateVerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe VerifiedAccessEndpointProtocol)
createVerifiedAccessEndpointLoadBalancerOptions_protocol = Lens.lens (\CreateVerifiedAccessEndpointLoadBalancerOptions' {protocol} -> protocol) (\s@CreateVerifiedAccessEndpointLoadBalancerOptions' {} a -> s {protocol = a} :: CreateVerifiedAccessEndpointLoadBalancerOptions)

-- | The IDs of the subnets.
createVerifiedAccessEndpointLoadBalancerOptions_subnetIds :: Lens.Lens' CreateVerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe [Prelude.Text])
createVerifiedAccessEndpointLoadBalancerOptions_subnetIds = Lens.lens (\CreateVerifiedAccessEndpointLoadBalancerOptions' {subnetIds} -> subnetIds) (\s@CreateVerifiedAccessEndpointLoadBalancerOptions' {} a -> s {subnetIds = a} :: CreateVerifiedAccessEndpointLoadBalancerOptions) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    CreateVerifiedAccessEndpointLoadBalancerOptions
  where
  hashWithSalt
    _salt
    CreateVerifiedAccessEndpointLoadBalancerOptions' {..} =
      _salt `Prelude.hashWithSalt` loadBalancerArn
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` protocol
        `Prelude.hashWithSalt` subnetIds

instance
  Prelude.NFData
    CreateVerifiedAccessEndpointLoadBalancerOptions
  where
  rnf
    CreateVerifiedAccessEndpointLoadBalancerOptions' {..} =
      Prelude.rnf loadBalancerArn
        `Prelude.seq` Prelude.rnf port
        `Prelude.seq` Prelude.rnf protocol
        `Prelude.seq` Prelude.rnf subnetIds

instance
  Data.ToQuery
    CreateVerifiedAccessEndpointLoadBalancerOptions
  where
  toQuery
    CreateVerifiedAccessEndpointLoadBalancerOptions' {..} =
      Prelude.mconcat
        [ "LoadBalancerArn" Data.=: loadBalancerArn,
          "Port" Data.=: port,
          "Protocol" Data.=: protocol,
          Data.toQuery
            (Data.toQueryList "SubnetId" Prelude.<$> subnetIds)
        ]
