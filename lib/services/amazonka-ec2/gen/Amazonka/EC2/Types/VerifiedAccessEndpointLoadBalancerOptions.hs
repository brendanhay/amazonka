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
-- Module      : Amazonka.EC2.Types.VerifiedAccessEndpointLoadBalancerOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessEndpointLoadBalancerOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessEndpointProtocol
import qualified Amazonka.Prelude as Prelude

-- | Describes a load balancer when creating an Amazon Web Services Verified
-- Access endpoint using the @load-balancer@ type.
--
-- /See:/ 'newVerifiedAccessEndpointLoadBalancerOptions' smart constructor.
data VerifiedAccessEndpointLoadBalancerOptions = VerifiedAccessEndpointLoadBalancerOptions'
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
-- Create a value of 'VerifiedAccessEndpointLoadBalancerOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerArn', 'verifiedAccessEndpointLoadBalancerOptions_loadBalancerArn' - The ARN of the load balancer.
--
-- 'port', 'verifiedAccessEndpointLoadBalancerOptions_port' - The IP port number.
--
-- 'protocol', 'verifiedAccessEndpointLoadBalancerOptions_protocol' - The IP protocol.
--
-- 'subnetIds', 'verifiedAccessEndpointLoadBalancerOptions_subnetIds' - The IDs of the subnets.
newVerifiedAccessEndpointLoadBalancerOptions ::
  VerifiedAccessEndpointLoadBalancerOptions
newVerifiedAccessEndpointLoadBalancerOptions =
  VerifiedAccessEndpointLoadBalancerOptions'
    { loadBalancerArn =
        Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | The ARN of the load balancer.
verifiedAccessEndpointLoadBalancerOptions_loadBalancerArn :: Lens.Lens' VerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe Prelude.Text)
verifiedAccessEndpointLoadBalancerOptions_loadBalancerArn = Lens.lens (\VerifiedAccessEndpointLoadBalancerOptions' {loadBalancerArn} -> loadBalancerArn) (\s@VerifiedAccessEndpointLoadBalancerOptions' {} a -> s {loadBalancerArn = a} :: VerifiedAccessEndpointLoadBalancerOptions)

-- | The IP port number.
verifiedAccessEndpointLoadBalancerOptions_port :: Lens.Lens' VerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe Prelude.Natural)
verifiedAccessEndpointLoadBalancerOptions_port = Lens.lens (\VerifiedAccessEndpointLoadBalancerOptions' {port} -> port) (\s@VerifiedAccessEndpointLoadBalancerOptions' {} a -> s {port = a} :: VerifiedAccessEndpointLoadBalancerOptions)

-- | The IP protocol.
verifiedAccessEndpointLoadBalancerOptions_protocol :: Lens.Lens' VerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe VerifiedAccessEndpointProtocol)
verifiedAccessEndpointLoadBalancerOptions_protocol = Lens.lens (\VerifiedAccessEndpointLoadBalancerOptions' {protocol} -> protocol) (\s@VerifiedAccessEndpointLoadBalancerOptions' {} a -> s {protocol = a} :: VerifiedAccessEndpointLoadBalancerOptions)

-- | The IDs of the subnets.
verifiedAccessEndpointLoadBalancerOptions_subnetIds :: Lens.Lens' VerifiedAccessEndpointLoadBalancerOptions (Prelude.Maybe [Prelude.Text])
verifiedAccessEndpointLoadBalancerOptions_subnetIds = Lens.lens (\VerifiedAccessEndpointLoadBalancerOptions' {subnetIds} -> subnetIds) (\s@VerifiedAccessEndpointLoadBalancerOptions' {} a -> s {subnetIds = a} :: VerifiedAccessEndpointLoadBalancerOptions) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromXML
    VerifiedAccessEndpointLoadBalancerOptions
  where
  parseXML x =
    VerifiedAccessEndpointLoadBalancerOptions'
      Prelude.<$> (x Data..@? "loadBalancerArn")
        Prelude.<*> (x Data..@? "port")
        Prelude.<*> (x Data..@? "protocol")
        Prelude.<*> ( x Data..@? "subnetIdSet" Core..!@ Prelude.mempty
                        Prelude.>>= Core.may (Data.parseXMLList "item")
                    )

instance
  Prelude.Hashable
    VerifiedAccessEndpointLoadBalancerOptions
  where
  hashWithSalt
    _salt
    VerifiedAccessEndpointLoadBalancerOptions' {..} =
      _salt `Prelude.hashWithSalt` loadBalancerArn
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` protocol
        `Prelude.hashWithSalt` subnetIds

instance
  Prelude.NFData
    VerifiedAccessEndpointLoadBalancerOptions
  where
  rnf VerifiedAccessEndpointLoadBalancerOptions' {..} =
    Prelude.rnf loadBalancerArn
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf subnetIds
