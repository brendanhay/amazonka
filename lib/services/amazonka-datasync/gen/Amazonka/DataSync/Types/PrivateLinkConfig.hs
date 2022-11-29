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
-- Module      : Amazonka.DataSync.Types.PrivateLinkConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.PrivateLinkConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The VPC endpoint, subnet, and security group that an agent uses to
-- access IP addresses in a VPC (Virtual Private Cloud).
--
-- /See:/ 'newPrivateLinkConfig' smart constructor.
data PrivateLinkConfig = PrivateLinkConfig'
  { -- | The Amazon Resource Names (ARNs) of the subnets that are configured for
    -- an agent activated in a VPC or an agent that has access to a VPC
    -- endpoint.
    subnetArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The private endpoint that is configured for an agent that has access to
    -- IP addresses in a
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html PrivateLink>.
    -- An agent that is configured with this endpoint will not be accessible
    -- over the public internet.
    privateLinkEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC endpoint that is configured for an agent. An agent
    -- that is configured with a VPC endpoint will not be accessible over the
    -- public internet.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the security groups that are
    -- configured for the EC2 resource that hosts an agent activated in a VPC
    -- or an agent that has access to a VPC endpoint.
    securityGroupArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateLinkConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetArns', 'privateLinkConfig_subnetArns' - The Amazon Resource Names (ARNs) of the subnets that are configured for
-- an agent activated in a VPC or an agent that has access to a VPC
-- endpoint.
--
-- 'privateLinkEndpoint', 'privateLinkConfig_privateLinkEndpoint' - The private endpoint that is configured for an agent that has access to
-- IP addresses in a
-- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html PrivateLink>.
-- An agent that is configured with this endpoint will not be accessible
-- over the public internet.
--
-- 'vpcEndpointId', 'privateLinkConfig_vpcEndpointId' - The ID of the VPC endpoint that is configured for an agent. An agent
-- that is configured with a VPC endpoint will not be accessible over the
-- public internet.
--
-- 'securityGroupArns', 'privateLinkConfig_securityGroupArns' - The Amazon Resource Names (ARNs) of the security groups that are
-- configured for the EC2 resource that hosts an agent activated in a VPC
-- or an agent that has access to a VPC endpoint.
newPrivateLinkConfig ::
  PrivateLinkConfig
newPrivateLinkConfig =
  PrivateLinkConfig'
    { subnetArns = Prelude.Nothing,
      privateLinkEndpoint = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing,
      securityGroupArns = Prelude.Nothing
    }

-- | The Amazon Resource Names (ARNs) of the subnets that are configured for
-- an agent activated in a VPC or an agent that has access to a VPC
-- endpoint.
privateLinkConfig_subnetArns :: Lens.Lens' PrivateLinkConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
privateLinkConfig_subnetArns = Lens.lens (\PrivateLinkConfig' {subnetArns} -> subnetArns) (\s@PrivateLinkConfig' {} a -> s {subnetArns = a} :: PrivateLinkConfig) Prelude.. Lens.mapping Lens.coerced

-- | The private endpoint that is configured for an agent that has access to
-- IP addresses in a
-- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html PrivateLink>.
-- An agent that is configured with this endpoint will not be accessible
-- over the public internet.
privateLinkConfig_privateLinkEndpoint :: Lens.Lens' PrivateLinkConfig (Prelude.Maybe Prelude.Text)
privateLinkConfig_privateLinkEndpoint = Lens.lens (\PrivateLinkConfig' {privateLinkEndpoint} -> privateLinkEndpoint) (\s@PrivateLinkConfig' {} a -> s {privateLinkEndpoint = a} :: PrivateLinkConfig)

-- | The ID of the VPC endpoint that is configured for an agent. An agent
-- that is configured with a VPC endpoint will not be accessible over the
-- public internet.
privateLinkConfig_vpcEndpointId :: Lens.Lens' PrivateLinkConfig (Prelude.Maybe Prelude.Text)
privateLinkConfig_vpcEndpointId = Lens.lens (\PrivateLinkConfig' {vpcEndpointId} -> vpcEndpointId) (\s@PrivateLinkConfig' {} a -> s {vpcEndpointId = a} :: PrivateLinkConfig)

-- | The Amazon Resource Names (ARNs) of the security groups that are
-- configured for the EC2 resource that hosts an agent activated in a VPC
-- or an agent that has access to a VPC endpoint.
privateLinkConfig_securityGroupArns :: Lens.Lens' PrivateLinkConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
privateLinkConfig_securityGroupArns = Lens.lens (\PrivateLinkConfig' {securityGroupArns} -> securityGroupArns) (\s@PrivateLinkConfig' {} a -> s {securityGroupArns = a} :: PrivateLinkConfig) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PrivateLinkConfig where
  parseJSON =
    Core.withObject
      "PrivateLinkConfig"
      ( \x ->
          PrivateLinkConfig'
            Prelude.<$> (x Core..:? "SubnetArns")
            Prelude.<*> (x Core..:? "PrivateLinkEndpoint")
            Prelude.<*> (x Core..:? "VpcEndpointId")
            Prelude.<*> (x Core..:? "SecurityGroupArns")
      )

instance Prelude.Hashable PrivateLinkConfig where
  hashWithSalt _salt PrivateLinkConfig' {..} =
    _salt `Prelude.hashWithSalt` subnetArns
      `Prelude.hashWithSalt` privateLinkEndpoint
      `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` securityGroupArns

instance Prelude.NFData PrivateLinkConfig where
  rnf PrivateLinkConfig' {..} =
    Prelude.rnf subnetArns
      `Prelude.seq` Prelude.rnf privateLinkEndpoint
      `Prelude.seq` Prelude.rnf vpcEndpointId
      `Prelude.seq` Prelude.rnf securityGroupArns
