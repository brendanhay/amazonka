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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerListener
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerListener where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a load balancer listener.
--
-- /See:/ 'newAwsElbLoadBalancerListener' smart constructor.
data AwsElbLoadBalancerListener = AwsElbLoadBalancerListener'
  { -- | The ARN of the server certificate.
    sslCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The protocol to use to route traffic to instances.
    --
    -- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
    instanceProtocol :: Prelude.Maybe Prelude.Text,
    -- | The port on which the instance is listening.
    instancePort :: Prelude.Maybe Prelude.Int,
    -- | The load balancer transport protocol to use for routing.
    --
    -- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The port on which the load balancer is listening.
    --
    -- On EC2-VPC, you can specify any port from the range 1-65535.
    --
    -- On EC2-Classic, you can specify any port from the following list: 25,
    -- 80, 443, 465, 587, 1024-65535.
    loadBalancerPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sslCertificateId', 'awsElbLoadBalancerListener_sslCertificateId' - The ARN of the server certificate.
--
-- 'instanceProtocol', 'awsElbLoadBalancerListener_instanceProtocol' - The protocol to use to route traffic to instances.
--
-- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
--
-- 'instancePort', 'awsElbLoadBalancerListener_instancePort' - The port on which the instance is listening.
--
-- 'protocol', 'awsElbLoadBalancerListener_protocol' - The load balancer transport protocol to use for routing.
--
-- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
--
-- 'loadBalancerPort', 'awsElbLoadBalancerListener_loadBalancerPort' - The port on which the load balancer is listening.
--
-- On EC2-VPC, you can specify any port from the range 1-65535.
--
-- On EC2-Classic, you can specify any port from the following list: 25,
-- 80, 443, 465, 587, 1024-65535.
newAwsElbLoadBalancerListener ::
  AwsElbLoadBalancerListener
newAwsElbLoadBalancerListener =
  AwsElbLoadBalancerListener'
    { sslCertificateId =
        Prelude.Nothing,
      instanceProtocol = Prelude.Nothing,
      instancePort = Prelude.Nothing,
      protocol = Prelude.Nothing,
      loadBalancerPort = Prelude.Nothing
    }

-- | The ARN of the server certificate.
awsElbLoadBalancerListener_sslCertificateId :: Lens.Lens' AwsElbLoadBalancerListener (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerListener_sslCertificateId = Lens.lens (\AwsElbLoadBalancerListener' {sslCertificateId} -> sslCertificateId) (\s@AwsElbLoadBalancerListener' {} a -> s {sslCertificateId = a} :: AwsElbLoadBalancerListener)

-- | The protocol to use to route traffic to instances.
--
-- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
awsElbLoadBalancerListener_instanceProtocol :: Lens.Lens' AwsElbLoadBalancerListener (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerListener_instanceProtocol = Lens.lens (\AwsElbLoadBalancerListener' {instanceProtocol} -> instanceProtocol) (\s@AwsElbLoadBalancerListener' {} a -> s {instanceProtocol = a} :: AwsElbLoadBalancerListener)

-- | The port on which the instance is listening.
awsElbLoadBalancerListener_instancePort :: Lens.Lens' AwsElbLoadBalancerListener (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerListener_instancePort = Lens.lens (\AwsElbLoadBalancerListener' {instancePort} -> instancePort) (\s@AwsElbLoadBalancerListener' {} a -> s {instancePort = a} :: AwsElbLoadBalancerListener)

-- | The load balancer transport protocol to use for routing.
--
-- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
awsElbLoadBalancerListener_protocol :: Lens.Lens' AwsElbLoadBalancerListener (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerListener_protocol = Lens.lens (\AwsElbLoadBalancerListener' {protocol} -> protocol) (\s@AwsElbLoadBalancerListener' {} a -> s {protocol = a} :: AwsElbLoadBalancerListener)

-- | The port on which the load balancer is listening.
--
-- On EC2-VPC, you can specify any port from the range 1-65535.
--
-- On EC2-Classic, you can specify any port from the following list: 25,
-- 80, 443, 465, 587, 1024-65535.
awsElbLoadBalancerListener_loadBalancerPort :: Lens.Lens' AwsElbLoadBalancerListener (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerListener_loadBalancerPort = Lens.lens (\AwsElbLoadBalancerListener' {loadBalancerPort} -> loadBalancerPort) (\s@AwsElbLoadBalancerListener' {} a -> s {loadBalancerPort = a} :: AwsElbLoadBalancerListener)

instance Core.FromJSON AwsElbLoadBalancerListener where
  parseJSON =
    Core.withObject
      "AwsElbLoadBalancerListener"
      ( \x ->
          AwsElbLoadBalancerListener'
            Prelude.<$> (x Core..:? "SslCertificateId")
            Prelude.<*> (x Core..:? "InstanceProtocol")
            Prelude.<*> (x Core..:? "InstancePort")
            Prelude.<*> (x Core..:? "Protocol")
            Prelude.<*> (x Core..:? "LoadBalancerPort")
      )

instance Prelude.Hashable AwsElbLoadBalancerListener where
  hashWithSalt _salt AwsElbLoadBalancerListener' {..} =
    _salt `Prelude.hashWithSalt` sslCertificateId
      `Prelude.hashWithSalt` instanceProtocol
      `Prelude.hashWithSalt` instancePort
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` loadBalancerPort

instance Prelude.NFData AwsElbLoadBalancerListener where
  rnf AwsElbLoadBalancerListener' {..} =
    Prelude.rnf sslCertificateId
      `Prelude.seq` Prelude.rnf instanceProtocol
      `Prelude.seq` Prelude.rnf instancePort
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf loadBalancerPort

instance Core.ToJSON AwsElbLoadBalancerListener where
  toJSON AwsElbLoadBalancerListener' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SslCertificateId" Core..=)
              Prelude.<$> sslCertificateId,
            ("InstanceProtocol" Core..=)
              Prelude.<$> instanceProtocol,
            ("InstancePort" Core..=) Prelude.<$> instancePort,
            ("Protocol" Core..=) Prelude.<$> protocol,
            ("LoadBalancerPort" Core..=)
              Prelude.<$> loadBalancerPort
          ]
      )
