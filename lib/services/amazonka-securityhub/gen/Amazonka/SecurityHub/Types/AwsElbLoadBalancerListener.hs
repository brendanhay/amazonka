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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a load balancer listener.
--
-- /See:/ 'newAwsElbLoadBalancerListener' smart constructor.
data AwsElbLoadBalancerListener = AwsElbLoadBalancerListener'
  { -- | The port on which the instance is listening.
    instancePort :: Prelude.Maybe Prelude.Int,
    -- | The protocol to use to route traffic to instances.
    --
    -- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
    instanceProtocol :: Prelude.Maybe Prelude.Text,
    -- | The port on which the load balancer is listening.
    --
    -- On EC2-VPC, you can specify any port from the range 1-65535.
    --
    -- On EC2-Classic, you can specify any port from the following list: 25,
    -- 80, 443, 465, 587, 1024-65535.
    loadBalancerPort :: Prelude.Maybe Prelude.Int,
    -- | The load balancer transport protocol to use for routing.
    --
    -- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the server certificate.
    sslCertificateId :: Prelude.Maybe Prelude.Text
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
-- 'instancePort', 'awsElbLoadBalancerListener_instancePort' - The port on which the instance is listening.
--
-- 'instanceProtocol', 'awsElbLoadBalancerListener_instanceProtocol' - The protocol to use to route traffic to instances.
--
-- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
--
-- 'loadBalancerPort', 'awsElbLoadBalancerListener_loadBalancerPort' - The port on which the load balancer is listening.
--
-- On EC2-VPC, you can specify any port from the range 1-65535.
--
-- On EC2-Classic, you can specify any port from the following list: 25,
-- 80, 443, 465, 587, 1024-65535.
--
-- 'protocol', 'awsElbLoadBalancerListener_protocol' - The load balancer transport protocol to use for routing.
--
-- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
--
-- 'sslCertificateId', 'awsElbLoadBalancerListener_sslCertificateId' - The ARN of the server certificate.
newAwsElbLoadBalancerListener ::
  AwsElbLoadBalancerListener
newAwsElbLoadBalancerListener =
  AwsElbLoadBalancerListener'
    { instancePort =
        Prelude.Nothing,
      instanceProtocol = Prelude.Nothing,
      loadBalancerPort = Prelude.Nothing,
      protocol = Prelude.Nothing,
      sslCertificateId = Prelude.Nothing
    }

-- | The port on which the instance is listening.
awsElbLoadBalancerListener_instancePort :: Lens.Lens' AwsElbLoadBalancerListener (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerListener_instancePort = Lens.lens (\AwsElbLoadBalancerListener' {instancePort} -> instancePort) (\s@AwsElbLoadBalancerListener' {} a -> s {instancePort = a} :: AwsElbLoadBalancerListener)

-- | The protocol to use to route traffic to instances.
--
-- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
awsElbLoadBalancerListener_instanceProtocol :: Lens.Lens' AwsElbLoadBalancerListener (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerListener_instanceProtocol = Lens.lens (\AwsElbLoadBalancerListener' {instanceProtocol} -> instanceProtocol) (\s@AwsElbLoadBalancerListener' {} a -> s {instanceProtocol = a} :: AwsElbLoadBalancerListener)

-- | The port on which the load balancer is listening.
--
-- On EC2-VPC, you can specify any port from the range 1-65535.
--
-- On EC2-Classic, you can specify any port from the following list: 25,
-- 80, 443, 465, 587, 1024-65535.
awsElbLoadBalancerListener_loadBalancerPort :: Lens.Lens' AwsElbLoadBalancerListener (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerListener_loadBalancerPort = Lens.lens (\AwsElbLoadBalancerListener' {loadBalancerPort} -> loadBalancerPort) (\s@AwsElbLoadBalancerListener' {} a -> s {loadBalancerPort = a} :: AwsElbLoadBalancerListener)

-- | The load balancer transport protocol to use for routing.
--
-- Valid values: @HTTP@ | @HTTPS@ | @TCP@ | @SSL@
awsElbLoadBalancerListener_protocol :: Lens.Lens' AwsElbLoadBalancerListener (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerListener_protocol = Lens.lens (\AwsElbLoadBalancerListener' {protocol} -> protocol) (\s@AwsElbLoadBalancerListener' {} a -> s {protocol = a} :: AwsElbLoadBalancerListener)

-- | The ARN of the server certificate.
awsElbLoadBalancerListener_sslCertificateId :: Lens.Lens' AwsElbLoadBalancerListener (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerListener_sslCertificateId = Lens.lens (\AwsElbLoadBalancerListener' {sslCertificateId} -> sslCertificateId) (\s@AwsElbLoadBalancerListener' {} a -> s {sslCertificateId = a} :: AwsElbLoadBalancerListener)

instance Data.FromJSON AwsElbLoadBalancerListener where
  parseJSON =
    Data.withObject
      "AwsElbLoadBalancerListener"
      ( \x ->
          AwsElbLoadBalancerListener'
            Prelude.<$> (x Data..:? "InstancePort")
            Prelude.<*> (x Data..:? "InstanceProtocol")
            Prelude.<*> (x Data..:? "LoadBalancerPort")
            Prelude.<*> (x Data..:? "Protocol")
            Prelude.<*> (x Data..:? "SslCertificateId")
      )

instance Prelude.Hashable AwsElbLoadBalancerListener where
  hashWithSalt _salt AwsElbLoadBalancerListener' {..} =
    _salt `Prelude.hashWithSalt` instancePort
      `Prelude.hashWithSalt` instanceProtocol
      `Prelude.hashWithSalt` loadBalancerPort
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` sslCertificateId

instance Prelude.NFData AwsElbLoadBalancerListener where
  rnf AwsElbLoadBalancerListener' {..} =
    Prelude.rnf instancePort
      `Prelude.seq` Prelude.rnf instanceProtocol
      `Prelude.seq` Prelude.rnf loadBalancerPort
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf sslCertificateId

instance Data.ToJSON AwsElbLoadBalancerListener where
  toJSON AwsElbLoadBalancerListener' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstancePort" Data..=) Prelude.<$> instancePort,
            ("InstanceProtocol" Data..=)
              Prelude.<$> instanceProtocol,
            ("LoadBalancerPort" Data..=)
              Prelude.<$> loadBalancerPort,
            ("Protocol" Data..=) Prelude.<$> protocol,
            ("SslCertificateId" Data..=)
              Prelude.<$> sslCertificateId
          ]
      )
