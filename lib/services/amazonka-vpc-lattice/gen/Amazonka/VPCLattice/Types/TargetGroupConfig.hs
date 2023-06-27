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
-- Module      : Amazonka.VPCLattice.Types.TargetGroupConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.TargetGroupConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.HealthCheckConfig
import Amazonka.VPCLattice.Types.IpAddressType
import Amazonka.VPCLattice.Types.TargetGroupProtocol
import Amazonka.VPCLattice.Types.TargetGroupProtocolVersion

-- | Describes the configuration of a target group. Lambda functions don\'t
-- support target group configuration.
--
-- /See:/ 'newTargetGroupConfig' smart constructor.
data TargetGroupConfig = TargetGroupConfig'
  { -- | The health check configuration.
    healthCheck :: Prelude.Maybe HealthCheckConfig,
    -- | The type of IP address used for the target group. The possible values
    -- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
    -- the IP address type defaults to @ipv4@.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The protocol version. Default value is @HTTP1@.
    protocolVersion :: Prelude.Maybe TargetGroupProtocolVersion,
    -- | The port on which the targets are listening. For HTTP, the default is
    -- @80@. For HTTPS, the default is @443@
    port :: Prelude.Natural,
    -- | The protocol to use for routing traffic to the targets. Default is the
    -- protocol of a target group.
    protocol :: TargetGroupProtocol,
    -- | The ID of the VPC.
    vpcIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetGroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheck', 'targetGroupConfig_healthCheck' - The health check configuration.
--
-- 'ipAddressType', 'targetGroupConfig_ipAddressType' - The type of IP address used for the target group. The possible values
-- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
-- the IP address type defaults to @ipv4@.
--
-- 'protocolVersion', 'targetGroupConfig_protocolVersion' - The protocol version. Default value is @HTTP1@.
--
-- 'port', 'targetGroupConfig_port' - The port on which the targets are listening. For HTTP, the default is
-- @80@. For HTTPS, the default is @443@
--
-- 'protocol', 'targetGroupConfig_protocol' - The protocol to use for routing traffic to the targets. Default is the
-- protocol of a target group.
--
-- 'vpcIdentifier', 'targetGroupConfig_vpcIdentifier' - The ID of the VPC.
newTargetGroupConfig ::
  -- | 'port'
  Prelude.Natural ->
  -- | 'protocol'
  TargetGroupProtocol ->
  -- | 'vpcIdentifier'
  Prelude.Text ->
  TargetGroupConfig
newTargetGroupConfig
  pPort_
  pProtocol_
  pVpcIdentifier_ =
    TargetGroupConfig'
      { healthCheck = Prelude.Nothing,
        ipAddressType = Prelude.Nothing,
        protocolVersion = Prelude.Nothing,
        port = pPort_,
        protocol = pProtocol_,
        vpcIdentifier = pVpcIdentifier_
      }

-- | The health check configuration.
targetGroupConfig_healthCheck :: Lens.Lens' TargetGroupConfig (Prelude.Maybe HealthCheckConfig)
targetGroupConfig_healthCheck = Lens.lens (\TargetGroupConfig' {healthCheck} -> healthCheck) (\s@TargetGroupConfig' {} a -> s {healthCheck = a} :: TargetGroupConfig)

-- | The type of IP address used for the target group. The possible values
-- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
-- the IP address type defaults to @ipv4@.
targetGroupConfig_ipAddressType :: Lens.Lens' TargetGroupConfig (Prelude.Maybe IpAddressType)
targetGroupConfig_ipAddressType = Lens.lens (\TargetGroupConfig' {ipAddressType} -> ipAddressType) (\s@TargetGroupConfig' {} a -> s {ipAddressType = a} :: TargetGroupConfig)

-- | The protocol version. Default value is @HTTP1@.
targetGroupConfig_protocolVersion :: Lens.Lens' TargetGroupConfig (Prelude.Maybe TargetGroupProtocolVersion)
targetGroupConfig_protocolVersion = Lens.lens (\TargetGroupConfig' {protocolVersion} -> protocolVersion) (\s@TargetGroupConfig' {} a -> s {protocolVersion = a} :: TargetGroupConfig)

-- | The port on which the targets are listening. For HTTP, the default is
-- @80@. For HTTPS, the default is @443@
targetGroupConfig_port :: Lens.Lens' TargetGroupConfig Prelude.Natural
targetGroupConfig_port = Lens.lens (\TargetGroupConfig' {port} -> port) (\s@TargetGroupConfig' {} a -> s {port = a} :: TargetGroupConfig)

-- | The protocol to use for routing traffic to the targets. Default is the
-- protocol of a target group.
targetGroupConfig_protocol :: Lens.Lens' TargetGroupConfig TargetGroupProtocol
targetGroupConfig_protocol = Lens.lens (\TargetGroupConfig' {protocol} -> protocol) (\s@TargetGroupConfig' {} a -> s {protocol = a} :: TargetGroupConfig)

-- | The ID of the VPC.
targetGroupConfig_vpcIdentifier :: Lens.Lens' TargetGroupConfig Prelude.Text
targetGroupConfig_vpcIdentifier = Lens.lens (\TargetGroupConfig' {vpcIdentifier} -> vpcIdentifier) (\s@TargetGroupConfig' {} a -> s {vpcIdentifier = a} :: TargetGroupConfig)

instance Data.FromJSON TargetGroupConfig where
  parseJSON =
    Data.withObject
      "TargetGroupConfig"
      ( \x ->
          TargetGroupConfig'
            Prelude.<$> (x Data..:? "healthCheck")
            Prelude.<*> (x Data..:? "ipAddressType")
            Prelude.<*> (x Data..:? "protocolVersion")
            Prelude.<*> (x Data..: "port")
            Prelude.<*> (x Data..: "protocol")
            Prelude.<*> (x Data..: "vpcIdentifier")
      )

instance Prelude.Hashable TargetGroupConfig where
  hashWithSalt _salt TargetGroupConfig' {..} =
    _salt
      `Prelude.hashWithSalt` healthCheck
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` protocolVersion
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` vpcIdentifier

instance Prelude.NFData TargetGroupConfig where
  rnf TargetGroupConfig' {..} =
    Prelude.rnf healthCheck
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf protocolVersion
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf vpcIdentifier

instance Data.ToJSON TargetGroupConfig where
  toJSON TargetGroupConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("healthCheck" Data..=) Prelude.<$> healthCheck,
            ("ipAddressType" Data..=) Prelude.<$> ipAddressType,
            ("protocolVersion" Data..=)
              Prelude.<$> protocolVersion,
            Prelude.Just ("port" Data..= port),
            Prelude.Just ("protocol" Data..= protocol),
            Prelude.Just
              ("vpcIdentifier" Data..= vpcIdentifier)
          ]
      )
