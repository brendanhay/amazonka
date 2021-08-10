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
-- Module      : Network.AWS.GameLift.Types.IpPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.IpPermission where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.IpProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A range of IP addresses and port settings that allow inbound traffic to
-- connect to server processes on an Amazon GameLift hosting resource. New
-- game sessions that are started on the fleet are assigned an IP
-- address\/port number combination, which must fall into the fleet\'s
-- allowed ranges. For fleets created with a custom game server, the ranges
-- reflect the server\'s game session assignments. For Realtime Servers
-- fleets, Amazon GameLift automatically opens two port ranges, one for TCP
-- messaging and one for UDP for use by the Realtime servers.
--
-- /See:/ 'newIpPermission' smart constructor.
data IpPermission = IpPermission'
  { -- | A starting value for a range of allowed port numbers.
    fromPort :: Prelude.Natural,
    -- | An ending value for a range of allowed port numbers. Port numbers are
    -- end-inclusive. This value must be higher than @FromPort@.
    toPort :: Prelude.Natural,
    -- | A range of allowed IP addresses. This value must be expressed in CIDR
    -- notation. Example: \"@000.000.000.000\/[subnet mask]@\" or optionally
    -- the shortened version \"@0.0.0.0\/[subnet mask]@\".
    ipRange :: Prelude.Text,
    -- | The network communication protocol used by the fleet.
    protocol :: IpProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'ipPermission_fromPort' - A starting value for a range of allowed port numbers.
--
-- 'toPort', 'ipPermission_toPort' - An ending value for a range of allowed port numbers. Port numbers are
-- end-inclusive. This value must be higher than @FromPort@.
--
-- 'ipRange', 'ipPermission_ipRange' - A range of allowed IP addresses. This value must be expressed in CIDR
-- notation. Example: \"@000.000.000.000\/[subnet mask]@\" or optionally
-- the shortened version \"@0.0.0.0\/[subnet mask]@\".
--
-- 'protocol', 'ipPermission_protocol' - The network communication protocol used by the fleet.
newIpPermission ::
  -- | 'fromPort'
  Prelude.Natural ->
  -- | 'toPort'
  Prelude.Natural ->
  -- | 'ipRange'
  Prelude.Text ->
  -- | 'protocol'
  IpProtocol ->
  IpPermission
newIpPermission
  pFromPort_
  pToPort_
  pIpRange_
  pProtocol_ =
    IpPermission'
      { fromPort = pFromPort_,
        toPort = pToPort_,
        ipRange = pIpRange_,
        protocol = pProtocol_
      }

-- | A starting value for a range of allowed port numbers.
ipPermission_fromPort :: Lens.Lens' IpPermission Prelude.Natural
ipPermission_fromPort = Lens.lens (\IpPermission' {fromPort} -> fromPort) (\s@IpPermission' {} a -> s {fromPort = a} :: IpPermission)

-- | An ending value for a range of allowed port numbers. Port numbers are
-- end-inclusive. This value must be higher than @FromPort@.
ipPermission_toPort :: Lens.Lens' IpPermission Prelude.Natural
ipPermission_toPort = Lens.lens (\IpPermission' {toPort} -> toPort) (\s@IpPermission' {} a -> s {toPort = a} :: IpPermission)

-- | A range of allowed IP addresses. This value must be expressed in CIDR
-- notation. Example: \"@000.000.000.000\/[subnet mask]@\" or optionally
-- the shortened version \"@0.0.0.0\/[subnet mask]@\".
ipPermission_ipRange :: Lens.Lens' IpPermission Prelude.Text
ipPermission_ipRange = Lens.lens (\IpPermission' {ipRange} -> ipRange) (\s@IpPermission' {} a -> s {ipRange = a} :: IpPermission)

-- | The network communication protocol used by the fleet.
ipPermission_protocol :: Lens.Lens' IpPermission IpProtocol
ipPermission_protocol = Lens.lens (\IpPermission' {protocol} -> protocol) (\s@IpPermission' {} a -> s {protocol = a} :: IpPermission)

instance Core.FromJSON IpPermission where
  parseJSON =
    Core.withObject
      "IpPermission"
      ( \x ->
          IpPermission'
            Prelude.<$> (x Core..: "FromPort")
            Prelude.<*> (x Core..: "ToPort")
            Prelude.<*> (x Core..: "IpRange")
            Prelude.<*> (x Core..: "Protocol")
      )

instance Prelude.Hashable IpPermission

instance Prelude.NFData IpPermission

instance Core.ToJSON IpPermission where
  toJSON IpPermission' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FromPort" Core..= fromPort),
            Prelude.Just ("ToPort" Core..= toPort),
            Prelude.Just ("IpRange" Core..= ipRange),
            Prelude.Just ("Protocol" Core..= protocol)
          ]
      )
