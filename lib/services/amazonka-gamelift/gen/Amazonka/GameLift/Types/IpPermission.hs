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
-- Module      : Amazonka.GameLift.Types.IpPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.IpPermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.IpProtocol
import qualified Amazonka.Prelude as Prelude

-- | A range of IP addresses and port settings that allow inbound traffic to
-- connect to server processes on an instance in a fleet. New game sessions
-- are assigned an IP address\/port number combination, which must fall
-- into the fleet\'s allowed ranges. Fleets with custom game builds must
-- have permissions explicitly set. For Realtime Servers fleets, GameLift
-- automatically opens two port ranges, one for TCP messaging and one for
-- UDP.
--
-- /See:/ 'newIpPermission' smart constructor.
data IpPermission = IpPermission'
  { -- | A starting value for a range of allowed port numbers.
    --
    -- For fleets using Windows and Linux builds, only ports 1026-60000 are
    -- valid.
    fromPort :: Prelude.Natural,
    -- | An ending value for a range of allowed port numbers. Port numbers are
    -- end-inclusive. This value must be higher than @FromPort@.
    --
    -- For fleets using Windows and Linux builds, only ports 1026-60000 are
    -- valid.
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
-- For fleets using Windows and Linux builds, only ports 1026-60000 are
-- valid.
--
-- 'toPort', 'ipPermission_toPort' - An ending value for a range of allowed port numbers. Port numbers are
-- end-inclusive. This value must be higher than @FromPort@.
--
-- For fleets using Windows and Linux builds, only ports 1026-60000 are
-- valid.
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
--
-- For fleets using Windows and Linux builds, only ports 1026-60000 are
-- valid.
ipPermission_fromPort :: Lens.Lens' IpPermission Prelude.Natural
ipPermission_fromPort = Lens.lens (\IpPermission' {fromPort} -> fromPort) (\s@IpPermission' {} a -> s {fromPort = a} :: IpPermission)

-- | An ending value for a range of allowed port numbers. Port numbers are
-- end-inclusive. This value must be higher than @FromPort@.
--
-- For fleets using Windows and Linux builds, only ports 1026-60000 are
-- valid.
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

instance Data.FromJSON IpPermission where
  parseJSON =
    Data.withObject
      "IpPermission"
      ( \x ->
          IpPermission'
            Prelude.<$> (x Data..: "FromPort")
            Prelude.<*> (x Data..: "ToPort")
            Prelude.<*> (x Data..: "IpRange")
            Prelude.<*> (x Data..: "Protocol")
      )

instance Prelude.Hashable IpPermission where
  hashWithSalt _salt IpPermission' {..} =
    _salt
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` toPort
      `Prelude.hashWithSalt` ipRange
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData IpPermission where
  rnf IpPermission' {..} =
    Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf toPort
      `Prelude.seq` Prelude.rnf ipRange
      `Prelude.seq` Prelude.rnf protocol

instance Data.ToJSON IpPermission where
  toJSON IpPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FromPort" Data..= fromPort),
            Prelude.Just ("ToPort" Data..= toPort),
            Prelude.Just ("IpRange" Data..= ipRange),
            Prelude.Just ("Protocol" Data..= protocol)
          ]
      )
