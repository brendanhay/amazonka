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
-- Module      : Network.AWS.EC2.Types.ProvisionedBandwidth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ProvisionedBandwidth where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
--
-- /See:/ 'newProvisionedBandwidth' smart constructor.
data ProvisionedBandwidth = ProvisionedBandwidth'
  { -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    provisionTime :: Core.Maybe Core.ISO8601,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    status :: Core.Maybe Core.Text,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    requestTime :: Core.Maybe Core.ISO8601,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    requested :: Core.Maybe Core.Text,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    provisioned :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisionedBandwidth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionTime', 'provisionedBandwidth_provisionTime' - Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
--
-- 'status', 'provisionedBandwidth_status' - Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
--
-- 'requestTime', 'provisionedBandwidth_requestTime' - Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
--
-- 'requested', 'provisionedBandwidth_requested' - Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
--
-- 'provisioned', 'provisionedBandwidth_provisioned' - Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
newProvisionedBandwidth ::
  ProvisionedBandwidth
newProvisionedBandwidth =
  ProvisionedBandwidth'
    { provisionTime = Core.Nothing,
      status = Core.Nothing,
      requestTime = Core.Nothing,
      requested = Core.Nothing,
      provisioned = Core.Nothing
    }

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
provisionedBandwidth_provisionTime :: Lens.Lens' ProvisionedBandwidth (Core.Maybe Core.UTCTime)
provisionedBandwidth_provisionTime = Lens.lens (\ProvisionedBandwidth' {provisionTime} -> provisionTime) (\s@ProvisionedBandwidth' {} a -> s {provisionTime = a} :: ProvisionedBandwidth) Core.. Lens.mapping Core._Time

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
provisionedBandwidth_status :: Lens.Lens' ProvisionedBandwidth (Core.Maybe Core.Text)
provisionedBandwidth_status = Lens.lens (\ProvisionedBandwidth' {status} -> status) (\s@ProvisionedBandwidth' {} a -> s {status = a} :: ProvisionedBandwidth)

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
provisionedBandwidth_requestTime :: Lens.Lens' ProvisionedBandwidth (Core.Maybe Core.UTCTime)
provisionedBandwidth_requestTime = Lens.lens (\ProvisionedBandwidth' {requestTime} -> requestTime) (\s@ProvisionedBandwidth' {} a -> s {requestTime = a} :: ProvisionedBandwidth) Core.. Lens.mapping Core._Time

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
provisionedBandwidth_requested :: Lens.Lens' ProvisionedBandwidth (Core.Maybe Core.Text)
provisionedBandwidth_requested = Lens.lens (\ProvisionedBandwidth' {requested} -> requested) (\s@ProvisionedBandwidth' {} a -> s {requested = a} :: ProvisionedBandwidth)

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
provisionedBandwidth_provisioned :: Lens.Lens' ProvisionedBandwidth (Core.Maybe Core.Text)
provisionedBandwidth_provisioned = Lens.lens (\ProvisionedBandwidth' {provisioned} -> provisioned) (\s@ProvisionedBandwidth' {} a -> s {provisioned = a} :: ProvisionedBandwidth)

instance Core.FromXML ProvisionedBandwidth where
  parseXML x =
    ProvisionedBandwidth'
      Core.<$> (x Core..@? "provisionTime")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "requestTime")
      Core.<*> (x Core..@? "requested")
      Core.<*> (x Core..@? "provisioned")

instance Core.Hashable ProvisionedBandwidth

instance Core.NFData ProvisionedBandwidth
