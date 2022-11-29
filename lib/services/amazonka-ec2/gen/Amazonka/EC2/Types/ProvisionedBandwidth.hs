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
-- Module      : Amazonka.EC2.Types.ProvisionedBandwidth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ProvisionedBandwidth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

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
    requested :: Prelude.Maybe Prelude.Text,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    provisionTime :: Prelude.Maybe Core.ISO8601,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    requestTime :: Prelude.Maybe Core.ISO8601,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    status :: Prelude.Maybe Prelude.Text,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    provisioned :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedBandwidth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requested', 'provisionedBandwidth_requested' - Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
--
-- 'provisionTime', 'provisionedBandwidth_provisionTime' - Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
--
-- 'requestTime', 'provisionedBandwidth_requestTime' - Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
--
-- 'status', 'provisionedBandwidth_status' - Reserved. If you need to sustain traffic greater than the
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
    { requested = Prelude.Nothing,
      provisionTime = Prelude.Nothing,
      requestTime = Prelude.Nothing,
      status = Prelude.Nothing,
      provisioned = Prelude.Nothing
    }

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
provisionedBandwidth_requested :: Lens.Lens' ProvisionedBandwidth (Prelude.Maybe Prelude.Text)
provisionedBandwidth_requested = Lens.lens (\ProvisionedBandwidth' {requested} -> requested) (\s@ProvisionedBandwidth' {} a -> s {requested = a} :: ProvisionedBandwidth)

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
provisionedBandwidth_provisionTime :: Lens.Lens' ProvisionedBandwidth (Prelude.Maybe Prelude.UTCTime)
provisionedBandwidth_provisionTime = Lens.lens (\ProvisionedBandwidth' {provisionTime} -> provisionTime) (\s@ProvisionedBandwidth' {} a -> s {provisionTime = a} :: ProvisionedBandwidth) Prelude.. Lens.mapping Core._Time

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
provisionedBandwidth_requestTime :: Lens.Lens' ProvisionedBandwidth (Prelude.Maybe Prelude.UTCTime)
provisionedBandwidth_requestTime = Lens.lens (\ProvisionedBandwidth' {requestTime} -> requestTime) (\s@ProvisionedBandwidth' {} a -> s {requestTime = a} :: ProvisionedBandwidth) Prelude.. Lens.mapping Core._Time

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
provisionedBandwidth_status :: Lens.Lens' ProvisionedBandwidth (Prelude.Maybe Prelude.Text)
provisionedBandwidth_status = Lens.lens (\ProvisionedBandwidth' {status} -> status) (\s@ProvisionedBandwidth' {} a -> s {status = a} :: ProvisionedBandwidth)

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
provisionedBandwidth_provisioned :: Lens.Lens' ProvisionedBandwidth (Prelude.Maybe Prelude.Text)
provisionedBandwidth_provisioned = Lens.lens (\ProvisionedBandwidth' {provisioned} -> provisioned) (\s@ProvisionedBandwidth' {} a -> s {provisioned = a} :: ProvisionedBandwidth)

instance Core.FromXML ProvisionedBandwidth where
  parseXML x =
    ProvisionedBandwidth'
      Prelude.<$> (x Core..@? "requested")
      Prelude.<*> (x Core..@? "provisionTime")
      Prelude.<*> (x Core..@? "requestTime")
      Prelude.<*> (x Core..@? "status")
      Prelude.<*> (x Core..@? "provisioned")

instance Prelude.Hashable ProvisionedBandwidth where
  hashWithSalt _salt ProvisionedBandwidth' {..} =
    _salt `Prelude.hashWithSalt` requested
      `Prelude.hashWithSalt` provisionTime
      `Prelude.hashWithSalt` requestTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` provisioned

instance Prelude.NFData ProvisionedBandwidth where
  rnf ProvisionedBandwidth' {..} =
    Prelude.rnf requested
      `Prelude.seq` Prelude.rnf provisionTime
      `Prelude.seq` Prelude.rnf requestTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf provisioned
