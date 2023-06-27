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
-- Module      : Amazonka.VPCLattice.Types.TargetSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.TargetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.TargetStatus

-- | Summary information about a target.
--
-- /See:/ 'newTargetSummary' smart constructor.
data TargetSummary = TargetSummary'
  { -- | The ID of the target. If the target type of the target group is
    -- @INSTANCE@, this is an instance ID. If the target type is @IP@ , this is
    -- an IP address. If the target type is @LAMBDA@, this is the ARN of the
    -- Lambda function. If the target type is @ALB@, this is the ARN of the
    -- Application Load Balancer.
    id :: Prelude.Maybe Prelude.Text,
    -- | The port on which the target is listening.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The code for why the target status is what it is.
    reasonCode :: Prelude.Maybe Prelude.Text,
    -- | The status of the target.
    --
    -- -   @Draining@: The target is being deregistered. No new connections
    --     will be sent to this target while current connections are being
    --     drained. Default draining time is 5 minutes.
    --
    -- -   @Unavailable@: Health checks are unavailable for the target group.
    --
    -- -   @Healthy@: The target is healthy.
    --
    -- -   @Unhealthy@: The target is unhealthy.
    --
    -- -   @Initial@: Initial health checks on the target are being performed.
    --
    -- -   @Unused@: Target group is not used in a service.
    status :: Prelude.Maybe TargetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'targetSummary_id' - The ID of the target. If the target type of the target group is
-- @INSTANCE@, this is an instance ID. If the target type is @IP@ , this is
-- an IP address. If the target type is @LAMBDA@, this is the ARN of the
-- Lambda function. If the target type is @ALB@, this is the ARN of the
-- Application Load Balancer.
--
-- 'port', 'targetSummary_port' - The port on which the target is listening.
--
-- 'reasonCode', 'targetSummary_reasonCode' - The code for why the target status is what it is.
--
-- 'status', 'targetSummary_status' - The status of the target.
--
-- -   @Draining@: The target is being deregistered. No new connections
--     will be sent to this target while current connections are being
--     drained. Default draining time is 5 minutes.
--
-- -   @Unavailable@: Health checks are unavailable for the target group.
--
-- -   @Healthy@: The target is healthy.
--
-- -   @Unhealthy@: The target is unhealthy.
--
-- -   @Initial@: Initial health checks on the target are being performed.
--
-- -   @Unused@: Target group is not used in a service.
newTargetSummary ::
  TargetSummary
newTargetSummary =
  TargetSummary'
    { id = Prelude.Nothing,
      port = Prelude.Nothing,
      reasonCode = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ID of the target. If the target type of the target group is
-- @INSTANCE@, this is an instance ID. If the target type is @IP@ , this is
-- an IP address. If the target type is @LAMBDA@, this is the ARN of the
-- Lambda function. If the target type is @ALB@, this is the ARN of the
-- Application Load Balancer.
targetSummary_id :: Lens.Lens' TargetSummary (Prelude.Maybe Prelude.Text)
targetSummary_id = Lens.lens (\TargetSummary' {id} -> id) (\s@TargetSummary' {} a -> s {id = a} :: TargetSummary)

-- | The port on which the target is listening.
targetSummary_port :: Lens.Lens' TargetSummary (Prelude.Maybe Prelude.Natural)
targetSummary_port = Lens.lens (\TargetSummary' {port} -> port) (\s@TargetSummary' {} a -> s {port = a} :: TargetSummary)

-- | The code for why the target status is what it is.
targetSummary_reasonCode :: Lens.Lens' TargetSummary (Prelude.Maybe Prelude.Text)
targetSummary_reasonCode = Lens.lens (\TargetSummary' {reasonCode} -> reasonCode) (\s@TargetSummary' {} a -> s {reasonCode = a} :: TargetSummary)

-- | The status of the target.
--
-- -   @Draining@: The target is being deregistered. No new connections
--     will be sent to this target while current connections are being
--     drained. Default draining time is 5 minutes.
--
-- -   @Unavailable@: Health checks are unavailable for the target group.
--
-- -   @Healthy@: The target is healthy.
--
-- -   @Unhealthy@: The target is unhealthy.
--
-- -   @Initial@: Initial health checks on the target are being performed.
--
-- -   @Unused@: Target group is not used in a service.
targetSummary_status :: Lens.Lens' TargetSummary (Prelude.Maybe TargetStatus)
targetSummary_status = Lens.lens (\TargetSummary' {status} -> status) (\s@TargetSummary' {} a -> s {status = a} :: TargetSummary)

instance Data.FromJSON TargetSummary where
  parseJSON =
    Data.withObject
      "TargetSummary"
      ( \x ->
          TargetSummary'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "reasonCode")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable TargetSummary where
  hashWithSalt _salt TargetSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` reasonCode
      `Prelude.hashWithSalt` status

instance Prelude.NFData TargetSummary where
  rnf TargetSummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf reasonCode
      `Prelude.seq` Prelude.rnf status
