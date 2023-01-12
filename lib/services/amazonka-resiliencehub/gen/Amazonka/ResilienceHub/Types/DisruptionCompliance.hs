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
-- Module      : Amazonka.ResilienceHub.Types.DisruptionCompliance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.DisruptionCompliance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.ComplianceStatus

-- | Defines the compliance against the resiliency policy for a disruption.
--
-- /See:/ 'newDisruptionCompliance' smart constructor.
data DisruptionCompliance = DisruptionCompliance'
  { -- | The Recovery Point Objective (RPO) that is achievable, in seconds.
    achievableRpoInSecs :: Prelude.Maybe Prelude.Natural,
    -- | The Recovery Time Objective (RTO) that is achievable, in seconds
    achievableRtoInSecs :: Prelude.Maybe Prelude.Natural,
    -- | The current RPO, in seconds.
    currentRpoInSecs :: Prelude.Maybe Prelude.Natural,
    -- | The current RTO, in seconds.
    currentRtoInSecs :: Prelude.Maybe Prelude.Natural,
    -- | The disruption compliance message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The RPO description.
    rpoDescription :: Prelude.Maybe Prelude.Text,
    -- | The RPO reference identifier.
    rpoReferenceId :: Prelude.Maybe Prelude.Text,
    -- | The RTO description.
    rtoDescription :: Prelude.Maybe Prelude.Text,
    -- | The RTO reference identifier.
    rtoReferenceId :: Prelude.Maybe Prelude.Text,
    -- | The current status of compliance for the resiliency policy.
    complianceStatus :: ComplianceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisruptionCompliance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'achievableRpoInSecs', 'disruptionCompliance_achievableRpoInSecs' - The Recovery Point Objective (RPO) that is achievable, in seconds.
--
-- 'achievableRtoInSecs', 'disruptionCompliance_achievableRtoInSecs' - The Recovery Time Objective (RTO) that is achievable, in seconds
--
-- 'currentRpoInSecs', 'disruptionCompliance_currentRpoInSecs' - The current RPO, in seconds.
--
-- 'currentRtoInSecs', 'disruptionCompliance_currentRtoInSecs' - The current RTO, in seconds.
--
-- 'message', 'disruptionCompliance_message' - The disruption compliance message.
--
-- 'rpoDescription', 'disruptionCompliance_rpoDescription' - The RPO description.
--
-- 'rpoReferenceId', 'disruptionCompliance_rpoReferenceId' - The RPO reference identifier.
--
-- 'rtoDescription', 'disruptionCompliance_rtoDescription' - The RTO description.
--
-- 'rtoReferenceId', 'disruptionCompliance_rtoReferenceId' - The RTO reference identifier.
--
-- 'complianceStatus', 'disruptionCompliance_complianceStatus' - The current status of compliance for the resiliency policy.
newDisruptionCompliance ::
  -- | 'complianceStatus'
  ComplianceStatus ->
  DisruptionCompliance
newDisruptionCompliance pComplianceStatus_ =
  DisruptionCompliance'
    { achievableRpoInSecs =
        Prelude.Nothing,
      achievableRtoInSecs = Prelude.Nothing,
      currentRpoInSecs = Prelude.Nothing,
      currentRtoInSecs = Prelude.Nothing,
      message = Prelude.Nothing,
      rpoDescription = Prelude.Nothing,
      rpoReferenceId = Prelude.Nothing,
      rtoDescription = Prelude.Nothing,
      rtoReferenceId = Prelude.Nothing,
      complianceStatus = pComplianceStatus_
    }

-- | The Recovery Point Objective (RPO) that is achievable, in seconds.
disruptionCompliance_achievableRpoInSecs :: Lens.Lens' DisruptionCompliance (Prelude.Maybe Prelude.Natural)
disruptionCompliance_achievableRpoInSecs = Lens.lens (\DisruptionCompliance' {achievableRpoInSecs} -> achievableRpoInSecs) (\s@DisruptionCompliance' {} a -> s {achievableRpoInSecs = a} :: DisruptionCompliance)

-- | The Recovery Time Objective (RTO) that is achievable, in seconds
disruptionCompliance_achievableRtoInSecs :: Lens.Lens' DisruptionCompliance (Prelude.Maybe Prelude.Natural)
disruptionCompliance_achievableRtoInSecs = Lens.lens (\DisruptionCompliance' {achievableRtoInSecs} -> achievableRtoInSecs) (\s@DisruptionCompliance' {} a -> s {achievableRtoInSecs = a} :: DisruptionCompliance)

-- | The current RPO, in seconds.
disruptionCompliance_currentRpoInSecs :: Lens.Lens' DisruptionCompliance (Prelude.Maybe Prelude.Natural)
disruptionCompliance_currentRpoInSecs = Lens.lens (\DisruptionCompliance' {currentRpoInSecs} -> currentRpoInSecs) (\s@DisruptionCompliance' {} a -> s {currentRpoInSecs = a} :: DisruptionCompliance)

-- | The current RTO, in seconds.
disruptionCompliance_currentRtoInSecs :: Lens.Lens' DisruptionCompliance (Prelude.Maybe Prelude.Natural)
disruptionCompliance_currentRtoInSecs = Lens.lens (\DisruptionCompliance' {currentRtoInSecs} -> currentRtoInSecs) (\s@DisruptionCompliance' {} a -> s {currentRtoInSecs = a} :: DisruptionCompliance)

-- | The disruption compliance message.
disruptionCompliance_message :: Lens.Lens' DisruptionCompliance (Prelude.Maybe Prelude.Text)
disruptionCompliance_message = Lens.lens (\DisruptionCompliance' {message} -> message) (\s@DisruptionCompliance' {} a -> s {message = a} :: DisruptionCompliance)

-- | The RPO description.
disruptionCompliance_rpoDescription :: Lens.Lens' DisruptionCompliance (Prelude.Maybe Prelude.Text)
disruptionCompliance_rpoDescription = Lens.lens (\DisruptionCompliance' {rpoDescription} -> rpoDescription) (\s@DisruptionCompliance' {} a -> s {rpoDescription = a} :: DisruptionCompliance)

-- | The RPO reference identifier.
disruptionCompliance_rpoReferenceId :: Lens.Lens' DisruptionCompliance (Prelude.Maybe Prelude.Text)
disruptionCompliance_rpoReferenceId = Lens.lens (\DisruptionCompliance' {rpoReferenceId} -> rpoReferenceId) (\s@DisruptionCompliance' {} a -> s {rpoReferenceId = a} :: DisruptionCompliance)

-- | The RTO description.
disruptionCompliance_rtoDescription :: Lens.Lens' DisruptionCompliance (Prelude.Maybe Prelude.Text)
disruptionCompliance_rtoDescription = Lens.lens (\DisruptionCompliance' {rtoDescription} -> rtoDescription) (\s@DisruptionCompliance' {} a -> s {rtoDescription = a} :: DisruptionCompliance)

-- | The RTO reference identifier.
disruptionCompliance_rtoReferenceId :: Lens.Lens' DisruptionCompliance (Prelude.Maybe Prelude.Text)
disruptionCompliance_rtoReferenceId = Lens.lens (\DisruptionCompliance' {rtoReferenceId} -> rtoReferenceId) (\s@DisruptionCompliance' {} a -> s {rtoReferenceId = a} :: DisruptionCompliance)

-- | The current status of compliance for the resiliency policy.
disruptionCompliance_complianceStatus :: Lens.Lens' DisruptionCompliance ComplianceStatus
disruptionCompliance_complianceStatus = Lens.lens (\DisruptionCompliance' {complianceStatus} -> complianceStatus) (\s@DisruptionCompliance' {} a -> s {complianceStatus = a} :: DisruptionCompliance)

instance Data.FromJSON DisruptionCompliance where
  parseJSON =
    Data.withObject
      "DisruptionCompliance"
      ( \x ->
          DisruptionCompliance'
            Prelude.<$> (x Data..:? "achievableRpoInSecs")
            Prelude.<*> (x Data..:? "achievableRtoInSecs")
            Prelude.<*> (x Data..:? "currentRpoInSecs")
            Prelude.<*> (x Data..:? "currentRtoInSecs")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "rpoDescription")
            Prelude.<*> (x Data..:? "rpoReferenceId")
            Prelude.<*> (x Data..:? "rtoDescription")
            Prelude.<*> (x Data..:? "rtoReferenceId")
            Prelude.<*> (x Data..: "complianceStatus")
      )

instance Prelude.Hashable DisruptionCompliance where
  hashWithSalt _salt DisruptionCompliance' {..} =
    _salt `Prelude.hashWithSalt` achievableRpoInSecs
      `Prelude.hashWithSalt` achievableRtoInSecs
      `Prelude.hashWithSalt` currentRpoInSecs
      `Prelude.hashWithSalt` currentRtoInSecs
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` rpoDescription
      `Prelude.hashWithSalt` rpoReferenceId
      `Prelude.hashWithSalt` rtoDescription
      `Prelude.hashWithSalt` rtoReferenceId
      `Prelude.hashWithSalt` complianceStatus

instance Prelude.NFData DisruptionCompliance where
  rnf DisruptionCompliance' {..} =
    Prelude.rnf achievableRpoInSecs
      `Prelude.seq` Prelude.rnf achievableRtoInSecs
      `Prelude.seq` Prelude.rnf currentRpoInSecs
      `Prelude.seq` Prelude.rnf currentRtoInSecs
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf rpoDescription
      `Prelude.seq` Prelude.rnf rpoReferenceId
      `Prelude.seq` Prelude.rnf rtoDescription
      `Prelude.seq` Prelude.rnf rtoReferenceId
      `Prelude.seq` Prelude.rnf complianceStatus
