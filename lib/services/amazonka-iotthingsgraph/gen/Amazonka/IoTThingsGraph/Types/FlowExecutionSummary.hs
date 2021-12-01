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
-- Module      : Amazonka.IoTThingsGraph.Types.FlowExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.FlowExecutionSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.FlowExecutionStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains summary information about a flow execution.
--
-- /See:/ 'newFlowExecutionSummary' smart constructor.
data FlowExecutionSummary = FlowExecutionSummary'
  { -- | The current status of the flow execution.
    status :: Prelude.Maybe FlowExecutionStatus,
    -- | The ID of the flow.
    flowTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the flow execution summary was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the flow execution.
    flowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the system instance that contains the flow.
    systemInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the flow execution summary was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlowExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'flowExecutionSummary_status' - The current status of the flow execution.
--
-- 'flowTemplateId', 'flowExecutionSummary_flowTemplateId' - The ID of the flow.
--
-- 'createdAt', 'flowExecutionSummary_createdAt' - The date and time when the flow execution summary was created.
--
-- 'flowExecutionId', 'flowExecutionSummary_flowExecutionId' - The ID of the flow execution.
--
-- 'systemInstanceId', 'flowExecutionSummary_systemInstanceId' - The ID of the system instance that contains the flow.
--
-- 'updatedAt', 'flowExecutionSummary_updatedAt' - The date and time when the flow execution summary was last updated.
newFlowExecutionSummary ::
  FlowExecutionSummary
newFlowExecutionSummary =
  FlowExecutionSummary'
    { status = Prelude.Nothing,
      flowTemplateId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      flowExecutionId = Prelude.Nothing,
      systemInstanceId = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The current status of the flow execution.
flowExecutionSummary_status :: Lens.Lens' FlowExecutionSummary (Prelude.Maybe FlowExecutionStatus)
flowExecutionSummary_status = Lens.lens (\FlowExecutionSummary' {status} -> status) (\s@FlowExecutionSummary' {} a -> s {status = a} :: FlowExecutionSummary)

-- | The ID of the flow.
flowExecutionSummary_flowTemplateId :: Lens.Lens' FlowExecutionSummary (Prelude.Maybe Prelude.Text)
flowExecutionSummary_flowTemplateId = Lens.lens (\FlowExecutionSummary' {flowTemplateId} -> flowTemplateId) (\s@FlowExecutionSummary' {} a -> s {flowTemplateId = a} :: FlowExecutionSummary)

-- | The date and time when the flow execution summary was created.
flowExecutionSummary_createdAt :: Lens.Lens' FlowExecutionSummary (Prelude.Maybe Prelude.UTCTime)
flowExecutionSummary_createdAt = Lens.lens (\FlowExecutionSummary' {createdAt} -> createdAt) (\s@FlowExecutionSummary' {} a -> s {createdAt = a} :: FlowExecutionSummary) Prelude.. Lens.mapping Core._Time

-- | The ID of the flow execution.
flowExecutionSummary_flowExecutionId :: Lens.Lens' FlowExecutionSummary (Prelude.Maybe Prelude.Text)
flowExecutionSummary_flowExecutionId = Lens.lens (\FlowExecutionSummary' {flowExecutionId} -> flowExecutionId) (\s@FlowExecutionSummary' {} a -> s {flowExecutionId = a} :: FlowExecutionSummary)

-- | The ID of the system instance that contains the flow.
flowExecutionSummary_systemInstanceId :: Lens.Lens' FlowExecutionSummary (Prelude.Maybe Prelude.Text)
flowExecutionSummary_systemInstanceId = Lens.lens (\FlowExecutionSummary' {systemInstanceId} -> systemInstanceId) (\s@FlowExecutionSummary' {} a -> s {systemInstanceId = a} :: FlowExecutionSummary)

-- | The date and time when the flow execution summary was last updated.
flowExecutionSummary_updatedAt :: Lens.Lens' FlowExecutionSummary (Prelude.Maybe Prelude.UTCTime)
flowExecutionSummary_updatedAt = Lens.lens (\FlowExecutionSummary' {updatedAt} -> updatedAt) (\s@FlowExecutionSummary' {} a -> s {updatedAt = a} :: FlowExecutionSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON FlowExecutionSummary where
  parseJSON =
    Core.withObject
      "FlowExecutionSummary"
      ( \x ->
          FlowExecutionSummary'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "flowTemplateId")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "flowExecutionId")
            Prelude.<*> (x Core..:? "systemInstanceId")
            Prelude.<*> (x Core..:? "updatedAt")
      )

instance Prelude.Hashable FlowExecutionSummary where
  hashWithSalt salt' FlowExecutionSummary' {..} =
    salt' `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` systemInstanceId
      `Prelude.hashWithSalt` flowExecutionId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` flowTemplateId
      `Prelude.hashWithSalt` status

instance Prelude.NFData FlowExecutionSummary where
  rnf FlowExecutionSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf systemInstanceId
      `Prelude.seq` Prelude.rnf flowExecutionId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf flowTemplateId
