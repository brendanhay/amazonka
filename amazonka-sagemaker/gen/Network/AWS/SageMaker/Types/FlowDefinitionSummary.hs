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
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.FlowDefinitionStatus

-- | Contains summary information about the flow definition.
--
-- /See:/ 'newFlowDefinitionSummary' smart constructor.
data FlowDefinitionSummary = FlowDefinitionSummary'
  { -- | The reason why the flow definition creation failed. A failure reason is
    -- returned only when the flow definition status is @Failed@.
    failureReason :: Core.Maybe Core.Text,
    -- | The name of the flow definition.
    flowDefinitionName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the flow definition.
    flowDefinitionArn :: Core.Text,
    -- | The status of the flow definition. Valid values:
    flowDefinitionStatus :: FlowDefinitionStatus,
    -- | The timestamp when SageMaker created the flow definition.
    creationTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FlowDefinitionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'flowDefinitionSummary_failureReason' - The reason why the flow definition creation failed. A failure reason is
-- returned only when the flow definition status is @Failed@.
--
-- 'flowDefinitionName', 'flowDefinitionSummary_flowDefinitionName' - The name of the flow definition.
--
-- 'flowDefinitionArn', 'flowDefinitionSummary_flowDefinitionArn' - The Amazon Resource Name (ARN) of the flow definition.
--
-- 'flowDefinitionStatus', 'flowDefinitionSummary_flowDefinitionStatus' - The status of the flow definition. Valid values:
--
-- 'creationTime', 'flowDefinitionSummary_creationTime' - The timestamp when SageMaker created the flow definition.
newFlowDefinitionSummary ::
  -- | 'flowDefinitionName'
  Core.Text ->
  -- | 'flowDefinitionArn'
  Core.Text ->
  -- | 'flowDefinitionStatus'
  FlowDefinitionStatus ->
  -- | 'creationTime'
  Core.UTCTime ->
  FlowDefinitionSummary
newFlowDefinitionSummary
  pFlowDefinitionName_
  pFlowDefinitionArn_
  pFlowDefinitionStatus_
  pCreationTime_ =
    FlowDefinitionSummary'
      { failureReason =
          Core.Nothing,
        flowDefinitionName = pFlowDefinitionName_,
        flowDefinitionArn = pFlowDefinitionArn_,
        flowDefinitionStatus = pFlowDefinitionStatus_,
        creationTime = Core._Time Lens.# pCreationTime_
      }

-- | The reason why the flow definition creation failed. A failure reason is
-- returned only when the flow definition status is @Failed@.
flowDefinitionSummary_failureReason :: Lens.Lens' FlowDefinitionSummary (Core.Maybe Core.Text)
flowDefinitionSummary_failureReason = Lens.lens (\FlowDefinitionSummary' {failureReason} -> failureReason) (\s@FlowDefinitionSummary' {} a -> s {failureReason = a} :: FlowDefinitionSummary)

-- | The name of the flow definition.
flowDefinitionSummary_flowDefinitionName :: Lens.Lens' FlowDefinitionSummary Core.Text
flowDefinitionSummary_flowDefinitionName = Lens.lens (\FlowDefinitionSummary' {flowDefinitionName} -> flowDefinitionName) (\s@FlowDefinitionSummary' {} a -> s {flowDefinitionName = a} :: FlowDefinitionSummary)

-- | The Amazon Resource Name (ARN) of the flow definition.
flowDefinitionSummary_flowDefinitionArn :: Lens.Lens' FlowDefinitionSummary Core.Text
flowDefinitionSummary_flowDefinitionArn = Lens.lens (\FlowDefinitionSummary' {flowDefinitionArn} -> flowDefinitionArn) (\s@FlowDefinitionSummary' {} a -> s {flowDefinitionArn = a} :: FlowDefinitionSummary)

-- | The status of the flow definition. Valid values:
flowDefinitionSummary_flowDefinitionStatus :: Lens.Lens' FlowDefinitionSummary FlowDefinitionStatus
flowDefinitionSummary_flowDefinitionStatus = Lens.lens (\FlowDefinitionSummary' {flowDefinitionStatus} -> flowDefinitionStatus) (\s@FlowDefinitionSummary' {} a -> s {flowDefinitionStatus = a} :: FlowDefinitionSummary)

-- | The timestamp when SageMaker created the flow definition.
flowDefinitionSummary_creationTime :: Lens.Lens' FlowDefinitionSummary Core.UTCTime
flowDefinitionSummary_creationTime = Lens.lens (\FlowDefinitionSummary' {creationTime} -> creationTime) (\s@FlowDefinitionSummary' {} a -> s {creationTime = a} :: FlowDefinitionSummary) Core.. Core._Time

instance Core.FromJSON FlowDefinitionSummary where
  parseJSON =
    Core.withObject
      "FlowDefinitionSummary"
      ( \x ->
          FlowDefinitionSummary'
            Core.<$> (x Core..:? "FailureReason")
            Core.<*> (x Core..: "FlowDefinitionName")
            Core.<*> (x Core..: "FlowDefinitionArn")
            Core.<*> (x Core..: "FlowDefinitionStatus")
            Core.<*> (x Core..: "CreationTime")
      )

instance Core.Hashable FlowDefinitionSummary

instance Core.NFData FlowDefinitionSummary
