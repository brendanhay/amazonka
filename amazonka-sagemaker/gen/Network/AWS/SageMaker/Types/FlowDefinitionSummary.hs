{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.FlowDefinitionStatus

-- | Contains summary information about the flow definition.
--
-- /See:/ 'newFlowDefinitionSummary' smart constructor.
data FlowDefinitionSummary = FlowDefinitionSummary'
  { -- | The reason why the flow definition creation failed. A failure reason is
    -- returned only when the flow definition status is @Failed@.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the flow definition.
    flowDefinitionName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the flow definition.
    flowDefinitionArn :: Prelude.Text,
    -- | The status of the flow definition. Valid values:
    flowDefinitionStatus :: FlowDefinitionStatus,
    -- | The timestamp when SageMaker created the flow definition.
    creationTime :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'flowDefinitionArn'
  Prelude.Text ->
  -- | 'flowDefinitionStatus'
  FlowDefinitionStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  FlowDefinitionSummary
newFlowDefinitionSummary
  pFlowDefinitionName_
  pFlowDefinitionArn_
  pFlowDefinitionStatus_
  pCreationTime_ =
    FlowDefinitionSummary'
      { failureReason =
          Prelude.Nothing,
        flowDefinitionName = pFlowDefinitionName_,
        flowDefinitionArn = pFlowDefinitionArn_,
        flowDefinitionStatus = pFlowDefinitionStatus_,
        creationTime = Prelude._Time Lens.# pCreationTime_
      }

-- | The reason why the flow definition creation failed. A failure reason is
-- returned only when the flow definition status is @Failed@.
flowDefinitionSummary_failureReason :: Lens.Lens' FlowDefinitionSummary (Prelude.Maybe Prelude.Text)
flowDefinitionSummary_failureReason = Lens.lens (\FlowDefinitionSummary' {failureReason} -> failureReason) (\s@FlowDefinitionSummary' {} a -> s {failureReason = a} :: FlowDefinitionSummary)

-- | The name of the flow definition.
flowDefinitionSummary_flowDefinitionName :: Lens.Lens' FlowDefinitionSummary Prelude.Text
flowDefinitionSummary_flowDefinitionName = Lens.lens (\FlowDefinitionSummary' {flowDefinitionName} -> flowDefinitionName) (\s@FlowDefinitionSummary' {} a -> s {flowDefinitionName = a} :: FlowDefinitionSummary)

-- | The Amazon Resource Name (ARN) of the flow definition.
flowDefinitionSummary_flowDefinitionArn :: Lens.Lens' FlowDefinitionSummary Prelude.Text
flowDefinitionSummary_flowDefinitionArn = Lens.lens (\FlowDefinitionSummary' {flowDefinitionArn} -> flowDefinitionArn) (\s@FlowDefinitionSummary' {} a -> s {flowDefinitionArn = a} :: FlowDefinitionSummary)

-- | The status of the flow definition. Valid values:
flowDefinitionSummary_flowDefinitionStatus :: Lens.Lens' FlowDefinitionSummary FlowDefinitionStatus
flowDefinitionSummary_flowDefinitionStatus = Lens.lens (\FlowDefinitionSummary' {flowDefinitionStatus} -> flowDefinitionStatus) (\s@FlowDefinitionSummary' {} a -> s {flowDefinitionStatus = a} :: FlowDefinitionSummary)

-- | The timestamp when SageMaker created the flow definition.
flowDefinitionSummary_creationTime :: Lens.Lens' FlowDefinitionSummary Prelude.UTCTime
flowDefinitionSummary_creationTime = Lens.lens (\FlowDefinitionSummary' {creationTime} -> creationTime) (\s@FlowDefinitionSummary' {} a -> s {creationTime = a} :: FlowDefinitionSummary) Prelude.. Prelude._Time

instance Prelude.FromJSON FlowDefinitionSummary where
  parseJSON =
    Prelude.withObject
      "FlowDefinitionSummary"
      ( \x ->
          FlowDefinitionSummary'
            Prelude.<$> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..: "FlowDefinitionName")
            Prelude.<*> (x Prelude..: "FlowDefinitionArn")
            Prelude.<*> (x Prelude..: "FlowDefinitionStatus")
            Prelude.<*> (x Prelude..: "CreationTime")
      )

instance Prelude.Hashable FlowDefinitionSummary

instance Prelude.NFData FlowDefinitionSummary
