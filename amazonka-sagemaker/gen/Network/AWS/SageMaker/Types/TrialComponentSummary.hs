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
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.TrialComponentStatus
import Network.AWS.SageMaker.Types.UserContext

-- | A summary of the properties of a trial component. To get all the
-- properties, call the DescribeTrialComponent API and provide the
-- @TrialComponentName@.
--
-- /See:/ 'newTrialComponentSummary' smart constructor.
data TrialComponentSummary = TrialComponentSummary'
  { -- | The status of the component. States include:
    --
    -- -   InProgress
    --
    -- -   Completed
    --
    -- -   Failed
    status :: Core.Maybe TrialComponentStatus,
    -- | When the component was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the trial component.
    trialComponentArn :: Core.Maybe Core.Text,
    -- | When the component started.
    startTime :: Core.Maybe Core.POSIX,
    -- | When the component ended.
    endTime :: Core.Maybe Core.POSIX,
    -- | When the component was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | Who created the component.
    createdBy :: Core.Maybe UserContext,
    -- | Who last modified the component.
    lastModifiedBy :: Core.Maybe UserContext,
    trialComponentSource :: Core.Maybe TrialComponentSource,
    -- | The name of the component as displayed. If @DisplayName@ isn\'t
    -- specified, @TrialComponentName@ is displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The name of the trial component.
    trialComponentName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrialComponentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'trialComponentSummary_status' - The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
--
-- 'creationTime', 'trialComponentSummary_creationTime' - When the component was created.
--
-- 'trialComponentArn', 'trialComponentSummary_trialComponentArn' - The ARN of the trial component.
--
-- 'startTime', 'trialComponentSummary_startTime' - When the component started.
--
-- 'endTime', 'trialComponentSummary_endTime' - When the component ended.
--
-- 'lastModifiedTime', 'trialComponentSummary_lastModifiedTime' - When the component was last modified.
--
-- 'createdBy', 'trialComponentSummary_createdBy' - Who created the component.
--
-- 'lastModifiedBy', 'trialComponentSummary_lastModifiedBy' - Who last modified the component.
--
-- 'trialComponentSource', 'trialComponentSummary_trialComponentSource' - Undocumented member.
--
-- 'displayName', 'trialComponentSummary_displayName' - The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
--
-- 'trialComponentName', 'trialComponentSummary_trialComponentName' - The name of the trial component.
newTrialComponentSummary ::
  TrialComponentSummary
newTrialComponentSummary =
  TrialComponentSummary'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      trialComponentArn = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      trialComponentSource = Core.Nothing,
      displayName = Core.Nothing,
      trialComponentName = Core.Nothing
    }

-- | The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
trialComponentSummary_status :: Lens.Lens' TrialComponentSummary (Core.Maybe TrialComponentStatus)
trialComponentSummary_status = Lens.lens (\TrialComponentSummary' {status} -> status) (\s@TrialComponentSummary' {} a -> s {status = a} :: TrialComponentSummary)

-- | When the component was created.
trialComponentSummary_creationTime :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.UTCTime)
trialComponentSummary_creationTime = Lens.lens (\TrialComponentSummary' {creationTime} -> creationTime) (\s@TrialComponentSummary' {} a -> s {creationTime = a} :: TrialComponentSummary) Core.. Lens.mapping Core._Time

-- | The ARN of the trial component.
trialComponentSummary_trialComponentArn :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.Text)
trialComponentSummary_trialComponentArn = Lens.lens (\TrialComponentSummary' {trialComponentArn} -> trialComponentArn) (\s@TrialComponentSummary' {} a -> s {trialComponentArn = a} :: TrialComponentSummary)

-- | When the component started.
trialComponentSummary_startTime :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.UTCTime)
trialComponentSummary_startTime = Lens.lens (\TrialComponentSummary' {startTime} -> startTime) (\s@TrialComponentSummary' {} a -> s {startTime = a} :: TrialComponentSummary) Core.. Lens.mapping Core._Time

-- | When the component ended.
trialComponentSummary_endTime :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.UTCTime)
trialComponentSummary_endTime = Lens.lens (\TrialComponentSummary' {endTime} -> endTime) (\s@TrialComponentSummary' {} a -> s {endTime = a} :: TrialComponentSummary) Core.. Lens.mapping Core._Time

-- | When the component was last modified.
trialComponentSummary_lastModifiedTime :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.UTCTime)
trialComponentSummary_lastModifiedTime = Lens.lens (\TrialComponentSummary' {lastModifiedTime} -> lastModifiedTime) (\s@TrialComponentSummary' {} a -> s {lastModifiedTime = a} :: TrialComponentSummary) Core.. Lens.mapping Core._Time

-- | Who created the component.
trialComponentSummary_createdBy :: Lens.Lens' TrialComponentSummary (Core.Maybe UserContext)
trialComponentSummary_createdBy = Lens.lens (\TrialComponentSummary' {createdBy} -> createdBy) (\s@TrialComponentSummary' {} a -> s {createdBy = a} :: TrialComponentSummary)

-- | Who last modified the component.
trialComponentSummary_lastModifiedBy :: Lens.Lens' TrialComponentSummary (Core.Maybe UserContext)
trialComponentSummary_lastModifiedBy = Lens.lens (\TrialComponentSummary' {lastModifiedBy} -> lastModifiedBy) (\s@TrialComponentSummary' {} a -> s {lastModifiedBy = a} :: TrialComponentSummary)

-- | Undocumented member.
trialComponentSummary_trialComponentSource :: Lens.Lens' TrialComponentSummary (Core.Maybe TrialComponentSource)
trialComponentSummary_trialComponentSource = Lens.lens (\TrialComponentSummary' {trialComponentSource} -> trialComponentSource) (\s@TrialComponentSummary' {} a -> s {trialComponentSource = a} :: TrialComponentSummary)

-- | The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
trialComponentSummary_displayName :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.Text)
trialComponentSummary_displayName = Lens.lens (\TrialComponentSummary' {displayName} -> displayName) (\s@TrialComponentSummary' {} a -> s {displayName = a} :: TrialComponentSummary)

-- | The name of the trial component.
trialComponentSummary_trialComponentName :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.Text)
trialComponentSummary_trialComponentName = Lens.lens (\TrialComponentSummary' {trialComponentName} -> trialComponentName) (\s@TrialComponentSummary' {} a -> s {trialComponentName = a} :: TrialComponentSummary)

instance Core.FromJSON TrialComponentSummary where
  parseJSON =
    Core.withObject
      "TrialComponentSummary"
      ( \x ->
          TrialComponentSummary'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "TrialComponentArn")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "TrialComponentSource")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "TrialComponentName")
      )

instance Core.Hashable TrialComponentSummary

instance Core.NFData TrialComponentSummary
