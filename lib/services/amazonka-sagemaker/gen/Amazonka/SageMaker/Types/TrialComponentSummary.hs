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
-- Module      : Amazonka.SageMaker.Types.TrialComponentSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrialComponentSource
import Amazonka.SageMaker.Types.TrialComponentStatus
import Amazonka.SageMaker.Types.UserContext

-- | A summary of the properties of a trial component. To get all the
-- properties, call the DescribeTrialComponent API and provide the
-- @TrialComponentName@.
--
-- /See:/ 'newTrialComponentSummary' smart constructor.
data TrialComponentSummary = TrialComponentSummary'
  { -- | When the component was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the component. States include:
    --
    -- -   InProgress
    --
    -- -   Completed
    --
    -- -   Failed
    status :: Prelude.Maybe TrialComponentStatus,
    -- | When the component started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | Who created the trial component.
    createdBy :: Prelude.Maybe UserContext,
    -- | When the component was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | When the component ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the trial component.
    trialComponentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the component as displayed. If @DisplayName@ isn\'t
    -- specified, @TrialComponentName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Who last modified the component.
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The ARN of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    trialComponentSource :: Prelude.Maybe TrialComponentSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialComponentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'trialComponentSummary_creationTime' - When the component was created.
--
-- 'status', 'trialComponentSummary_status' - The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
--
-- 'startTime', 'trialComponentSummary_startTime' - When the component started.
--
-- 'createdBy', 'trialComponentSummary_createdBy' - Who created the trial component.
--
-- 'lastModifiedTime', 'trialComponentSummary_lastModifiedTime' - When the component was last modified.
--
-- 'endTime', 'trialComponentSummary_endTime' - When the component ended.
--
-- 'trialComponentName', 'trialComponentSummary_trialComponentName' - The name of the trial component.
--
-- 'displayName', 'trialComponentSummary_displayName' - The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
--
-- 'lastModifiedBy', 'trialComponentSummary_lastModifiedBy' - Who last modified the component.
--
-- 'trialComponentArn', 'trialComponentSummary_trialComponentArn' - The ARN of the trial component.
--
-- 'trialComponentSource', 'trialComponentSummary_trialComponentSource' - Undocumented member.
newTrialComponentSummary ::
  TrialComponentSummary
newTrialComponentSummary =
  TrialComponentSummary'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      startTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      trialComponentName = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      trialComponentArn = Prelude.Nothing,
      trialComponentSource = Prelude.Nothing
    }

-- | When the component was created.
trialComponentSummary_creationTime :: Lens.Lens' TrialComponentSummary (Prelude.Maybe Prelude.UTCTime)
trialComponentSummary_creationTime = Lens.lens (\TrialComponentSummary' {creationTime} -> creationTime) (\s@TrialComponentSummary' {} a -> s {creationTime = a} :: TrialComponentSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
trialComponentSummary_status :: Lens.Lens' TrialComponentSummary (Prelude.Maybe TrialComponentStatus)
trialComponentSummary_status = Lens.lens (\TrialComponentSummary' {status} -> status) (\s@TrialComponentSummary' {} a -> s {status = a} :: TrialComponentSummary)

-- | When the component started.
trialComponentSummary_startTime :: Lens.Lens' TrialComponentSummary (Prelude.Maybe Prelude.UTCTime)
trialComponentSummary_startTime = Lens.lens (\TrialComponentSummary' {startTime} -> startTime) (\s@TrialComponentSummary' {} a -> s {startTime = a} :: TrialComponentSummary) Prelude.. Lens.mapping Core._Time

-- | Who created the trial component.
trialComponentSummary_createdBy :: Lens.Lens' TrialComponentSummary (Prelude.Maybe UserContext)
trialComponentSummary_createdBy = Lens.lens (\TrialComponentSummary' {createdBy} -> createdBy) (\s@TrialComponentSummary' {} a -> s {createdBy = a} :: TrialComponentSummary)

-- | When the component was last modified.
trialComponentSummary_lastModifiedTime :: Lens.Lens' TrialComponentSummary (Prelude.Maybe Prelude.UTCTime)
trialComponentSummary_lastModifiedTime = Lens.lens (\TrialComponentSummary' {lastModifiedTime} -> lastModifiedTime) (\s@TrialComponentSummary' {} a -> s {lastModifiedTime = a} :: TrialComponentSummary) Prelude.. Lens.mapping Core._Time

-- | When the component ended.
trialComponentSummary_endTime :: Lens.Lens' TrialComponentSummary (Prelude.Maybe Prelude.UTCTime)
trialComponentSummary_endTime = Lens.lens (\TrialComponentSummary' {endTime} -> endTime) (\s@TrialComponentSummary' {} a -> s {endTime = a} :: TrialComponentSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the trial component.
trialComponentSummary_trialComponentName :: Lens.Lens' TrialComponentSummary (Prelude.Maybe Prelude.Text)
trialComponentSummary_trialComponentName = Lens.lens (\TrialComponentSummary' {trialComponentName} -> trialComponentName) (\s@TrialComponentSummary' {} a -> s {trialComponentName = a} :: TrialComponentSummary)

-- | The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
trialComponentSummary_displayName :: Lens.Lens' TrialComponentSummary (Prelude.Maybe Prelude.Text)
trialComponentSummary_displayName = Lens.lens (\TrialComponentSummary' {displayName} -> displayName) (\s@TrialComponentSummary' {} a -> s {displayName = a} :: TrialComponentSummary)

-- | Who last modified the component.
trialComponentSummary_lastModifiedBy :: Lens.Lens' TrialComponentSummary (Prelude.Maybe UserContext)
trialComponentSummary_lastModifiedBy = Lens.lens (\TrialComponentSummary' {lastModifiedBy} -> lastModifiedBy) (\s@TrialComponentSummary' {} a -> s {lastModifiedBy = a} :: TrialComponentSummary)

-- | The ARN of the trial component.
trialComponentSummary_trialComponentArn :: Lens.Lens' TrialComponentSummary (Prelude.Maybe Prelude.Text)
trialComponentSummary_trialComponentArn = Lens.lens (\TrialComponentSummary' {trialComponentArn} -> trialComponentArn) (\s@TrialComponentSummary' {} a -> s {trialComponentArn = a} :: TrialComponentSummary)

-- | Undocumented member.
trialComponentSummary_trialComponentSource :: Lens.Lens' TrialComponentSummary (Prelude.Maybe TrialComponentSource)
trialComponentSummary_trialComponentSource = Lens.lens (\TrialComponentSummary' {trialComponentSource} -> trialComponentSource) (\s@TrialComponentSummary' {} a -> s {trialComponentSource = a} :: TrialComponentSummary)

instance Core.FromJSON TrialComponentSummary where
  parseJSON =
    Core.withObject
      "TrialComponentSummary"
      ( \x ->
          TrialComponentSummary'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "TrialComponentName")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "TrialComponentArn")
            Prelude.<*> (x Core..:? "TrialComponentSource")
      )

instance Prelude.Hashable TrialComponentSummary where
  hashWithSalt _salt TrialComponentSummary' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` trialComponentName
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` trialComponentArn
      `Prelude.hashWithSalt` trialComponentSource

instance Prelude.NFData TrialComponentSummary where
  rnf TrialComponentSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf trialComponentName
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf trialComponentArn
      `Prelude.seq` Prelude.rnf trialComponentSource
