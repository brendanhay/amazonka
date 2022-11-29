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
-- Module      : Amazonka.SageMaker.Types.TrialSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrialSource

-- | A summary of the properties of a trial. To get the complete set of
-- properties, call the DescribeTrial API and provide the @TrialName@.
--
-- /See:/ 'newTrialSummary' smart constructor.
data TrialSummary = TrialSummary'
  { -- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
    -- @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | When the trial was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the trial.
    trialName :: Prelude.Maybe Prelude.Text,
    -- | When the trial was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    trialSource :: Prelude.Maybe TrialSource,
    -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'trialSummary_displayName' - The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
--
-- 'lastModifiedTime', 'trialSummary_lastModifiedTime' - When the trial was last modified.
--
-- 'trialName', 'trialSummary_trialName' - The name of the trial.
--
-- 'creationTime', 'trialSummary_creationTime' - When the trial was created.
--
-- 'trialSource', 'trialSummary_trialSource' - Undocumented member.
--
-- 'trialArn', 'trialSummary_trialArn' - The Amazon Resource Name (ARN) of the trial.
newTrialSummary ::
  TrialSummary
newTrialSummary =
  TrialSummary'
    { displayName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      trialName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      trialSource = Prelude.Nothing,
      trialArn = Prelude.Nothing
    }

-- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
trialSummary_displayName :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.Text)
trialSummary_displayName = Lens.lens (\TrialSummary' {displayName} -> displayName) (\s@TrialSummary' {} a -> s {displayName = a} :: TrialSummary)

-- | When the trial was last modified.
trialSummary_lastModifiedTime :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.UTCTime)
trialSummary_lastModifiedTime = Lens.lens (\TrialSummary' {lastModifiedTime} -> lastModifiedTime) (\s@TrialSummary' {} a -> s {lastModifiedTime = a} :: TrialSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the trial.
trialSummary_trialName :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.Text)
trialSummary_trialName = Lens.lens (\TrialSummary' {trialName} -> trialName) (\s@TrialSummary' {} a -> s {trialName = a} :: TrialSummary)

-- | When the trial was created.
trialSummary_creationTime :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.UTCTime)
trialSummary_creationTime = Lens.lens (\TrialSummary' {creationTime} -> creationTime) (\s@TrialSummary' {} a -> s {creationTime = a} :: TrialSummary) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
trialSummary_trialSource :: Lens.Lens' TrialSummary (Prelude.Maybe TrialSource)
trialSummary_trialSource = Lens.lens (\TrialSummary' {trialSource} -> trialSource) (\s@TrialSummary' {} a -> s {trialSource = a} :: TrialSummary)

-- | The Amazon Resource Name (ARN) of the trial.
trialSummary_trialArn :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.Text)
trialSummary_trialArn = Lens.lens (\TrialSummary' {trialArn} -> trialArn) (\s@TrialSummary' {} a -> s {trialArn = a} :: TrialSummary)

instance Core.FromJSON TrialSummary where
  parseJSON =
    Core.withObject
      "TrialSummary"
      ( \x ->
          TrialSummary'
            Prelude.<$> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "TrialName")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "TrialSource")
            Prelude.<*> (x Core..:? "TrialArn")
      )

instance Prelude.Hashable TrialSummary where
  hashWithSalt _salt TrialSummary' {..} =
    _salt `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` trialName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` trialSource
      `Prelude.hashWithSalt` trialArn

instance Prelude.NFData TrialSummary where
  rnf TrialSummary' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf trialName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf trialSource
      `Prelude.seq` Prelude.rnf trialArn
