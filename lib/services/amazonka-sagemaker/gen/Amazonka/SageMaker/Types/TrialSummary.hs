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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrialSource

-- | A summary of the properties of a trial. To get the complete set of
-- properties, call the DescribeTrial API and provide the @TrialName@.
--
-- /See:/ 'newTrialSummary' smart constructor.
data TrialSummary = TrialSummary'
  { -- | When the trial was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
    -- @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | When the trial was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial.
    trialName :: Prelude.Maybe Prelude.Text,
    trialSource :: Prelude.Maybe TrialSource
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
-- 'creationTime', 'trialSummary_creationTime' - When the trial was created.
--
-- 'displayName', 'trialSummary_displayName' - The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
--
-- 'lastModifiedTime', 'trialSummary_lastModifiedTime' - When the trial was last modified.
--
-- 'trialArn', 'trialSummary_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'trialName', 'trialSummary_trialName' - The name of the trial.
--
-- 'trialSource', 'trialSummary_trialSource' - Undocumented member.
newTrialSummary ::
  TrialSummary
newTrialSummary =
  TrialSummary'
    { creationTime = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      trialArn = Prelude.Nothing,
      trialName = Prelude.Nothing,
      trialSource = Prelude.Nothing
    }

-- | When the trial was created.
trialSummary_creationTime :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.UTCTime)
trialSummary_creationTime = Lens.lens (\TrialSummary' {creationTime} -> creationTime) (\s@TrialSummary' {} a -> s {creationTime = a} :: TrialSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
trialSummary_displayName :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.Text)
trialSummary_displayName = Lens.lens (\TrialSummary' {displayName} -> displayName) (\s@TrialSummary' {} a -> s {displayName = a} :: TrialSummary)

-- | When the trial was last modified.
trialSummary_lastModifiedTime :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.UTCTime)
trialSummary_lastModifiedTime = Lens.lens (\TrialSummary' {lastModifiedTime} -> lastModifiedTime) (\s@TrialSummary' {} a -> s {lastModifiedTime = a} :: TrialSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the trial.
trialSummary_trialArn :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.Text)
trialSummary_trialArn = Lens.lens (\TrialSummary' {trialArn} -> trialArn) (\s@TrialSummary' {} a -> s {trialArn = a} :: TrialSummary)

-- | The name of the trial.
trialSummary_trialName :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.Text)
trialSummary_trialName = Lens.lens (\TrialSummary' {trialName} -> trialName) (\s@TrialSummary' {} a -> s {trialName = a} :: TrialSummary)

-- | Undocumented member.
trialSummary_trialSource :: Lens.Lens' TrialSummary (Prelude.Maybe TrialSource)
trialSummary_trialSource = Lens.lens (\TrialSummary' {trialSource} -> trialSource) (\s@TrialSummary' {} a -> s {trialSource = a} :: TrialSummary)

instance Data.FromJSON TrialSummary where
  parseJSON =
    Data.withObject
      "TrialSummary"
      ( \x ->
          TrialSummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "TrialArn")
            Prelude.<*> (x Data..:? "TrialName")
            Prelude.<*> (x Data..:? "TrialSource")
      )

instance Prelude.Hashable TrialSummary where
  hashWithSalt _salt TrialSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` trialArn
      `Prelude.hashWithSalt` trialName
      `Prelude.hashWithSalt` trialSource

instance Prelude.NFData TrialSummary where
  rnf TrialSummary' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf displayName `Prelude.seq`
        Prelude.rnf lastModifiedTime `Prelude.seq`
          Prelude.rnf trialArn `Prelude.seq`
            Prelude.rnf trialName `Prelude.seq`
              Prelude.rnf trialSource
