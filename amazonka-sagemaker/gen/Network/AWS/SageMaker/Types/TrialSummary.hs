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
-- Module      : Network.AWS.SageMaker.Types.TrialSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.TrialSource

-- | A summary of the properties of a trial. To get the complete set of
-- properties, call the DescribeTrial API and provide the @TrialName@.
--
-- /See:/ 'newTrialSummary' smart constructor.
data TrialSummary = TrialSummary'
  { trialSource :: Prelude.Maybe TrialSource,
    -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | When the trial was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | When the trial was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
    -- @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial.
    trialName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrialSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialSource', 'trialSummary_trialSource' - Undocumented member.
--
-- 'trialArn', 'trialSummary_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'creationTime', 'trialSummary_creationTime' - When the trial was created.
--
-- 'lastModifiedTime', 'trialSummary_lastModifiedTime' - When the trial was last modified.
--
-- 'displayName', 'trialSummary_displayName' - The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
--
-- 'trialName', 'trialSummary_trialName' - The name of the trial.
newTrialSummary ::
  TrialSummary
newTrialSummary =
  TrialSummary'
    { trialSource = Prelude.Nothing,
      trialArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      displayName = Prelude.Nothing,
      trialName = Prelude.Nothing
    }

-- | Undocumented member.
trialSummary_trialSource :: Lens.Lens' TrialSummary (Prelude.Maybe TrialSource)
trialSummary_trialSource = Lens.lens (\TrialSummary' {trialSource} -> trialSource) (\s@TrialSummary' {} a -> s {trialSource = a} :: TrialSummary)

-- | The Amazon Resource Name (ARN) of the trial.
trialSummary_trialArn :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.Text)
trialSummary_trialArn = Lens.lens (\TrialSummary' {trialArn} -> trialArn) (\s@TrialSummary' {} a -> s {trialArn = a} :: TrialSummary)

-- | When the trial was created.
trialSummary_creationTime :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.UTCTime)
trialSummary_creationTime = Lens.lens (\TrialSummary' {creationTime} -> creationTime) (\s@TrialSummary' {} a -> s {creationTime = a} :: TrialSummary) Prelude.. Lens.mapping Prelude._Time

-- | When the trial was last modified.
trialSummary_lastModifiedTime :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.UTCTime)
trialSummary_lastModifiedTime = Lens.lens (\TrialSummary' {lastModifiedTime} -> lastModifiedTime) (\s@TrialSummary' {} a -> s {lastModifiedTime = a} :: TrialSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
trialSummary_displayName :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.Text)
trialSummary_displayName = Lens.lens (\TrialSummary' {displayName} -> displayName) (\s@TrialSummary' {} a -> s {displayName = a} :: TrialSummary)

-- | The name of the trial.
trialSummary_trialName :: Lens.Lens' TrialSummary (Prelude.Maybe Prelude.Text)
trialSummary_trialName = Lens.lens (\TrialSummary' {trialName} -> trialName) (\s@TrialSummary' {} a -> s {trialName = a} :: TrialSummary)

instance Prelude.FromJSON TrialSummary where
  parseJSON =
    Prelude.withObject
      "TrialSummary"
      ( \x ->
          TrialSummary'
            Prelude.<$> (x Prelude..:? "TrialSource")
            Prelude.<*> (x Prelude..:? "TrialArn")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "DisplayName")
            Prelude.<*> (x Prelude..:? "TrialName")
      )

instance Prelude.Hashable TrialSummary

instance Prelude.NFData TrialSummary
