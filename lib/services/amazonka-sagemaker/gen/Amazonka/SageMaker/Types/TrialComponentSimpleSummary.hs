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
-- Module      : Amazonka.SageMaker.Types.TrialComponentSimpleSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentSimpleSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrialComponentSource
import Amazonka.SageMaker.Types.UserContext

-- | A short summary of a trial component.
--
-- /See:/ 'newTrialComponentSimpleSummary' smart constructor.
data TrialComponentSimpleSummary = TrialComponentSimpleSummary'
  { createdBy :: Prelude.Maybe UserContext,
    -- | When the component was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial component.
    trialComponentName :: Prelude.Maybe Prelude.Text,
    trialComponentSource :: Prelude.Maybe TrialComponentSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialComponentSimpleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdBy', 'trialComponentSimpleSummary_createdBy' - Undocumented member.
--
-- 'creationTime', 'trialComponentSimpleSummary_creationTime' - When the component was created.
--
-- 'trialComponentArn', 'trialComponentSimpleSummary_trialComponentArn' - The Amazon Resource Name (ARN) of the trial component.
--
-- 'trialComponentName', 'trialComponentSimpleSummary_trialComponentName' - The name of the trial component.
--
-- 'trialComponentSource', 'trialComponentSimpleSummary_trialComponentSource' - Undocumented member.
newTrialComponentSimpleSummary ::
  TrialComponentSimpleSummary
newTrialComponentSimpleSummary =
  TrialComponentSimpleSummary'
    { createdBy =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      trialComponentArn = Prelude.Nothing,
      trialComponentName = Prelude.Nothing,
      trialComponentSource = Prelude.Nothing
    }

-- | Undocumented member.
trialComponentSimpleSummary_createdBy :: Lens.Lens' TrialComponentSimpleSummary (Prelude.Maybe UserContext)
trialComponentSimpleSummary_createdBy = Lens.lens (\TrialComponentSimpleSummary' {createdBy} -> createdBy) (\s@TrialComponentSimpleSummary' {} a -> s {createdBy = a} :: TrialComponentSimpleSummary)

-- | When the component was created.
trialComponentSimpleSummary_creationTime :: Lens.Lens' TrialComponentSimpleSummary (Prelude.Maybe Prelude.UTCTime)
trialComponentSimpleSummary_creationTime = Lens.lens (\TrialComponentSimpleSummary' {creationTime} -> creationTime) (\s@TrialComponentSimpleSummary' {} a -> s {creationTime = a} :: TrialComponentSimpleSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the trial component.
trialComponentSimpleSummary_trialComponentArn :: Lens.Lens' TrialComponentSimpleSummary (Prelude.Maybe Prelude.Text)
trialComponentSimpleSummary_trialComponentArn = Lens.lens (\TrialComponentSimpleSummary' {trialComponentArn} -> trialComponentArn) (\s@TrialComponentSimpleSummary' {} a -> s {trialComponentArn = a} :: TrialComponentSimpleSummary)

-- | The name of the trial component.
trialComponentSimpleSummary_trialComponentName :: Lens.Lens' TrialComponentSimpleSummary (Prelude.Maybe Prelude.Text)
trialComponentSimpleSummary_trialComponentName = Lens.lens (\TrialComponentSimpleSummary' {trialComponentName} -> trialComponentName) (\s@TrialComponentSimpleSummary' {} a -> s {trialComponentName = a} :: TrialComponentSimpleSummary)

-- | Undocumented member.
trialComponentSimpleSummary_trialComponentSource :: Lens.Lens' TrialComponentSimpleSummary (Prelude.Maybe TrialComponentSource)
trialComponentSimpleSummary_trialComponentSource = Lens.lens (\TrialComponentSimpleSummary' {trialComponentSource} -> trialComponentSource) (\s@TrialComponentSimpleSummary' {} a -> s {trialComponentSource = a} :: TrialComponentSimpleSummary)

instance Data.FromJSON TrialComponentSimpleSummary where
  parseJSON =
    Data.withObject
      "TrialComponentSimpleSummary"
      ( \x ->
          TrialComponentSimpleSummary'
            Prelude.<$> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "TrialComponentArn")
            Prelude.<*> (x Data..:? "TrialComponentName")
            Prelude.<*> (x Data..:? "TrialComponentSource")
      )

instance Prelude.Hashable TrialComponentSimpleSummary where
  hashWithSalt _salt TrialComponentSimpleSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` trialComponentArn
      `Prelude.hashWithSalt` trialComponentName
      `Prelude.hashWithSalt` trialComponentSource

instance Prelude.NFData TrialComponentSimpleSummary where
  rnf TrialComponentSimpleSummary' {..} =
    Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf trialComponentArn
      `Prelude.seq` Prelude.rnf trialComponentName
      `Prelude.seq` Prelude.rnf trialComponentSource
