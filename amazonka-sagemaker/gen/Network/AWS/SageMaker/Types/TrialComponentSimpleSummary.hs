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
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSimpleSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.UserContext

-- | A short summary of a trial component.
--
-- /See:/ 'newTrialComponentSimpleSummary' smart constructor.
data TrialComponentSimpleSummary = TrialComponentSimpleSummary'
  { -- | When the component was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe UserContext,
    trialComponentSource :: Prelude.Maybe TrialComponentSource,
    -- | The name of the trial component.
    trialComponentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrialComponentSimpleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'trialComponentSimpleSummary_creationTime' - When the component was created.
--
-- 'trialComponentArn', 'trialComponentSimpleSummary_trialComponentArn' - The Amazon Resource Name (ARN) of the trial component.
--
-- 'createdBy', 'trialComponentSimpleSummary_createdBy' - Undocumented member.
--
-- 'trialComponentSource', 'trialComponentSimpleSummary_trialComponentSource' - Undocumented member.
--
-- 'trialComponentName', 'trialComponentSimpleSummary_trialComponentName' - The name of the trial component.
newTrialComponentSimpleSummary ::
  TrialComponentSimpleSummary
newTrialComponentSimpleSummary =
  TrialComponentSimpleSummary'
    { creationTime =
        Prelude.Nothing,
      trialComponentArn = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      trialComponentSource = Prelude.Nothing,
      trialComponentName = Prelude.Nothing
    }

-- | When the component was created.
trialComponentSimpleSummary_creationTime :: Lens.Lens' TrialComponentSimpleSummary (Prelude.Maybe Prelude.UTCTime)
trialComponentSimpleSummary_creationTime = Lens.lens (\TrialComponentSimpleSummary' {creationTime} -> creationTime) (\s@TrialComponentSimpleSummary' {} a -> s {creationTime = a} :: TrialComponentSimpleSummary) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the trial component.
trialComponentSimpleSummary_trialComponentArn :: Lens.Lens' TrialComponentSimpleSummary (Prelude.Maybe Prelude.Text)
trialComponentSimpleSummary_trialComponentArn = Lens.lens (\TrialComponentSimpleSummary' {trialComponentArn} -> trialComponentArn) (\s@TrialComponentSimpleSummary' {} a -> s {trialComponentArn = a} :: TrialComponentSimpleSummary)

-- | Undocumented member.
trialComponentSimpleSummary_createdBy :: Lens.Lens' TrialComponentSimpleSummary (Prelude.Maybe UserContext)
trialComponentSimpleSummary_createdBy = Lens.lens (\TrialComponentSimpleSummary' {createdBy} -> createdBy) (\s@TrialComponentSimpleSummary' {} a -> s {createdBy = a} :: TrialComponentSimpleSummary)

-- | Undocumented member.
trialComponentSimpleSummary_trialComponentSource :: Lens.Lens' TrialComponentSimpleSummary (Prelude.Maybe TrialComponentSource)
trialComponentSimpleSummary_trialComponentSource = Lens.lens (\TrialComponentSimpleSummary' {trialComponentSource} -> trialComponentSource) (\s@TrialComponentSimpleSummary' {} a -> s {trialComponentSource = a} :: TrialComponentSimpleSummary)

-- | The name of the trial component.
trialComponentSimpleSummary_trialComponentName :: Lens.Lens' TrialComponentSimpleSummary (Prelude.Maybe Prelude.Text)
trialComponentSimpleSummary_trialComponentName = Lens.lens (\TrialComponentSimpleSummary' {trialComponentName} -> trialComponentName) (\s@TrialComponentSimpleSummary' {} a -> s {trialComponentName = a} :: TrialComponentSimpleSummary)

instance Prelude.FromJSON TrialComponentSimpleSummary where
  parseJSON =
    Prelude.withObject
      "TrialComponentSimpleSummary"
      ( \x ->
          TrialComponentSimpleSummary'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "TrialComponentArn")
            Prelude.<*> (x Prelude..:? "CreatedBy")
            Prelude.<*> (x Prelude..:? "TrialComponentSource")
            Prelude.<*> (x Prelude..:? "TrialComponentName")
      )

instance Prelude.Hashable TrialComponentSimpleSummary

instance Prelude.NFData TrialComponentSimpleSummary
