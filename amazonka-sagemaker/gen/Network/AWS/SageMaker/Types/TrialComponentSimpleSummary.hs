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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.UserContext

-- | A short summary of a trial component.
--
-- /See:/ 'newTrialComponentSimpleSummary' smart constructor.
data TrialComponentSimpleSummary = TrialComponentSimpleSummary'
  { -- | When the component was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Core.Maybe Core.Text,
    createdBy :: Core.Maybe UserContext,
    trialComponentSource :: Core.Maybe TrialComponentSource,
    -- | The name of the trial component.
    trialComponentName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      trialComponentArn = Core.Nothing,
      createdBy = Core.Nothing,
      trialComponentSource = Core.Nothing,
      trialComponentName = Core.Nothing
    }

-- | When the component was created.
trialComponentSimpleSummary_creationTime :: Lens.Lens' TrialComponentSimpleSummary (Core.Maybe Core.UTCTime)
trialComponentSimpleSummary_creationTime = Lens.lens (\TrialComponentSimpleSummary' {creationTime} -> creationTime) (\s@TrialComponentSimpleSummary' {} a -> s {creationTime = a} :: TrialComponentSimpleSummary) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the trial component.
trialComponentSimpleSummary_trialComponentArn :: Lens.Lens' TrialComponentSimpleSummary (Core.Maybe Core.Text)
trialComponentSimpleSummary_trialComponentArn = Lens.lens (\TrialComponentSimpleSummary' {trialComponentArn} -> trialComponentArn) (\s@TrialComponentSimpleSummary' {} a -> s {trialComponentArn = a} :: TrialComponentSimpleSummary)

-- | Undocumented member.
trialComponentSimpleSummary_createdBy :: Lens.Lens' TrialComponentSimpleSummary (Core.Maybe UserContext)
trialComponentSimpleSummary_createdBy = Lens.lens (\TrialComponentSimpleSummary' {createdBy} -> createdBy) (\s@TrialComponentSimpleSummary' {} a -> s {createdBy = a} :: TrialComponentSimpleSummary)

-- | Undocumented member.
trialComponentSimpleSummary_trialComponentSource :: Lens.Lens' TrialComponentSimpleSummary (Core.Maybe TrialComponentSource)
trialComponentSimpleSummary_trialComponentSource = Lens.lens (\TrialComponentSimpleSummary' {trialComponentSource} -> trialComponentSource) (\s@TrialComponentSimpleSummary' {} a -> s {trialComponentSource = a} :: TrialComponentSimpleSummary)

-- | The name of the trial component.
trialComponentSimpleSummary_trialComponentName :: Lens.Lens' TrialComponentSimpleSummary (Core.Maybe Core.Text)
trialComponentSimpleSummary_trialComponentName = Lens.lens (\TrialComponentSimpleSummary' {trialComponentName} -> trialComponentName) (\s@TrialComponentSimpleSummary' {} a -> s {trialComponentName = a} :: TrialComponentSimpleSummary)

instance Core.FromJSON TrialComponentSimpleSummary where
  parseJSON =
    Core.withObject
      "TrialComponentSimpleSummary"
      ( \x ->
          TrialComponentSimpleSummary'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "TrialComponentArn")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "TrialComponentSource")
            Core.<*> (x Core..:? "TrialComponentName")
      )

instance Core.Hashable TrialComponentSimpleSummary

instance Core.NFData TrialComponentSimpleSummary
