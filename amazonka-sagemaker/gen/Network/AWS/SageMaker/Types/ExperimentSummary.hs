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
-- Module      : Network.AWS.SageMaker.Types.ExperimentSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExperimentSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ExperimentSource

-- | A summary of the properties of an experiment. To get the complete set of
-- properties, call the DescribeExperiment API and provide the
-- @ExperimentName@.
--
-- /See:/ 'newExperimentSummary' smart constructor.
data ExperimentSummary = ExperimentSummary'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Core.Maybe Core.Text,
    -- | When the experiment was created.
    creationTime :: Core.Maybe Core.POSIX,
    experimentSource :: Core.Maybe ExperimentSource,
    -- | When the experiment was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The name of the experiment.
    experimentName :: Core.Maybe Core.Text,
    -- | The name of the experiment as displayed. If @DisplayName@ isn\'t
    -- specified, @ExperimentName@ is displayed.
    displayName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExperimentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentArn', 'experimentSummary_experimentArn' - The Amazon Resource Name (ARN) of the experiment.
--
-- 'creationTime', 'experimentSummary_creationTime' - When the experiment was created.
--
-- 'experimentSource', 'experimentSummary_experimentSource' - Undocumented member.
--
-- 'lastModifiedTime', 'experimentSummary_lastModifiedTime' - When the experiment was last modified.
--
-- 'experimentName', 'experimentSummary_experimentName' - The name of the experiment.
--
-- 'displayName', 'experimentSummary_displayName' - The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
newExperimentSummary ::
  ExperimentSummary
newExperimentSummary =
  ExperimentSummary'
    { experimentArn = Core.Nothing,
      creationTime = Core.Nothing,
      experimentSource = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      experimentName = Core.Nothing,
      displayName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the experiment.
experimentSummary_experimentArn :: Lens.Lens' ExperimentSummary (Core.Maybe Core.Text)
experimentSummary_experimentArn = Lens.lens (\ExperimentSummary' {experimentArn} -> experimentArn) (\s@ExperimentSummary' {} a -> s {experimentArn = a} :: ExperimentSummary)

-- | When the experiment was created.
experimentSummary_creationTime :: Lens.Lens' ExperimentSummary (Core.Maybe Core.UTCTime)
experimentSummary_creationTime = Lens.lens (\ExperimentSummary' {creationTime} -> creationTime) (\s@ExperimentSummary' {} a -> s {creationTime = a} :: ExperimentSummary) Core.. Lens.mapping Core._Time

-- | Undocumented member.
experimentSummary_experimentSource :: Lens.Lens' ExperimentSummary (Core.Maybe ExperimentSource)
experimentSummary_experimentSource = Lens.lens (\ExperimentSummary' {experimentSource} -> experimentSource) (\s@ExperimentSummary' {} a -> s {experimentSource = a} :: ExperimentSummary)

-- | When the experiment was last modified.
experimentSummary_lastModifiedTime :: Lens.Lens' ExperimentSummary (Core.Maybe Core.UTCTime)
experimentSummary_lastModifiedTime = Lens.lens (\ExperimentSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ExperimentSummary' {} a -> s {lastModifiedTime = a} :: ExperimentSummary) Core.. Lens.mapping Core._Time

-- | The name of the experiment.
experimentSummary_experimentName :: Lens.Lens' ExperimentSummary (Core.Maybe Core.Text)
experimentSummary_experimentName = Lens.lens (\ExperimentSummary' {experimentName} -> experimentName) (\s@ExperimentSummary' {} a -> s {experimentName = a} :: ExperimentSummary)

-- | The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
experimentSummary_displayName :: Lens.Lens' ExperimentSummary (Core.Maybe Core.Text)
experimentSummary_displayName = Lens.lens (\ExperimentSummary' {displayName} -> displayName) (\s@ExperimentSummary' {} a -> s {displayName = a} :: ExperimentSummary)

instance Core.FromJSON ExperimentSummary where
  parseJSON =
    Core.withObject
      "ExperimentSummary"
      ( \x ->
          ExperimentSummary'
            Core.<$> (x Core..:? "ExperimentArn")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ExperimentSource")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "ExperimentName")
            Core.<*> (x Core..:? "DisplayName")
      )

instance Core.Hashable ExperimentSummary

instance Core.NFData ExperimentSummary
