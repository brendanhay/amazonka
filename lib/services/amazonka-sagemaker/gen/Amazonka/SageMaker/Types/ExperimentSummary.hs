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
-- Module      : Amazonka.SageMaker.Types.ExperimentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ExperimentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ExperimentSource

-- | A summary of the properties of an experiment. To get the complete set of
-- properties, call the DescribeExperiment API and provide the
-- @ExperimentName@.
--
-- /See:/ 'newExperimentSummary' smart constructor.
data ExperimentSummary = ExperimentSummary'
  { -- | The name of the experiment as displayed. If @DisplayName@ isn\'t
    -- specified, @ExperimentName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | When the experiment was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    experimentSource :: Prelude.Maybe ExperimentSource,
    -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Prelude.Maybe Prelude.Text,
    -- | When the experiment was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the experiment.
    experimentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'experimentSummary_displayName' - The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
--
-- 'lastModifiedTime', 'experimentSummary_lastModifiedTime' - When the experiment was last modified.
--
-- 'experimentSource', 'experimentSummary_experimentSource' - Undocumented member.
--
-- 'experimentArn', 'experimentSummary_experimentArn' - The Amazon Resource Name (ARN) of the experiment.
--
-- 'creationTime', 'experimentSummary_creationTime' - When the experiment was created.
--
-- 'experimentName', 'experimentSummary_experimentName' - The name of the experiment.
newExperimentSummary ::
  ExperimentSummary
newExperimentSummary =
  ExperimentSummary'
    { displayName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      experimentSource = Prelude.Nothing,
      experimentArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      experimentName = Prelude.Nothing
    }

-- | The name of the experiment as displayed. If @DisplayName@ isn\'t
-- specified, @ExperimentName@ is displayed.
experimentSummary_displayName :: Lens.Lens' ExperimentSummary (Prelude.Maybe Prelude.Text)
experimentSummary_displayName = Lens.lens (\ExperimentSummary' {displayName} -> displayName) (\s@ExperimentSummary' {} a -> s {displayName = a} :: ExperimentSummary)

-- | When the experiment was last modified.
experimentSummary_lastModifiedTime :: Lens.Lens' ExperimentSummary (Prelude.Maybe Prelude.UTCTime)
experimentSummary_lastModifiedTime = Lens.lens (\ExperimentSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ExperimentSummary' {} a -> s {lastModifiedTime = a} :: ExperimentSummary) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
experimentSummary_experimentSource :: Lens.Lens' ExperimentSummary (Prelude.Maybe ExperimentSource)
experimentSummary_experimentSource = Lens.lens (\ExperimentSummary' {experimentSource} -> experimentSource) (\s@ExperimentSummary' {} a -> s {experimentSource = a} :: ExperimentSummary)

-- | The Amazon Resource Name (ARN) of the experiment.
experimentSummary_experimentArn :: Lens.Lens' ExperimentSummary (Prelude.Maybe Prelude.Text)
experimentSummary_experimentArn = Lens.lens (\ExperimentSummary' {experimentArn} -> experimentArn) (\s@ExperimentSummary' {} a -> s {experimentArn = a} :: ExperimentSummary)

-- | When the experiment was created.
experimentSummary_creationTime :: Lens.Lens' ExperimentSummary (Prelude.Maybe Prelude.UTCTime)
experimentSummary_creationTime = Lens.lens (\ExperimentSummary' {creationTime} -> creationTime) (\s@ExperimentSummary' {} a -> s {creationTime = a} :: ExperimentSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the experiment.
experimentSummary_experimentName :: Lens.Lens' ExperimentSummary (Prelude.Maybe Prelude.Text)
experimentSummary_experimentName = Lens.lens (\ExperimentSummary' {experimentName} -> experimentName) (\s@ExperimentSummary' {} a -> s {experimentName = a} :: ExperimentSummary)

instance Core.FromJSON ExperimentSummary where
  parseJSON =
    Core.withObject
      "ExperimentSummary"
      ( \x ->
          ExperimentSummary'
            Prelude.<$> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "ExperimentSource")
            Prelude.<*> (x Core..:? "ExperimentArn")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "ExperimentName")
      )

instance Prelude.Hashable ExperimentSummary where
  hashWithSalt _salt ExperimentSummary' {..} =
    _salt `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` experimentSource
      `Prelude.hashWithSalt` experimentArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` experimentName

instance Prelude.NFData ExperimentSummary where
  rnf ExperimentSummary' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf experimentSource
      `Prelude.seq` Prelude.rnf experimentArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf experimentName
