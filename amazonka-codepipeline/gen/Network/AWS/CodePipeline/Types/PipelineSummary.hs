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
-- Module      : Network.AWS.CodePipeline.Types.PipelineSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns a summary of a pipeline.
--
-- /See:/ 'newPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | The version number of the pipeline.
    version :: Core.Maybe Core.Natural,
    -- | The name of the pipeline.
    name :: Core.Maybe Core.Text,
    -- | The date and time the pipeline was created, in timestamp format.
    created :: Core.Maybe Core.POSIX,
    -- | The date and time of the last update to the pipeline, in timestamp
    -- format.
    updated :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'pipelineSummary_version' - The version number of the pipeline.
--
-- 'name', 'pipelineSummary_name' - The name of the pipeline.
--
-- 'created', 'pipelineSummary_created' - The date and time the pipeline was created, in timestamp format.
--
-- 'updated', 'pipelineSummary_updated' - The date and time of the last update to the pipeline, in timestamp
-- format.
newPipelineSummary ::
  PipelineSummary
newPipelineSummary =
  PipelineSummary'
    { version = Core.Nothing,
      name = Core.Nothing,
      created = Core.Nothing,
      updated = Core.Nothing
    }

-- | The version number of the pipeline.
pipelineSummary_version :: Lens.Lens' PipelineSummary (Core.Maybe Core.Natural)
pipelineSummary_version = Lens.lens (\PipelineSummary' {version} -> version) (\s@PipelineSummary' {} a -> s {version = a} :: PipelineSummary)

-- | The name of the pipeline.
pipelineSummary_name :: Lens.Lens' PipelineSummary (Core.Maybe Core.Text)
pipelineSummary_name = Lens.lens (\PipelineSummary' {name} -> name) (\s@PipelineSummary' {} a -> s {name = a} :: PipelineSummary)

-- | The date and time the pipeline was created, in timestamp format.
pipelineSummary_created :: Lens.Lens' PipelineSummary (Core.Maybe Core.UTCTime)
pipelineSummary_created = Lens.lens (\PipelineSummary' {created} -> created) (\s@PipelineSummary' {} a -> s {created = a} :: PipelineSummary) Core.. Lens.mapping Core._Time

-- | The date and time of the last update to the pipeline, in timestamp
-- format.
pipelineSummary_updated :: Lens.Lens' PipelineSummary (Core.Maybe Core.UTCTime)
pipelineSummary_updated = Lens.lens (\PipelineSummary' {updated} -> updated) (\s@PipelineSummary' {} a -> s {updated = a} :: PipelineSummary) Core.. Lens.mapping Core._Time

instance Core.FromJSON PipelineSummary where
  parseJSON =
    Core.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Core.<$> (x Core..:? "version")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "created")
            Core.<*> (x Core..:? "updated")
      )

instance Core.Hashable PipelineSummary

instance Core.NFData PipelineSummary
