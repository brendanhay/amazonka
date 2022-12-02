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
-- Module      : Amazonka.CodePipeline.Types.PipelineSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.PipelineSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns a summary of a pipeline.
--
-- /See:/ 'newPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | The name of the pipeline.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the last update to the pipeline, in timestamp
    -- format.
    updated :: Prelude.Maybe Data.POSIX,
    -- | The date and time the pipeline was created, in timestamp format.
    created :: Prelude.Maybe Data.POSIX,
    -- | The version number of the pipeline.
    version :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'pipelineSummary_name' - The name of the pipeline.
--
-- 'updated', 'pipelineSummary_updated' - The date and time of the last update to the pipeline, in timestamp
-- format.
--
-- 'created', 'pipelineSummary_created' - The date and time the pipeline was created, in timestamp format.
--
-- 'version', 'pipelineSummary_version' - The version number of the pipeline.
newPipelineSummary ::
  PipelineSummary
newPipelineSummary =
  PipelineSummary'
    { name = Prelude.Nothing,
      updated = Prelude.Nothing,
      created = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the pipeline.
pipelineSummary_name :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_name = Lens.lens (\PipelineSummary' {name} -> name) (\s@PipelineSummary' {} a -> s {name = a} :: PipelineSummary)

-- | The date and time of the last update to the pipeline, in timestamp
-- format.
pipelineSummary_updated :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_updated = Lens.lens (\PipelineSummary' {updated} -> updated) (\s@PipelineSummary' {} a -> s {updated = a} :: PipelineSummary) Prelude.. Lens.mapping Data._Time

-- | The date and time the pipeline was created, in timestamp format.
pipelineSummary_created :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_created = Lens.lens (\PipelineSummary' {created} -> created) (\s@PipelineSummary' {} a -> s {created = a} :: PipelineSummary) Prelude.. Lens.mapping Data._Time

-- | The version number of the pipeline.
pipelineSummary_version :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Natural)
pipelineSummary_version = Lens.lens (\PipelineSummary' {version} -> version) (\s@PipelineSummary' {} a -> s {version = a} :: PipelineSummary)

instance Data.FromJSON PipelineSummary where
  parseJSON =
    Data.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "updated")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable PipelineSummary where
  hashWithSalt _salt PipelineSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` updated
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` version

instance Prelude.NFData PipelineSummary where
  rnf PipelineSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf updated
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf version
