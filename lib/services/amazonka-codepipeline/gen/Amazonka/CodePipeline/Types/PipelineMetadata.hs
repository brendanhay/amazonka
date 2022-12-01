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
-- Module      : Amazonka.CodePipeline.Types.PipelineMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.PipelineMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a pipeline.
--
-- /See:/ 'newPipelineMetadata' smart constructor.
data PipelineMetadata = PipelineMetadata'
  { -- | The date and time the pipeline was last updated, in timestamp format.
    updated :: Prelude.Maybe Core.POSIX,
    -- | The date and time the pipeline was created, in timestamp format.
    created :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updated', 'pipelineMetadata_updated' - The date and time the pipeline was last updated, in timestamp format.
--
-- 'created', 'pipelineMetadata_created' - The date and time the pipeline was created, in timestamp format.
--
-- 'pipelineArn', 'pipelineMetadata_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
newPipelineMetadata ::
  PipelineMetadata
newPipelineMetadata =
  PipelineMetadata'
    { updated = Prelude.Nothing,
      created = Prelude.Nothing,
      pipelineArn = Prelude.Nothing
    }

-- | The date and time the pipeline was last updated, in timestamp format.
pipelineMetadata_updated :: Lens.Lens' PipelineMetadata (Prelude.Maybe Prelude.UTCTime)
pipelineMetadata_updated = Lens.lens (\PipelineMetadata' {updated} -> updated) (\s@PipelineMetadata' {} a -> s {updated = a} :: PipelineMetadata) Prelude.. Lens.mapping Core._Time

-- | The date and time the pipeline was created, in timestamp format.
pipelineMetadata_created :: Lens.Lens' PipelineMetadata (Prelude.Maybe Prelude.UTCTime)
pipelineMetadata_created = Lens.lens (\PipelineMetadata' {created} -> created) (\s@PipelineMetadata' {} a -> s {created = a} :: PipelineMetadata) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the pipeline.
pipelineMetadata_pipelineArn :: Lens.Lens' PipelineMetadata (Prelude.Maybe Prelude.Text)
pipelineMetadata_pipelineArn = Lens.lens (\PipelineMetadata' {pipelineArn} -> pipelineArn) (\s@PipelineMetadata' {} a -> s {pipelineArn = a} :: PipelineMetadata)

instance Core.FromJSON PipelineMetadata where
  parseJSON =
    Core.withObject
      "PipelineMetadata"
      ( \x ->
          PipelineMetadata'
            Prelude.<$> (x Core..:? "updated")
            Prelude.<*> (x Core..:? "created")
            Prelude.<*> (x Core..:? "pipelineArn")
      )

instance Prelude.Hashable PipelineMetadata where
  hashWithSalt _salt PipelineMetadata' {..} =
    _salt `Prelude.hashWithSalt` updated
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` pipelineArn

instance Prelude.NFData PipelineMetadata where
  rnf PipelineMetadata' {..} =
    Prelude.rnf updated
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf pipelineArn
