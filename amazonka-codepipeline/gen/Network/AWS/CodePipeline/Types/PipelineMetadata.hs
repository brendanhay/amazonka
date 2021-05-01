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
-- Module      : Network.AWS.CodePipeline.Types.PipelineMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a pipeline.
--
-- /See:/ 'newPipelineMetadata' smart constructor.
data PipelineMetadata = PipelineMetadata'
  { -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time the pipeline was created, in timestamp format.
    created :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time the pipeline was last updated, in timestamp format.
    updated :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PipelineMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineArn', 'pipelineMetadata_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'created', 'pipelineMetadata_created' - The date and time the pipeline was created, in timestamp format.
--
-- 'updated', 'pipelineMetadata_updated' - The date and time the pipeline was last updated, in timestamp format.
newPipelineMetadata ::
  PipelineMetadata
newPipelineMetadata =
  PipelineMetadata'
    { pipelineArn = Prelude.Nothing,
      created = Prelude.Nothing,
      updated = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline.
pipelineMetadata_pipelineArn :: Lens.Lens' PipelineMetadata (Prelude.Maybe Prelude.Text)
pipelineMetadata_pipelineArn = Lens.lens (\PipelineMetadata' {pipelineArn} -> pipelineArn) (\s@PipelineMetadata' {} a -> s {pipelineArn = a} :: PipelineMetadata)

-- | The date and time the pipeline was created, in timestamp format.
pipelineMetadata_created :: Lens.Lens' PipelineMetadata (Prelude.Maybe Prelude.UTCTime)
pipelineMetadata_created = Lens.lens (\PipelineMetadata' {created} -> created) (\s@PipelineMetadata' {} a -> s {created = a} :: PipelineMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The date and time the pipeline was last updated, in timestamp format.
pipelineMetadata_updated :: Lens.Lens' PipelineMetadata (Prelude.Maybe Prelude.UTCTime)
pipelineMetadata_updated = Lens.lens (\PipelineMetadata' {updated} -> updated) (\s@PipelineMetadata' {} a -> s {updated = a} :: PipelineMetadata) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON PipelineMetadata where
  parseJSON =
    Prelude.withObject
      "PipelineMetadata"
      ( \x ->
          PipelineMetadata'
            Prelude.<$> (x Prelude..:? "pipelineArn")
            Prelude.<*> (x Prelude..:? "created")
            Prelude.<*> (x Prelude..:? "updated")
      )

instance Prelude.Hashable PipelineMetadata

instance Prelude.NFData PipelineMetadata
