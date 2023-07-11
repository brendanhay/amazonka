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
-- Module      : Amazonka.Glue.Types.LabelingSetGenerationTaskRunProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.LabelingSetGenerationTaskRunProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration properties for a labeling set generation task
-- run.
--
-- /See:/ 'newLabelingSetGenerationTaskRunProperties' smart constructor.
data LabelingSetGenerationTaskRunProperties = LabelingSetGenerationTaskRunProperties'
  { -- | The Amazon Simple Storage Service (Amazon S3) path where you will
    -- generate the labeling set.
    outputS3Path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelingSetGenerationTaskRunProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputS3Path', 'labelingSetGenerationTaskRunProperties_outputS3Path' - The Amazon Simple Storage Service (Amazon S3) path where you will
-- generate the labeling set.
newLabelingSetGenerationTaskRunProperties ::
  LabelingSetGenerationTaskRunProperties
newLabelingSetGenerationTaskRunProperties =
  LabelingSetGenerationTaskRunProperties'
    { outputS3Path =
        Prelude.Nothing
    }

-- | The Amazon Simple Storage Service (Amazon S3) path where you will
-- generate the labeling set.
labelingSetGenerationTaskRunProperties_outputS3Path :: Lens.Lens' LabelingSetGenerationTaskRunProperties (Prelude.Maybe Prelude.Text)
labelingSetGenerationTaskRunProperties_outputS3Path = Lens.lens (\LabelingSetGenerationTaskRunProperties' {outputS3Path} -> outputS3Path) (\s@LabelingSetGenerationTaskRunProperties' {} a -> s {outputS3Path = a} :: LabelingSetGenerationTaskRunProperties)

instance
  Data.FromJSON
    LabelingSetGenerationTaskRunProperties
  where
  parseJSON =
    Data.withObject
      "LabelingSetGenerationTaskRunProperties"
      ( \x ->
          LabelingSetGenerationTaskRunProperties'
            Prelude.<$> (x Data..:? "OutputS3Path")
      )

instance
  Prelude.Hashable
    LabelingSetGenerationTaskRunProperties
  where
  hashWithSalt
    _salt
    LabelingSetGenerationTaskRunProperties' {..} =
      _salt `Prelude.hashWithSalt` outputS3Path

instance
  Prelude.NFData
    LabelingSetGenerationTaskRunProperties
  where
  rnf LabelingSetGenerationTaskRunProperties' {..} =
    Prelude.rnf outputS3Path
