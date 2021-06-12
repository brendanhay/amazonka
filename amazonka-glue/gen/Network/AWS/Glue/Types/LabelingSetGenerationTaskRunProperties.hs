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
-- Module      : Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies configuration properties for a labeling set generation task
-- run.
--
-- /See:/ 'newLabelingSetGenerationTaskRunProperties' smart constructor.
data LabelingSetGenerationTaskRunProperties = LabelingSetGenerationTaskRunProperties'
  { -- | The Amazon Simple Storage Service (Amazon S3) path where you will
    -- generate the labeling set.
    outputS3Path :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The Amazon Simple Storage Service (Amazon S3) path where you will
-- generate the labeling set.
labelingSetGenerationTaskRunProperties_outputS3Path :: Lens.Lens' LabelingSetGenerationTaskRunProperties (Core.Maybe Core.Text)
labelingSetGenerationTaskRunProperties_outputS3Path = Lens.lens (\LabelingSetGenerationTaskRunProperties' {outputS3Path} -> outputS3Path) (\s@LabelingSetGenerationTaskRunProperties' {} a -> s {outputS3Path = a} :: LabelingSetGenerationTaskRunProperties)

instance
  Core.FromJSON
    LabelingSetGenerationTaskRunProperties
  where
  parseJSON =
    Core.withObject
      "LabelingSetGenerationTaskRunProperties"
      ( \x ->
          LabelingSetGenerationTaskRunProperties'
            Core.<$> (x Core..:? "OutputS3Path")
      )

instance
  Core.Hashable
    LabelingSetGenerationTaskRunProperties

instance
  Core.NFData
    LabelingSetGenerationTaskRunProperties
