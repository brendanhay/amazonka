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
-- Module      : Network.AWS.SageMaker.Types.ProcessingJobStepMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJobStepMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Metadata for a processing job step.
--
-- /See:/ 'newProcessingJobStepMetadata' smart constructor.
data ProcessingJobStepMetadata = ProcessingJobStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the processing job.
    arn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProcessingJobStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'processingJobStepMetadata_arn' - The Amazon Resource Name (ARN) of the processing job.
newProcessingJobStepMetadata ::
  ProcessingJobStepMetadata
newProcessingJobStepMetadata =
  ProcessingJobStepMetadata' {arn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the processing job.
processingJobStepMetadata_arn :: Lens.Lens' ProcessingJobStepMetadata (Core.Maybe Core.Text)
processingJobStepMetadata_arn = Lens.lens (\ProcessingJobStepMetadata' {arn} -> arn) (\s@ProcessingJobStepMetadata' {} a -> s {arn = a} :: ProcessingJobStepMetadata)

instance Core.FromJSON ProcessingJobStepMetadata where
  parseJSON =
    Core.withObject
      "ProcessingJobStepMetadata"
      ( \x ->
          ProcessingJobStepMetadata'
            Core.<$> (x Core..:? "Arn")
      )

instance Core.Hashable ProcessingJobStepMetadata

instance Core.NFData ProcessingJobStepMetadata
