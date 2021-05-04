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
-- Module      : Network.AWS.SageMaker.Types.ProcessingJobStepMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJobStepMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Metadata for a processing job step.
--
-- /See:/ 'newProcessingJobStepMetadata' smart constructor.
data ProcessingJobStepMetadata = ProcessingJobStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the processing job.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  ProcessingJobStepMetadata' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the processing job.
processingJobStepMetadata_arn :: Lens.Lens' ProcessingJobStepMetadata (Prelude.Maybe Prelude.Text)
processingJobStepMetadata_arn = Lens.lens (\ProcessingJobStepMetadata' {arn} -> arn) (\s@ProcessingJobStepMetadata' {} a -> s {arn = a} :: ProcessingJobStepMetadata)

instance Prelude.FromJSON ProcessingJobStepMetadata where
  parseJSON =
    Prelude.withObject
      "ProcessingJobStepMetadata"
      ( \x ->
          ProcessingJobStepMetadata'
            Prelude.<$> (x Prelude..:? "Arn")
      )

instance Prelude.Hashable ProcessingJobStepMetadata

instance Prelude.NFData ProcessingJobStepMetadata
