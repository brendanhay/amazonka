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
-- Module      : Network.AWS.SageMaker.Types.ProcessingInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingInput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.DatasetDefinition
import Network.AWS.SageMaker.Types.ProcessingS3Input

-- | The inputs for a processing job. The processing input must specify
-- exactly one of either @S3Input@ or @DatasetDefinition@ types.
--
-- /See:/ 'newProcessingInput' smart constructor.
data ProcessingInput = ProcessingInput'
  { -- | Configuration for a Dataset Definition input.
    datasetDefinition :: Core.Maybe DatasetDefinition,
    -- | When @True@, input operations such as data download are managed natively
    -- by the processing job application. When @False@ (default), input
    -- operations are managed by Amazon SageMaker.
    appManaged :: Core.Maybe Core.Bool,
    -- | Configuration for downloading input data from Amazon S3 into the
    -- processing container.
    s3Input :: Core.Maybe ProcessingS3Input,
    -- | The name for the processing job input.
    inputName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProcessingInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetDefinition', 'processingInput_datasetDefinition' - Configuration for a Dataset Definition input.
--
-- 'appManaged', 'processingInput_appManaged' - When @True@, input operations such as data download are managed natively
-- by the processing job application. When @False@ (default), input
-- operations are managed by Amazon SageMaker.
--
-- 's3Input', 'processingInput_s3Input' - Configuration for downloading input data from Amazon S3 into the
-- processing container.
--
-- 'inputName', 'processingInput_inputName' - The name for the processing job input.
newProcessingInput ::
  -- | 'inputName'
  Core.Text ->
  ProcessingInput
newProcessingInput pInputName_ =
  ProcessingInput'
    { datasetDefinition = Core.Nothing,
      appManaged = Core.Nothing,
      s3Input = Core.Nothing,
      inputName = pInputName_
    }

-- | Configuration for a Dataset Definition input.
processingInput_datasetDefinition :: Lens.Lens' ProcessingInput (Core.Maybe DatasetDefinition)
processingInput_datasetDefinition = Lens.lens (\ProcessingInput' {datasetDefinition} -> datasetDefinition) (\s@ProcessingInput' {} a -> s {datasetDefinition = a} :: ProcessingInput)

-- | When @True@, input operations such as data download are managed natively
-- by the processing job application. When @False@ (default), input
-- operations are managed by Amazon SageMaker.
processingInput_appManaged :: Lens.Lens' ProcessingInput (Core.Maybe Core.Bool)
processingInput_appManaged = Lens.lens (\ProcessingInput' {appManaged} -> appManaged) (\s@ProcessingInput' {} a -> s {appManaged = a} :: ProcessingInput)

-- | Configuration for downloading input data from Amazon S3 into the
-- processing container.
processingInput_s3Input :: Lens.Lens' ProcessingInput (Core.Maybe ProcessingS3Input)
processingInput_s3Input = Lens.lens (\ProcessingInput' {s3Input} -> s3Input) (\s@ProcessingInput' {} a -> s {s3Input = a} :: ProcessingInput)

-- | The name for the processing job input.
processingInput_inputName :: Lens.Lens' ProcessingInput Core.Text
processingInput_inputName = Lens.lens (\ProcessingInput' {inputName} -> inputName) (\s@ProcessingInput' {} a -> s {inputName = a} :: ProcessingInput)

instance Core.FromJSON ProcessingInput where
  parseJSON =
    Core.withObject
      "ProcessingInput"
      ( \x ->
          ProcessingInput'
            Core.<$> (x Core..:? "DatasetDefinition")
            Core.<*> (x Core..:? "AppManaged")
            Core.<*> (x Core..:? "S3Input")
            Core.<*> (x Core..: "InputName")
      )

instance Core.Hashable ProcessingInput

instance Core.NFData ProcessingInput

instance Core.ToJSON ProcessingInput where
  toJSON ProcessingInput' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DatasetDefinition" Core..=)
              Core.<$> datasetDefinition,
            ("AppManaged" Core..=) Core.<$> appManaged,
            ("S3Input" Core..=) Core.<$> s3Input,
            Core.Just ("InputName" Core..= inputName)
          ]
      )
