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
-- Module      : Amazonka.SageMaker.Types.ProcessingInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProcessingInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DatasetDefinition
import Amazonka.SageMaker.Types.ProcessingS3Input

-- | The inputs for a processing job. The processing input must specify
-- exactly one of either @S3Input@ or @DatasetDefinition@ types.
--
-- /See:/ 'newProcessingInput' smart constructor.
data ProcessingInput = ProcessingInput'
  { -- | Configuration for downloading input data from Amazon S3 into the
    -- processing container.
    s3Input :: Prelude.Maybe ProcessingS3Input,
    -- | Configuration for a Dataset Definition input.
    datasetDefinition :: Prelude.Maybe DatasetDefinition,
    -- | When @True@, input operations such as data download are managed natively
    -- by the processing job application. When @False@ (default), input
    -- operations are managed by Amazon SageMaker.
    appManaged :: Prelude.Maybe Prelude.Bool,
    -- | The name for the processing job input.
    inputName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessingInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Input', 'processingInput_s3Input' - Configuration for downloading input data from Amazon S3 into the
-- processing container.
--
-- 'datasetDefinition', 'processingInput_datasetDefinition' - Configuration for a Dataset Definition input.
--
-- 'appManaged', 'processingInput_appManaged' - When @True@, input operations such as data download are managed natively
-- by the processing job application. When @False@ (default), input
-- operations are managed by Amazon SageMaker.
--
-- 'inputName', 'processingInput_inputName' - The name for the processing job input.
newProcessingInput ::
  -- | 'inputName'
  Prelude.Text ->
  ProcessingInput
newProcessingInput pInputName_ =
  ProcessingInput'
    { s3Input = Prelude.Nothing,
      datasetDefinition = Prelude.Nothing,
      appManaged = Prelude.Nothing,
      inputName = pInputName_
    }

-- | Configuration for downloading input data from Amazon S3 into the
-- processing container.
processingInput_s3Input :: Lens.Lens' ProcessingInput (Prelude.Maybe ProcessingS3Input)
processingInput_s3Input = Lens.lens (\ProcessingInput' {s3Input} -> s3Input) (\s@ProcessingInput' {} a -> s {s3Input = a} :: ProcessingInput)

-- | Configuration for a Dataset Definition input.
processingInput_datasetDefinition :: Lens.Lens' ProcessingInput (Prelude.Maybe DatasetDefinition)
processingInput_datasetDefinition = Lens.lens (\ProcessingInput' {datasetDefinition} -> datasetDefinition) (\s@ProcessingInput' {} a -> s {datasetDefinition = a} :: ProcessingInput)

-- | When @True@, input operations such as data download are managed natively
-- by the processing job application. When @False@ (default), input
-- operations are managed by Amazon SageMaker.
processingInput_appManaged :: Lens.Lens' ProcessingInput (Prelude.Maybe Prelude.Bool)
processingInput_appManaged = Lens.lens (\ProcessingInput' {appManaged} -> appManaged) (\s@ProcessingInput' {} a -> s {appManaged = a} :: ProcessingInput)

-- | The name for the processing job input.
processingInput_inputName :: Lens.Lens' ProcessingInput Prelude.Text
processingInput_inputName = Lens.lens (\ProcessingInput' {inputName} -> inputName) (\s@ProcessingInput' {} a -> s {inputName = a} :: ProcessingInput)

instance Data.FromJSON ProcessingInput where
  parseJSON =
    Data.withObject
      "ProcessingInput"
      ( \x ->
          ProcessingInput'
            Prelude.<$> (x Data..:? "S3Input")
            Prelude.<*> (x Data..:? "DatasetDefinition")
            Prelude.<*> (x Data..:? "AppManaged")
            Prelude.<*> (x Data..: "InputName")
      )

instance Prelude.Hashable ProcessingInput where
  hashWithSalt _salt ProcessingInput' {..} =
    _salt `Prelude.hashWithSalt` s3Input
      `Prelude.hashWithSalt` datasetDefinition
      `Prelude.hashWithSalt` appManaged
      `Prelude.hashWithSalt` inputName

instance Prelude.NFData ProcessingInput where
  rnf ProcessingInput' {..} =
    Prelude.rnf s3Input
      `Prelude.seq` Prelude.rnf datasetDefinition
      `Prelude.seq` Prelude.rnf appManaged
      `Prelude.seq` Prelude.rnf inputName

instance Data.ToJSON ProcessingInput where
  toJSON ProcessingInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3Input" Data..=) Prelude.<$> s3Input,
            ("DatasetDefinition" Data..=)
              Prelude.<$> datasetDefinition,
            ("AppManaged" Data..=) Prelude.<$> appManaged,
            Prelude.Just ("InputName" Data..= inputName)
          ]
      )
