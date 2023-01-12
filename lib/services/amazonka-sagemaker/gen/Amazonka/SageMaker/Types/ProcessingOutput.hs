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
-- Module      : Amazonka.SageMaker.Types.ProcessingOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProcessingOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProcessingFeatureStoreOutput
import Amazonka.SageMaker.Types.ProcessingS3Output

-- | Describes the results of a processing job. The processing output must
-- specify exactly one of either @S3Output@ or @FeatureStoreOutput@ types.
--
-- /See:/ 'newProcessingOutput' smart constructor.
data ProcessingOutput = ProcessingOutput'
  { -- | When @True@, output operations such as data upload are managed natively
    -- by the processing job application. When @False@ (default), output
    -- operations are managed by Amazon SageMaker.
    appManaged :: Prelude.Maybe Prelude.Bool,
    -- | Configuration for processing job outputs in Amazon SageMaker Feature
    -- Store. This processing output type is only supported when @AppManaged@
    -- is specified.
    featureStoreOutput :: Prelude.Maybe ProcessingFeatureStoreOutput,
    -- | Configuration for processing job outputs in Amazon S3.
    s3Output :: Prelude.Maybe ProcessingS3Output,
    -- | The name for the processing job output.
    outputName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessingOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appManaged', 'processingOutput_appManaged' - When @True@, output operations such as data upload are managed natively
-- by the processing job application. When @False@ (default), output
-- operations are managed by Amazon SageMaker.
--
-- 'featureStoreOutput', 'processingOutput_featureStoreOutput' - Configuration for processing job outputs in Amazon SageMaker Feature
-- Store. This processing output type is only supported when @AppManaged@
-- is specified.
--
-- 's3Output', 'processingOutput_s3Output' - Configuration for processing job outputs in Amazon S3.
--
-- 'outputName', 'processingOutput_outputName' - The name for the processing job output.
newProcessingOutput ::
  -- | 'outputName'
  Prelude.Text ->
  ProcessingOutput
newProcessingOutput pOutputName_ =
  ProcessingOutput'
    { appManaged = Prelude.Nothing,
      featureStoreOutput = Prelude.Nothing,
      s3Output = Prelude.Nothing,
      outputName = pOutputName_
    }

-- | When @True@, output operations such as data upload are managed natively
-- by the processing job application. When @False@ (default), output
-- operations are managed by Amazon SageMaker.
processingOutput_appManaged :: Lens.Lens' ProcessingOutput (Prelude.Maybe Prelude.Bool)
processingOutput_appManaged = Lens.lens (\ProcessingOutput' {appManaged} -> appManaged) (\s@ProcessingOutput' {} a -> s {appManaged = a} :: ProcessingOutput)

-- | Configuration for processing job outputs in Amazon SageMaker Feature
-- Store. This processing output type is only supported when @AppManaged@
-- is specified.
processingOutput_featureStoreOutput :: Lens.Lens' ProcessingOutput (Prelude.Maybe ProcessingFeatureStoreOutput)
processingOutput_featureStoreOutput = Lens.lens (\ProcessingOutput' {featureStoreOutput} -> featureStoreOutput) (\s@ProcessingOutput' {} a -> s {featureStoreOutput = a} :: ProcessingOutput)

-- | Configuration for processing job outputs in Amazon S3.
processingOutput_s3Output :: Lens.Lens' ProcessingOutput (Prelude.Maybe ProcessingS3Output)
processingOutput_s3Output = Lens.lens (\ProcessingOutput' {s3Output} -> s3Output) (\s@ProcessingOutput' {} a -> s {s3Output = a} :: ProcessingOutput)

-- | The name for the processing job output.
processingOutput_outputName :: Lens.Lens' ProcessingOutput Prelude.Text
processingOutput_outputName = Lens.lens (\ProcessingOutput' {outputName} -> outputName) (\s@ProcessingOutput' {} a -> s {outputName = a} :: ProcessingOutput)

instance Data.FromJSON ProcessingOutput where
  parseJSON =
    Data.withObject
      "ProcessingOutput"
      ( \x ->
          ProcessingOutput'
            Prelude.<$> (x Data..:? "AppManaged")
            Prelude.<*> (x Data..:? "FeatureStoreOutput")
            Prelude.<*> (x Data..:? "S3Output")
            Prelude.<*> (x Data..: "OutputName")
      )

instance Prelude.Hashable ProcessingOutput where
  hashWithSalt _salt ProcessingOutput' {..} =
    _salt `Prelude.hashWithSalt` appManaged
      `Prelude.hashWithSalt` featureStoreOutput
      `Prelude.hashWithSalt` s3Output
      `Prelude.hashWithSalt` outputName

instance Prelude.NFData ProcessingOutput where
  rnf ProcessingOutput' {..} =
    Prelude.rnf appManaged
      `Prelude.seq` Prelude.rnf featureStoreOutput
      `Prelude.seq` Prelude.rnf s3Output
      `Prelude.seq` Prelude.rnf outputName

instance Data.ToJSON ProcessingOutput where
  toJSON ProcessingOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppManaged" Data..=) Prelude.<$> appManaged,
            ("FeatureStoreOutput" Data..=)
              Prelude.<$> featureStoreOutput,
            ("S3Output" Data..=) Prelude.<$> s3Output,
            Prelude.Just ("OutputName" Data..= outputName)
          ]
      )
