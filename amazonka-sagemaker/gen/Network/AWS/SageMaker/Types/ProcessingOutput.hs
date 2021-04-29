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
-- Module      : Network.AWS.SageMaker.Types.ProcessingOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingOutput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ProcessingFeatureStoreOutput
import Network.AWS.SageMaker.Types.ProcessingS3Output

-- | Describes the results of a processing job. The processing output must
-- specify exactly one of either @S3Output@ or @FeatureStoreOutput@ types.
--
-- /See:/ 'newProcessingOutput' smart constructor.
data ProcessingOutput = ProcessingOutput'
  { -- | Configuration for processing job outputs in Amazon S3.
    s3Output :: Prelude.Maybe ProcessingS3Output,
    -- | Configuration for processing job outputs in Amazon SageMaker Feature
    -- Store. This processing output type is only supported when @AppManaged@
    -- is specified.
    featureStoreOutput :: Prelude.Maybe ProcessingFeatureStoreOutput,
    -- | When @True@, output operations such as data upload are managed natively
    -- by the processing job application. When @False@ (default), output
    -- operations are managed by Amazon SageMaker.
    appManaged :: Prelude.Maybe Prelude.Bool,
    -- | The name for the processing job output.
    outputName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProcessingOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Output', 'processingOutput_s3Output' - Configuration for processing job outputs in Amazon S3.
--
-- 'featureStoreOutput', 'processingOutput_featureStoreOutput' - Configuration for processing job outputs in Amazon SageMaker Feature
-- Store. This processing output type is only supported when @AppManaged@
-- is specified.
--
-- 'appManaged', 'processingOutput_appManaged' - When @True@, output operations such as data upload are managed natively
-- by the processing job application. When @False@ (default), output
-- operations are managed by Amazon SageMaker.
--
-- 'outputName', 'processingOutput_outputName' - The name for the processing job output.
newProcessingOutput ::
  -- | 'outputName'
  Prelude.Text ->
  ProcessingOutput
newProcessingOutput pOutputName_ =
  ProcessingOutput'
    { s3Output = Prelude.Nothing,
      featureStoreOutput = Prelude.Nothing,
      appManaged = Prelude.Nothing,
      outputName = pOutputName_
    }

-- | Configuration for processing job outputs in Amazon S3.
processingOutput_s3Output :: Lens.Lens' ProcessingOutput (Prelude.Maybe ProcessingS3Output)
processingOutput_s3Output = Lens.lens (\ProcessingOutput' {s3Output} -> s3Output) (\s@ProcessingOutput' {} a -> s {s3Output = a} :: ProcessingOutput)

-- | Configuration for processing job outputs in Amazon SageMaker Feature
-- Store. This processing output type is only supported when @AppManaged@
-- is specified.
processingOutput_featureStoreOutput :: Lens.Lens' ProcessingOutput (Prelude.Maybe ProcessingFeatureStoreOutput)
processingOutput_featureStoreOutput = Lens.lens (\ProcessingOutput' {featureStoreOutput} -> featureStoreOutput) (\s@ProcessingOutput' {} a -> s {featureStoreOutput = a} :: ProcessingOutput)

-- | When @True@, output operations such as data upload are managed natively
-- by the processing job application. When @False@ (default), output
-- operations are managed by Amazon SageMaker.
processingOutput_appManaged :: Lens.Lens' ProcessingOutput (Prelude.Maybe Prelude.Bool)
processingOutput_appManaged = Lens.lens (\ProcessingOutput' {appManaged} -> appManaged) (\s@ProcessingOutput' {} a -> s {appManaged = a} :: ProcessingOutput)

-- | The name for the processing job output.
processingOutput_outputName :: Lens.Lens' ProcessingOutput Prelude.Text
processingOutput_outputName = Lens.lens (\ProcessingOutput' {outputName} -> outputName) (\s@ProcessingOutput' {} a -> s {outputName = a} :: ProcessingOutput)

instance Prelude.FromJSON ProcessingOutput where
  parseJSON =
    Prelude.withObject
      "ProcessingOutput"
      ( \x ->
          ProcessingOutput'
            Prelude.<$> (x Prelude..:? "S3Output")
            Prelude.<*> (x Prelude..:? "FeatureStoreOutput")
            Prelude.<*> (x Prelude..:? "AppManaged")
            Prelude.<*> (x Prelude..: "OutputName")
      )

instance Prelude.Hashable ProcessingOutput

instance Prelude.NFData ProcessingOutput

instance Prelude.ToJSON ProcessingOutput where
  toJSON ProcessingOutput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("S3Output" Prelude..=) Prelude.<$> s3Output,
            ("FeatureStoreOutput" Prelude..=)
              Prelude.<$> featureStoreOutput,
            ("AppManaged" Prelude..=) Prelude.<$> appManaged,
            Prelude.Just ("OutputName" Prelude..= outputName)
          ]
      )
