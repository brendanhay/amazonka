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
-- Module      : Amazonka.Comprehend.Types.InputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.InputDataConfig where

import Amazonka.Comprehend.Types.DocumentReaderConfig
import Amazonka.Comprehend.Types.InputFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The input properties for an inference job. The document reader config
-- field applies only to non-text inputs for custom analysis.
--
-- /See:/ 'newInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { -- | Provides configuration parameters to override the default actions for
    -- extracting text from PDF documents and image files.
    documentReaderConfig :: Prelude.Maybe DocumentReaderConfig,
    -- | Specifies how the text in an input file should be processed:
    --
    -- -   @ONE_DOC_PER_FILE@ - Each file is considered a separate document.
    --     Use this option when you are processing large documents, such as
    --     newspaper articles or scientific papers.
    --
    -- -   @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate
    --     document. Use this option when you are processing many short
    --     documents, such as text messages.
    inputFormat :: Prelude.Maybe InputFormat,
    -- | The Amazon S3 URI for the input data. The URI must be in same region as
    -- the API endpoint that you are calling. The URI can point to a single
    -- input file or it can provide the prefix for a collection of data files.
    --
    -- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
    -- prefix is a single file, Amazon Comprehend uses that file as input. If
    -- more than one file begins with the prefix, Amazon Comprehend uses all of
    -- them as input.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentReaderConfig', 'inputDataConfig_documentReaderConfig' - Provides configuration parameters to override the default actions for
-- extracting text from PDF documents and image files.
--
-- 'inputFormat', 'inputDataConfig_inputFormat' - Specifies how the text in an input file should be processed:
--
-- -   @ONE_DOC_PER_FILE@ - Each file is considered a separate document.
--     Use this option when you are processing large documents, such as
--     newspaper articles or scientific papers.
--
-- -   @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate
--     document. Use this option when you are processing many short
--     documents, such as text messages.
--
-- 's3Uri', 'inputDataConfig_s3Uri' - The Amazon S3 URI for the input data. The URI must be in same region as
-- the API endpoint that you are calling. The URI can point to a single
-- input file or it can provide the prefix for a collection of data files.
--
-- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
-- prefix is a single file, Amazon Comprehend uses that file as input. If
-- more than one file begins with the prefix, Amazon Comprehend uses all of
-- them as input.
newInputDataConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  InputDataConfig
newInputDataConfig pS3Uri_ =
  InputDataConfig'
    { documentReaderConfig =
        Prelude.Nothing,
      inputFormat = Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | Provides configuration parameters to override the default actions for
-- extracting text from PDF documents and image files.
inputDataConfig_documentReaderConfig :: Lens.Lens' InputDataConfig (Prelude.Maybe DocumentReaderConfig)
inputDataConfig_documentReaderConfig = Lens.lens (\InputDataConfig' {documentReaderConfig} -> documentReaderConfig) (\s@InputDataConfig' {} a -> s {documentReaderConfig = a} :: InputDataConfig)

-- | Specifies how the text in an input file should be processed:
--
-- -   @ONE_DOC_PER_FILE@ - Each file is considered a separate document.
--     Use this option when you are processing large documents, such as
--     newspaper articles or scientific papers.
--
-- -   @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate
--     document. Use this option when you are processing many short
--     documents, such as text messages.
inputDataConfig_inputFormat :: Lens.Lens' InputDataConfig (Prelude.Maybe InputFormat)
inputDataConfig_inputFormat = Lens.lens (\InputDataConfig' {inputFormat} -> inputFormat) (\s@InputDataConfig' {} a -> s {inputFormat = a} :: InputDataConfig)

-- | The Amazon S3 URI for the input data. The URI must be in same region as
-- the API endpoint that you are calling. The URI can point to a single
-- input file or it can provide the prefix for a collection of data files.
--
-- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
-- prefix is a single file, Amazon Comprehend uses that file as input. If
-- more than one file begins with the prefix, Amazon Comprehend uses all of
-- them as input.
inputDataConfig_s3Uri :: Lens.Lens' InputDataConfig Prelude.Text
inputDataConfig_s3Uri = Lens.lens (\InputDataConfig' {s3Uri} -> s3Uri) (\s@InputDataConfig' {} a -> s {s3Uri = a} :: InputDataConfig)

instance Data.FromJSON InputDataConfig where
  parseJSON =
    Data.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Prelude.<$> (x Data..:? "DocumentReaderConfig")
            Prelude.<*> (x Data..:? "InputFormat")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable InputDataConfig where
  hashWithSalt _salt InputDataConfig' {..} =
    _salt
      `Prelude.hashWithSalt` documentReaderConfig
      `Prelude.hashWithSalt` inputFormat
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData InputDataConfig where
  rnf InputDataConfig' {..} =
    Prelude.rnf documentReaderConfig `Prelude.seq`
      Prelude.rnf inputFormat `Prelude.seq`
        Prelude.rnf s3Uri

instance Data.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentReaderConfig" Data..=)
              Prelude.<$> documentReaderConfig,
            ("InputFormat" Data..=) Prelude.<$> inputFormat,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
