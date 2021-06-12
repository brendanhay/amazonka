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
-- Module      : Network.AWS.Comprehend.Types.InputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.InputDataConfig where

import Network.AWS.Comprehend.Types.InputFormat
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The input properties for a topic detection job.
--
-- /See:/ 'newInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { -- | Specifies how the text in an input file should be processed:
    --
    -- -   @ONE_DOC_PER_FILE@ - Each file is considered a separate document.
    --     Use this option when you are processing large documents, such as
    --     newspaper articles or scientific papers.
    --
    -- -   @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate
    --     document. Use this option when you are processing many short
    --     documents, such as text messages.
    inputFormat :: Core.Maybe InputFormat,
    -- | The Amazon S3 URI for the input data. The URI must be in same region as
    -- the API endpoint that you are calling. The URI can point to a single
    -- input file or it can provide the prefix for a collection of data files.
    --
    -- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
    -- prefix is a single file, Amazon Comprehend uses that file as input. If
    -- more than one file begins with the prefix, Amazon Comprehend uses all of
    -- them as input.
    s3Uri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
  Core.Text ->
  InputDataConfig
newInputDataConfig pS3Uri_ =
  InputDataConfig'
    { inputFormat = Core.Nothing,
      s3Uri = pS3Uri_
    }

-- | Specifies how the text in an input file should be processed:
--
-- -   @ONE_DOC_PER_FILE@ - Each file is considered a separate document.
--     Use this option when you are processing large documents, such as
--     newspaper articles or scientific papers.
--
-- -   @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate
--     document. Use this option when you are processing many short
--     documents, such as text messages.
inputDataConfig_inputFormat :: Lens.Lens' InputDataConfig (Core.Maybe InputFormat)
inputDataConfig_inputFormat = Lens.lens (\InputDataConfig' {inputFormat} -> inputFormat) (\s@InputDataConfig' {} a -> s {inputFormat = a} :: InputDataConfig)

-- | The Amazon S3 URI for the input data. The URI must be in same region as
-- the API endpoint that you are calling. The URI can point to a single
-- input file or it can provide the prefix for a collection of data files.
--
-- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
-- prefix is a single file, Amazon Comprehend uses that file as input. If
-- more than one file begins with the prefix, Amazon Comprehend uses all of
-- them as input.
inputDataConfig_s3Uri :: Lens.Lens' InputDataConfig Core.Text
inputDataConfig_s3Uri = Lens.lens (\InputDataConfig' {s3Uri} -> s3Uri) (\s@InputDataConfig' {} a -> s {s3Uri = a} :: InputDataConfig)

instance Core.FromJSON InputDataConfig where
  parseJSON =
    Core.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Core.<$> (x Core..:? "InputFormat")
            Core.<*> (x Core..: "S3Uri")
      )

instance Core.Hashable InputDataConfig

instance Core.NFData InputDataConfig

instance Core.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InputFormat" Core..=) Core.<$> inputFormat,
            Core.Just ("S3Uri" Core..= s3Uri)
          ]
      )
