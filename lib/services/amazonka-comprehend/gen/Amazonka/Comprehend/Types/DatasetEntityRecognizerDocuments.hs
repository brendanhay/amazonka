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
-- Module      : Amazonka.Comprehend.Types.DatasetEntityRecognizerDocuments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetEntityRecognizerDocuments where

import Amazonka.Comprehend.Types.InputFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the documents submitted with a dataset for an entity
-- recognizer model.
--
-- /See:/ 'newDatasetEntityRecognizerDocuments' smart constructor.
data DatasetEntityRecognizerDocuments = DatasetEntityRecognizerDocuments'
  { -- | Specifies how the text in an input file should be processed. This is
    -- optional, and the default is ONE_DOC_PER_LINE. ONE_DOC_PER_FILE - Each
    -- file is considered a separate document. Use this option when you are
    -- processing large documents, such as newspaper articles or scientific
    -- papers. ONE_DOC_PER_LINE - Each line in a file is considered a separate
    -- document. Use this option when you are processing many short documents,
    -- such as text messages.
    inputFormat :: Prelude.Maybe InputFormat,
    -- | Specifies the Amazon S3 location where the documents for the dataset are
    -- located.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetEntityRecognizerDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputFormat', 'datasetEntityRecognizerDocuments_inputFormat' - Specifies how the text in an input file should be processed. This is
-- optional, and the default is ONE_DOC_PER_LINE. ONE_DOC_PER_FILE - Each
-- file is considered a separate document. Use this option when you are
-- processing large documents, such as newspaper articles or scientific
-- papers. ONE_DOC_PER_LINE - Each line in a file is considered a separate
-- document. Use this option when you are processing many short documents,
-- such as text messages.
--
-- 's3Uri', 'datasetEntityRecognizerDocuments_s3Uri' - Specifies the Amazon S3 location where the documents for the dataset are
-- located.
newDatasetEntityRecognizerDocuments ::
  -- | 's3Uri'
  Prelude.Text ->
  DatasetEntityRecognizerDocuments
newDatasetEntityRecognizerDocuments pS3Uri_ =
  DatasetEntityRecognizerDocuments'
    { inputFormat =
        Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | Specifies how the text in an input file should be processed. This is
-- optional, and the default is ONE_DOC_PER_LINE. ONE_DOC_PER_FILE - Each
-- file is considered a separate document. Use this option when you are
-- processing large documents, such as newspaper articles or scientific
-- papers. ONE_DOC_PER_LINE - Each line in a file is considered a separate
-- document. Use this option when you are processing many short documents,
-- such as text messages.
datasetEntityRecognizerDocuments_inputFormat :: Lens.Lens' DatasetEntityRecognizerDocuments (Prelude.Maybe InputFormat)
datasetEntityRecognizerDocuments_inputFormat = Lens.lens (\DatasetEntityRecognizerDocuments' {inputFormat} -> inputFormat) (\s@DatasetEntityRecognizerDocuments' {} a -> s {inputFormat = a} :: DatasetEntityRecognizerDocuments)

-- | Specifies the Amazon S3 location where the documents for the dataset are
-- located.
datasetEntityRecognizerDocuments_s3Uri :: Lens.Lens' DatasetEntityRecognizerDocuments Prelude.Text
datasetEntityRecognizerDocuments_s3Uri = Lens.lens (\DatasetEntityRecognizerDocuments' {s3Uri} -> s3Uri) (\s@DatasetEntityRecognizerDocuments' {} a -> s {s3Uri = a} :: DatasetEntityRecognizerDocuments)

instance
  Prelude.Hashable
    DatasetEntityRecognizerDocuments
  where
  hashWithSalt
    _salt
    DatasetEntityRecognizerDocuments' {..} =
      _salt
        `Prelude.hashWithSalt` inputFormat
        `Prelude.hashWithSalt` s3Uri

instance
  Prelude.NFData
    DatasetEntityRecognizerDocuments
  where
  rnf DatasetEntityRecognizerDocuments' {..} =
    Prelude.rnf inputFormat
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON DatasetEntityRecognizerDocuments where
  toJSON DatasetEntityRecognizerDocuments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InputFormat" Data..=) Prelude.<$> inputFormat,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
