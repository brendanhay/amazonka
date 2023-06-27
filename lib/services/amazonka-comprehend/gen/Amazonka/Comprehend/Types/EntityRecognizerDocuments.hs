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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerDocuments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerDocuments where

import Amazonka.Comprehend.Types.InputFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the training documents submitted with an entity recognizer.
--
-- /See:/ 'newEntityRecognizerDocuments' smart constructor.
data EntityRecognizerDocuments = EntityRecognizerDocuments'
  { -- | Specifies how the text in an input file should be processed. This is
    -- optional, and the default is ONE_DOC_PER_LINE. ONE_DOC_PER_FILE - Each
    -- file is considered a separate document. Use this option when you are
    -- processing large documents, such as newspaper articles or scientific
    -- papers. ONE_DOC_PER_LINE - Each line in a file is considered a separate
    -- document. Use this option when you are processing many short documents,
    -- such as text messages.
    inputFormat :: Prelude.Maybe InputFormat,
    -- | Specifies the Amazon S3 location where the test documents for an entity
    -- recognizer are located. The URI must be in the same Amazon Web Services
    -- Region as the API endpoint that you are calling.
    testS3Uri :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon S3 location where the training documents for an
    -- entity recognizer are located. The URI must be in the same Region as the
    -- API endpoint that you are calling.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputFormat', 'entityRecognizerDocuments_inputFormat' - Specifies how the text in an input file should be processed. This is
-- optional, and the default is ONE_DOC_PER_LINE. ONE_DOC_PER_FILE - Each
-- file is considered a separate document. Use this option when you are
-- processing large documents, such as newspaper articles or scientific
-- papers. ONE_DOC_PER_LINE - Each line in a file is considered a separate
-- document. Use this option when you are processing many short documents,
-- such as text messages.
--
-- 'testS3Uri', 'entityRecognizerDocuments_testS3Uri' - Specifies the Amazon S3 location where the test documents for an entity
-- recognizer are located. The URI must be in the same Amazon Web Services
-- Region as the API endpoint that you are calling.
--
-- 's3Uri', 'entityRecognizerDocuments_s3Uri' - Specifies the Amazon S3 location where the training documents for an
-- entity recognizer are located. The URI must be in the same Region as the
-- API endpoint that you are calling.
newEntityRecognizerDocuments ::
  -- | 's3Uri'
  Prelude.Text ->
  EntityRecognizerDocuments
newEntityRecognizerDocuments pS3Uri_ =
  EntityRecognizerDocuments'
    { inputFormat =
        Prelude.Nothing,
      testS3Uri = Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | Specifies how the text in an input file should be processed. This is
-- optional, and the default is ONE_DOC_PER_LINE. ONE_DOC_PER_FILE - Each
-- file is considered a separate document. Use this option when you are
-- processing large documents, such as newspaper articles or scientific
-- papers. ONE_DOC_PER_LINE - Each line in a file is considered a separate
-- document. Use this option when you are processing many short documents,
-- such as text messages.
entityRecognizerDocuments_inputFormat :: Lens.Lens' EntityRecognizerDocuments (Prelude.Maybe InputFormat)
entityRecognizerDocuments_inputFormat = Lens.lens (\EntityRecognizerDocuments' {inputFormat} -> inputFormat) (\s@EntityRecognizerDocuments' {} a -> s {inputFormat = a} :: EntityRecognizerDocuments)

-- | Specifies the Amazon S3 location where the test documents for an entity
-- recognizer are located. The URI must be in the same Amazon Web Services
-- Region as the API endpoint that you are calling.
entityRecognizerDocuments_testS3Uri :: Lens.Lens' EntityRecognizerDocuments (Prelude.Maybe Prelude.Text)
entityRecognizerDocuments_testS3Uri = Lens.lens (\EntityRecognizerDocuments' {testS3Uri} -> testS3Uri) (\s@EntityRecognizerDocuments' {} a -> s {testS3Uri = a} :: EntityRecognizerDocuments)

-- | Specifies the Amazon S3 location where the training documents for an
-- entity recognizer are located. The URI must be in the same Region as the
-- API endpoint that you are calling.
entityRecognizerDocuments_s3Uri :: Lens.Lens' EntityRecognizerDocuments Prelude.Text
entityRecognizerDocuments_s3Uri = Lens.lens (\EntityRecognizerDocuments' {s3Uri} -> s3Uri) (\s@EntityRecognizerDocuments' {} a -> s {s3Uri = a} :: EntityRecognizerDocuments)

instance Data.FromJSON EntityRecognizerDocuments where
  parseJSON =
    Data.withObject
      "EntityRecognizerDocuments"
      ( \x ->
          EntityRecognizerDocuments'
            Prelude.<$> (x Data..:? "InputFormat")
            Prelude.<*> (x Data..:? "TestS3Uri")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable EntityRecognizerDocuments where
  hashWithSalt _salt EntityRecognizerDocuments' {..} =
    _salt
      `Prelude.hashWithSalt` inputFormat
      `Prelude.hashWithSalt` testS3Uri
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData EntityRecognizerDocuments where
  rnf EntityRecognizerDocuments' {..} =
    Prelude.rnf inputFormat
      `Prelude.seq` Prelude.rnf testS3Uri
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON EntityRecognizerDocuments where
  toJSON EntityRecognizerDocuments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InputFormat" Data..=) Prelude.<$> inputFormat,
            ("TestS3Uri" Data..=) Prelude.<$> testS3Uri,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
