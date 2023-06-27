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
-- Module      : Amazonka.Comprehend.Types.DocumentClassifierDocuments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentClassifierDocuments where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The location of the training documents. This parameter is required in a
-- request to create a native classifier model.
--
-- /See:/ 'newDocumentClassifierDocuments' smart constructor.
data DocumentClassifierDocuments = DocumentClassifierDocuments'
  { -- | The S3 URI location of the test documents included in the TestS3Uri CSV
    -- file. This field is not required if you do not specify a test CSV file.
    testS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The S3 URI location of the training documents specified in the S3Uri CSV
    -- file.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentClassifierDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testS3Uri', 'documentClassifierDocuments_testS3Uri' - The S3 URI location of the test documents included in the TestS3Uri CSV
-- file. This field is not required if you do not specify a test CSV file.
--
-- 's3Uri', 'documentClassifierDocuments_s3Uri' - The S3 URI location of the training documents specified in the S3Uri CSV
-- file.
newDocumentClassifierDocuments ::
  -- | 's3Uri'
  Prelude.Text ->
  DocumentClassifierDocuments
newDocumentClassifierDocuments pS3Uri_ =
  DocumentClassifierDocuments'
    { testS3Uri =
        Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | The S3 URI location of the test documents included in the TestS3Uri CSV
-- file. This field is not required if you do not specify a test CSV file.
documentClassifierDocuments_testS3Uri :: Lens.Lens' DocumentClassifierDocuments (Prelude.Maybe Prelude.Text)
documentClassifierDocuments_testS3Uri = Lens.lens (\DocumentClassifierDocuments' {testS3Uri} -> testS3Uri) (\s@DocumentClassifierDocuments' {} a -> s {testS3Uri = a} :: DocumentClassifierDocuments)

-- | The S3 URI location of the training documents specified in the S3Uri CSV
-- file.
documentClassifierDocuments_s3Uri :: Lens.Lens' DocumentClassifierDocuments Prelude.Text
documentClassifierDocuments_s3Uri = Lens.lens (\DocumentClassifierDocuments' {s3Uri} -> s3Uri) (\s@DocumentClassifierDocuments' {} a -> s {s3Uri = a} :: DocumentClassifierDocuments)

instance Data.FromJSON DocumentClassifierDocuments where
  parseJSON =
    Data.withObject
      "DocumentClassifierDocuments"
      ( \x ->
          DocumentClassifierDocuments'
            Prelude.<$> (x Data..:? "TestS3Uri")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable DocumentClassifierDocuments where
  hashWithSalt _salt DocumentClassifierDocuments' {..} =
    _salt
      `Prelude.hashWithSalt` testS3Uri
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData DocumentClassifierDocuments where
  rnf DocumentClassifierDocuments' {..} =
    Prelude.rnf testS3Uri
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON DocumentClassifierDocuments where
  toJSON DocumentClassifierDocuments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TestS3Uri" Data..=) Prelude.<$> testS3Uri,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
