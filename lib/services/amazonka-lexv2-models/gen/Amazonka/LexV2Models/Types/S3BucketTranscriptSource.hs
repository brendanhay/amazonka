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
-- Module      : Amazonka.LexV2Models.Types.S3BucketTranscriptSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.S3BucketTranscriptSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.PathFormat
import Amazonka.LexV2Models.Types.TranscriptFilter
import Amazonka.LexV2Models.Types.TranscriptFormat
import qualified Amazonka.Prelude as Prelude

-- | The object representing the Amazon S3 bucket containing the transcript,
-- as well as the associated metadata.
--
-- /See:/ 'newS3BucketTranscriptSource' smart constructor.
data S3BucketTranscriptSource = S3BucketTranscriptSource'
  { -- | The ARN of the KMS key that customer use to encrypt their Amazon S3
    -- bucket. Only use this field if your bucket is encrypted using a customer
    -- managed KMS key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The object that contains a path format that will be applied when Amazon
    -- Lex reads the transcript file in the bucket you provide. Specify this
    -- object if you only want Lex to read a subset of files in your Amazon S3
    -- bucket.
    pathFormat :: Prelude.Maybe PathFormat,
    -- | The object that contains the filter which will be applied when Amazon
    -- Lex reads through the Amazon S3 bucket. Specify this object if you want
    -- Amazon Lex to read only a subset of the Amazon S3 bucket based on the
    -- filter you provide.
    transcriptFilter :: Prelude.Maybe TranscriptFilter,
    -- | The name of the bucket containing the transcript and the associated
    -- metadata.
    s3BucketName :: Prelude.Text,
    -- | The format of the transcript content. Currently, Genie only supports the
    -- Amazon Lex transcript format.
    transcriptFormat :: TranscriptFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3BucketTranscriptSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 's3BucketTranscriptSource_kmsKeyArn' - The ARN of the KMS key that customer use to encrypt their Amazon S3
-- bucket. Only use this field if your bucket is encrypted using a customer
-- managed KMS key.
--
-- 'pathFormat', 's3BucketTranscriptSource_pathFormat' - The object that contains a path format that will be applied when Amazon
-- Lex reads the transcript file in the bucket you provide. Specify this
-- object if you only want Lex to read a subset of files in your Amazon S3
-- bucket.
--
-- 'transcriptFilter', 's3BucketTranscriptSource_transcriptFilter' - The object that contains the filter which will be applied when Amazon
-- Lex reads through the Amazon S3 bucket. Specify this object if you want
-- Amazon Lex to read only a subset of the Amazon S3 bucket based on the
-- filter you provide.
--
-- 's3BucketName', 's3BucketTranscriptSource_s3BucketName' - The name of the bucket containing the transcript and the associated
-- metadata.
--
-- 'transcriptFormat', 's3BucketTranscriptSource_transcriptFormat' - The format of the transcript content. Currently, Genie only supports the
-- Amazon Lex transcript format.
newS3BucketTranscriptSource ::
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 'transcriptFormat'
  TranscriptFormat ->
  S3BucketTranscriptSource
newS3BucketTranscriptSource
  pS3BucketName_
  pTranscriptFormat_ =
    S3BucketTranscriptSource'
      { kmsKeyArn =
          Prelude.Nothing,
        pathFormat = Prelude.Nothing,
        transcriptFilter = Prelude.Nothing,
        s3BucketName = pS3BucketName_,
        transcriptFormat = pTranscriptFormat_
      }

-- | The ARN of the KMS key that customer use to encrypt their Amazon S3
-- bucket. Only use this field if your bucket is encrypted using a customer
-- managed KMS key.
s3BucketTranscriptSource_kmsKeyArn :: Lens.Lens' S3BucketTranscriptSource (Prelude.Maybe Prelude.Text)
s3BucketTranscriptSource_kmsKeyArn = Lens.lens (\S3BucketTranscriptSource' {kmsKeyArn} -> kmsKeyArn) (\s@S3BucketTranscriptSource' {} a -> s {kmsKeyArn = a} :: S3BucketTranscriptSource)

-- | The object that contains a path format that will be applied when Amazon
-- Lex reads the transcript file in the bucket you provide. Specify this
-- object if you only want Lex to read a subset of files in your Amazon S3
-- bucket.
s3BucketTranscriptSource_pathFormat :: Lens.Lens' S3BucketTranscriptSource (Prelude.Maybe PathFormat)
s3BucketTranscriptSource_pathFormat = Lens.lens (\S3BucketTranscriptSource' {pathFormat} -> pathFormat) (\s@S3BucketTranscriptSource' {} a -> s {pathFormat = a} :: S3BucketTranscriptSource)

-- | The object that contains the filter which will be applied when Amazon
-- Lex reads through the Amazon S3 bucket. Specify this object if you want
-- Amazon Lex to read only a subset of the Amazon S3 bucket based on the
-- filter you provide.
s3BucketTranscriptSource_transcriptFilter :: Lens.Lens' S3BucketTranscriptSource (Prelude.Maybe TranscriptFilter)
s3BucketTranscriptSource_transcriptFilter = Lens.lens (\S3BucketTranscriptSource' {transcriptFilter} -> transcriptFilter) (\s@S3BucketTranscriptSource' {} a -> s {transcriptFilter = a} :: S3BucketTranscriptSource)

-- | The name of the bucket containing the transcript and the associated
-- metadata.
s3BucketTranscriptSource_s3BucketName :: Lens.Lens' S3BucketTranscriptSource Prelude.Text
s3BucketTranscriptSource_s3BucketName = Lens.lens (\S3BucketTranscriptSource' {s3BucketName} -> s3BucketName) (\s@S3BucketTranscriptSource' {} a -> s {s3BucketName = a} :: S3BucketTranscriptSource)

-- | The format of the transcript content. Currently, Genie only supports the
-- Amazon Lex transcript format.
s3BucketTranscriptSource_transcriptFormat :: Lens.Lens' S3BucketTranscriptSource TranscriptFormat
s3BucketTranscriptSource_transcriptFormat = Lens.lens (\S3BucketTranscriptSource' {transcriptFormat} -> transcriptFormat) (\s@S3BucketTranscriptSource' {} a -> s {transcriptFormat = a} :: S3BucketTranscriptSource)

instance Data.FromJSON S3BucketTranscriptSource where
  parseJSON =
    Data.withObject
      "S3BucketTranscriptSource"
      ( \x ->
          S3BucketTranscriptSource'
            Prelude.<$> (x Data..:? "kmsKeyArn")
            Prelude.<*> (x Data..:? "pathFormat")
            Prelude.<*> (x Data..:? "transcriptFilter")
            Prelude.<*> (x Data..: "s3BucketName")
            Prelude.<*> (x Data..: "transcriptFormat")
      )

instance Prelude.Hashable S3BucketTranscriptSource where
  hashWithSalt _salt S3BucketTranscriptSource' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` pathFormat
      `Prelude.hashWithSalt` transcriptFilter
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` transcriptFormat

instance Prelude.NFData S3BucketTranscriptSource where
  rnf S3BucketTranscriptSource' {..} =
    Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf pathFormat
      `Prelude.seq` Prelude.rnf transcriptFilter
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf transcriptFormat

instance Data.ToJSON S3BucketTranscriptSource where
  toJSON S3BucketTranscriptSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("pathFormat" Data..=) Prelude.<$> pathFormat,
            ("transcriptFilter" Data..=)
              Prelude.<$> transcriptFilter,
            Prelude.Just ("s3BucketName" Data..= s3BucketName),
            Prelude.Just
              ("transcriptFormat" Data..= transcriptFormat)
          ]
      )
