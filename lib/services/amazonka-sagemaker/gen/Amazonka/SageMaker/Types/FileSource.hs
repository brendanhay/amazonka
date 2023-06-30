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
-- Module      : Amazonka.SageMaker.Types.FileSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FileSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details regarding the file source.
--
-- /See:/ 'newFileSource' smart constructor.
data FileSource = FileSource'
  { -- | The digest of the file source.
    contentDigest :: Prelude.Maybe Prelude.Text,
    -- | The type of content stored in the file source.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI for the file source.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentDigest', 'fileSource_contentDigest' - The digest of the file source.
--
-- 'contentType', 'fileSource_contentType' - The type of content stored in the file source.
--
-- 's3Uri', 'fileSource_s3Uri' - The Amazon S3 URI for the file source.
newFileSource ::
  -- | 's3Uri'
  Prelude.Text ->
  FileSource
newFileSource pS3Uri_ =
  FileSource'
    { contentDigest = Prelude.Nothing,
      contentType = Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | The digest of the file source.
fileSource_contentDigest :: Lens.Lens' FileSource (Prelude.Maybe Prelude.Text)
fileSource_contentDigest = Lens.lens (\FileSource' {contentDigest} -> contentDigest) (\s@FileSource' {} a -> s {contentDigest = a} :: FileSource)

-- | The type of content stored in the file source.
fileSource_contentType :: Lens.Lens' FileSource (Prelude.Maybe Prelude.Text)
fileSource_contentType = Lens.lens (\FileSource' {contentType} -> contentType) (\s@FileSource' {} a -> s {contentType = a} :: FileSource)

-- | The Amazon S3 URI for the file source.
fileSource_s3Uri :: Lens.Lens' FileSource Prelude.Text
fileSource_s3Uri = Lens.lens (\FileSource' {s3Uri} -> s3Uri) (\s@FileSource' {} a -> s {s3Uri = a} :: FileSource)

instance Data.FromJSON FileSource where
  parseJSON =
    Data.withObject
      "FileSource"
      ( \x ->
          FileSource'
            Prelude.<$> (x Data..:? "ContentDigest")
            Prelude.<*> (x Data..:? "ContentType")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable FileSource where
  hashWithSalt _salt FileSource' {..} =
    _salt
      `Prelude.hashWithSalt` contentDigest
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData FileSource where
  rnf FileSource' {..} =
    Prelude.rnf contentDigest
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON FileSource where
  toJSON FileSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentDigest" Data..=) Prelude.<$> contentDigest,
            ("ContentType" Data..=) Prelude.<$> contentType,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
