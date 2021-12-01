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
-- Module      : Amazonka.AppFlow.Types.S3SourceProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.S3SourceProperties where

import Amazonka.AppFlow.Types.S3InputFormatConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Amazon S3 is being used as the flow
-- source.
--
-- /See:/ 'newS3SourceProperties' smart constructor.
data S3SourceProperties = S3SourceProperties'
  { s3InputFormatConfig :: Prelude.Maybe S3InputFormatConfig,
    -- | The object key for the Amazon S3 bucket in which the source files are
    -- stored.
    bucketPrefix :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket name where the source files are stored.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3SourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3InputFormatConfig', 's3SourceProperties_s3InputFormatConfig' - Undocumented member.
--
-- 'bucketPrefix', 's3SourceProperties_bucketPrefix' - The object key for the Amazon S3 bucket in which the source files are
-- stored.
--
-- 'bucketName', 's3SourceProperties_bucketName' - The Amazon S3 bucket name where the source files are stored.
newS3SourceProperties ::
  -- | 'bucketName'
  Prelude.Text ->
  S3SourceProperties
newS3SourceProperties pBucketName_ =
  S3SourceProperties'
    { s3InputFormatConfig =
        Prelude.Nothing,
      bucketPrefix = Prelude.Nothing,
      bucketName = pBucketName_
    }

-- | Undocumented member.
s3SourceProperties_s3InputFormatConfig :: Lens.Lens' S3SourceProperties (Prelude.Maybe S3InputFormatConfig)
s3SourceProperties_s3InputFormatConfig = Lens.lens (\S3SourceProperties' {s3InputFormatConfig} -> s3InputFormatConfig) (\s@S3SourceProperties' {} a -> s {s3InputFormatConfig = a} :: S3SourceProperties)

-- | The object key for the Amazon S3 bucket in which the source files are
-- stored.
s3SourceProperties_bucketPrefix :: Lens.Lens' S3SourceProperties (Prelude.Maybe Prelude.Text)
s3SourceProperties_bucketPrefix = Lens.lens (\S3SourceProperties' {bucketPrefix} -> bucketPrefix) (\s@S3SourceProperties' {} a -> s {bucketPrefix = a} :: S3SourceProperties)

-- | The Amazon S3 bucket name where the source files are stored.
s3SourceProperties_bucketName :: Lens.Lens' S3SourceProperties Prelude.Text
s3SourceProperties_bucketName = Lens.lens (\S3SourceProperties' {bucketName} -> bucketName) (\s@S3SourceProperties' {} a -> s {bucketName = a} :: S3SourceProperties)

instance Core.FromJSON S3SourceProperties where
  parseJSON =
    Core.withObject
      "S3SourceProperties"
      ( \x ->
          S3SourceProperties'
            Prelude.<$> (x Core..:? "s3InputFormatConfig")
            Prelude.<*> (x Core..:? "bucketPrefix")
            Prelude.<*> (x Core..: "bucketName")
      )

instance Prelude.Hashable S3SourceProperties where
  hashWithSalt salt' S3SourceProperties' {..} =
    salt' `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` bucketPrefix
      `Prelude.hashWithSalt` s3InputFormatConfig

instance Prelude.NFData S3SourceProperties where
  rnf S3SourceProperties' {..} =
    Prelude.rnf s3InputFormatConfig
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf bucketPrefix

instance Core.ToJSON S3SourceProperties where
  toJSON S3SourceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("s3InputFormatConfig" Core..=)
              Prelude.<$> s3InputFormatConfig,
            ("bucketPrefix" Core..=) Prelude.<$> bucketPrefix,
            Prelude.Just ("bucketName" Core..= bucketName)
          ]
      )
