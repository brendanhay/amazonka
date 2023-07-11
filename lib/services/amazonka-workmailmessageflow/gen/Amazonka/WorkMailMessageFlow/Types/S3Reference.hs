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
-- Module      : Amazonka.WorkMailMessageFlow.Types.S3Reference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMailMessageFlow.Types.S3Reference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Amazon S3 object representing the updated message content, in MIME
-- format.
--
-- The region for the S3 bucket containing the S3 object must match the
-- region used for WorkMail operations. Also, for WorkMail to process an S3
-- object, it must have permission to access that object. For more
-- information, see
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/update-with-lambda.html Updating message content with AWS Lambda>.
--
-- /See:/ 'newS3Reference' smart constructor.
data S3Reference = S3Reference'
  { -- | If you enable versioning for the bucket, you can specify the object
    -- version.
    objectVersion :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket name.
    bucket :: Prelude.Text,
    -- | The S3 key object name.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Reference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectVersion', 's3Reference_objectVersion' - If you enable versioning for the bucket, you can specify the object
-- version.
--
-- 'bucket', 's3Reference_bucket' - The S3 bucket name.
--
-- 'key', 's3Reference_key' - The S3 key object name.
newS3Reference ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  S3Reference
newS3Reference pBucket_ pKey_ =
  S3Reference'
    { objectVersion = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | If you enable versioning for the bucket, you can specify the object
-- version.
s3Reference_objectVersion :: Lens.Lens' S3Reference (Prelude.Maybe Prelude.Text)
s3Reference_objectVersion = Lens.lens (\S3Reference' {objectVersion} -> objectVersion) (\s@S3Reference' {} a -> s {objectVersion = a} :: S3Reference)

-- | The S3 bucket name.
s3Reference_bucket :: Lens.Lens' S3Reference Prelude.Text
s3Reference_bucket = Lens.lens (\S3Reference' {bucket} -> bucket) (\s@S3Reference' {} a -> s {bucket = a} :: S3Reference)

-- | The S3 key object name.
s3Reference_key :: Lens.Lens' S3Reference Prelude.Text
s3Reference_key = Lens.lens (\S3Reference' {key} -> key) (\s@S3Reference' {} a -> s {key = a} :: S3Reference)

instance Prelude.Hashable S3Reference where
  hashWithSalt _salt S3Reference' {..} =
    _salt
      `Prelude.hashWithSalt` objectVersion
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData S3Reference where
  rnf S3Reference' {..} =
    Prelude.rnf objectVersion
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToJSON S3Reference where
  toJSON S3Reference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("objectVersion" Data..=) Prelude.<$> objectVersion,
            Prelude.Just ("bucket" Data..= bucket),
            Prelude.Just ("key" Data..= key)
          ]
      )
