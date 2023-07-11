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
-- Module      : Amazonka.LookoutEquipment.Types.S3Object
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.S3Object where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an S3 bucket.
--
-- /See:/ 'newS3Object' smart constructor.
data S3Object = S3Object'
  { -- | The name of the specific S3 bucket.
    bucket :: Prelude.Text,
    -- | The AWS Key Management Service (AWS KMS) key being used to encrypt the
    -- S3 object. Without this key, data in the bucket is not accessible.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Object' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3Object_bucket' - The name of the specific S3 bucket.
--
-- 'key', 's3Object_key' - The AWS Key Management Service (AWS KMS) key being used to encrypt the
-- S3 object. Without this key, data in the bucket is not accessible.
newS3Object ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  S3Object
newS3Object pBucket_ pKey_ =
  S3Object' {bucket = pBucket_, key = pKey_}

-- | The name of the specific S3 bucket.
s3Object_bucket :: Lens.Lens' S3Object Prelude.Text
s3Object_bucket = Lens.lens (\S3Object' {bucket} -> bucket) (\s@S3Object' {} a -> s {bucket = a} :: S3Object)

-- | The AWS Key Management Service (AWS KMS) key being used to encrypt the
-- S3 object. Without this key, data in the bucket is not accessible.
s3Object_key :: Lens.Lens' S3Object Prelude.Text
s3Object_key = Lens.lens (\S3Object' {key} -> key) (\s@S3Object' {} a -> s {key = a} :: S3Object)

instance Data.FromJSON S3Object where
  parseJSON =
    Data.withObject
      "S3Object"
      ( \x ->
          S3Object'
            Prelude.<$> (x Data..: "Bucket")
            Prelude.<*> (x Data..: "Key")
      )

instance Prelude.Hashable S3Object where
  hashWithSalt _salt S3Object' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData S3Object where
  rnf S3Object' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf key
