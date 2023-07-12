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
-- Module      : Amazonka.FinSpaceData.Types.S3Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The location of an external Dataview in an S3 bucket.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The name of the S3 bucket.
    bucket :: Prelude.Text,
    -- | The path of the folder, within the S3 bucket that contains the Dataset.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3Location_bucket' - The name of the S3 bucket.
--
-- 'key', 's3Location_key' - The path of the folder, within the S3 bucket that contains the Dataset.
newS3Location ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  S3Location
newS3Location pBucket_ pKey_ =
  S3Location' {bucket = pBucket_, key = pKey_}

-- | The name of the S3 bucket.
s3Location_bucket :: Lens.Lens' S3Location Prelude.Text
s3Location_bucket = Lens.lens (\S3Location' {bucket} -> bucket) (\s@S3Location' {} a -> s {bucket = a} :: S3Location)

-- | The path of the folder, within the S3 bucket that contains the Dataset.
s3Location_key :: Lens.Lens' S3Location Prelude.Text
s3Location_key = Lens.lens (\S3Location' {key} -> key) (\s@S3Location' {} a -> s {key = a} :: S3Location)

instance Data.FromJSON S3Location where
  parseJSON =
    Data.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Data..: "bucket")
            Prelude.<*> (x Data..: "key")
      )

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf key
