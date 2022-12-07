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
-- Module      : Amazonka.Kendra.Types.S3Path
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.S3Path where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information required to find a specific file in an Amazon S3 bucket.
--
-- /See:/ 'newS3Path' smart constructor.
data S3Path = S3Path'
  { -- | The name of the S3 bucket that contains the file.
    bucket :: Prelude.Text,
    -- | The name of the file.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Path' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3Path_bucket' - The name of the S3 bucket that contains the file.
--
-- 'key', 's3Path_key' - The name of the file.
newS3Path ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  S3Path
newS3Path pBucket_ pKey_ =
  S3Path' {bucket = pBucket_, key = pKey_}

-- | The name of the S3 bucket that contains the file.
s3Path_bucket :: Lens.Lens' S3Path Prelude.Text
s3Path_bucket = Lens.lens (\S3Path' {bucket} -> bucket) (\s@S3Path' {} a -> s {bucket = a} :: S3Path)

-- | The name of the file.
s3Path_key :: Lens.Lens' S3Path Prelude.Text
s3Path_key = Lens.lens (\S3Path' {key} -> key) (\s@S3Path' {} a -> s {key = a} :: S3Path)

instance Data.FromJSON S3Path where
  parseJSON =
    Data.withObject
      "S3Path"
      ( \x ->
          S3Path'
            Prelude.<$> (x Data..: "Bucket") Prelude.<*> (x Data..: "Key")
      )

instance Prelude.Hashable S3Path where
  hashWithSalt _salt S3Path' {..} =
    _salt `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData S3Path where
  rnf S3Path' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf key

instance Data.ToJSON S3Path where
  toJSON S3Path' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Bucket" Data..= bucket),
            Prelude.Just ("Key" Data..= key)
          ]
      )
