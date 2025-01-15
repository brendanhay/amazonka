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
-- Module      : Amazonka.MigrationHubStrategy.Types.S3Object
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.S3Object where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the S3 bucket name and the Amazon S3 key name.
--
-- /See:/ 'newS3Object' smart constructor.
data S3Object = S3Object'
  { -- | The S3 bucket name.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key name.
    s3key :: Prelude.Maybe Prelude.Text
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
-- 's3Bucket', 's3Object_s3Bucket' - The S3 bucket name.
--
-- 's3key', 's3Object_s3key' - The Amazon S3 key name.
newS3Object ::
  S3Object
newS3Object =
  S3Object'
    { s3Bucket = Prelude.Nothing,
      s3key = Prelude.Nothing
    }

-- | The S3 bucket name.
s3Object_s3Bucket :: Lens.Lens' S3Object (Prelude.Maybe Prelude.Text)
s3Object_s3Bucket = Lens.lens (\S3Object' {s3Bucket} -> s3Bucket) (\s@S3Object' {} a -> s {s3Bucket = a} :: S3Object)

-- | The Amazon S3 key name.
s3Object_s3key :: Lens.Lens' S3Object (Prelude.Maybe Prelude.Text)
s3Object_s3key = Lens.lens (\S3Object' {s3key} -> s3key) (\s@S3Object' {} a -> s {s3key = a} :: S3Object)

instance Data.FromJSON S3Object where
  parseJSON =
    Data.withObject
      "S3Object"
      ( \x ->
          S3Object'
            Prelude.<$> (x Data..:? "s3Bucket")
            Prelude.<*> (x Data..:? "s3key")
      )

instance Prelude.Hashable S3Object where
  hashWithSalt _salt S3Object' {..} =
    _salt
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3key

instance Prelude.NFData S3Object where
  rnf S3Object' {..} =
    Prelude.rnf s3Bucket `Prelude.seq`
      Prelude.rnf s3key
