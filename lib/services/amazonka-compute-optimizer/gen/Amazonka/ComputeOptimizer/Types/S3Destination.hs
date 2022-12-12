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
-- Module      : Amazonka.ComputeOptimizer.Types.S3Destination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.S3Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination Amazon Simple Storage Service (Amazon S3)
-- bucket name and object keys of a recommendations export file, and its
-- associated metadata file.
--
-- /See:/ 'newS3Destination' smart constructor.
data S3Destination = S3Destination'
  { -- | The name of the Amazon S3 bucket used as the destination of an export
    -- file.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket key of an export file.
    --
    -- The key uniquely identifies the object, or export file, in the S3
    -- bucket.
    key :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket key of a metadata file.
    --
    -- The key uniquely identifies the object, or metadata file, in the S3
    -- bucket.
    metadataKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3Destination_bucket' - The name of the Amazon S3 bucket used as the destination of an export
-- file.
--
-- 'key', 's3Destination_key' - The Amazon S3 bucket key of an export file.
--
-- The key uniquely identifies the object, or export file, in the S3
-- bucket.
--
-- 'metadataKey', 's3Destination_metadataKey' - The Amazon S3 bucket key of a metadata file.
--
-- The key uniquely identifies the object, or metadata file, in the S3
-- bucket.
newS3Destination ::
  S3Destination
newS3Destination =
  S3Destination'
    { bucket = Prelude.Nothing,
      key = Prelude.Nothing,
      metadataKey = Prelude.Nothing
    }

-- | The name of the Amazon S3 bucket used as the destination of an export
-- file.
s3Destination_bucket :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_bucket = Lens.lens (\S3Destination' {bucket} -> bucket) (\s@S3Destination' {} a -> s {bucket = a} :: S3Destination)

-- | The Amazon S3 bucket key of an export file.
--
-- The key uniquely identifies the object, or export file, in the S3
-- bucket.
s3Destination_key :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_key = Lens.lens (\S3Destination' {key} -> key) (\s@S3Destination' {} a -> s {key = a} :: S3Destination)

-- | The Amazon S3 bucket key of a metadata file.
--
-- The key uniquely identifies the object, or metadata file, in the S3
-- bucket.
s3Destination_metadataKey :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_metadataKey = Lens.lens (\S3Destination' {metadataKey} -> metadataKey) (\s@S3Destination' {} a -> s {metadataKey = a} :: S3Destination)

instance Data.FromJSON S3Destination where
  parseJSON =
    Data.withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            Prelude.<$> (x Data..:? "bucket")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "metadataKey")
      )

instance Prelude.Hashable S3Destination where
  hashWithSalt _salt S3Destination' {..} =
    _salt `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` metadataKey

instance Prelude.NFData S3Destination where
  rnf S3Destination' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf metadataKey
