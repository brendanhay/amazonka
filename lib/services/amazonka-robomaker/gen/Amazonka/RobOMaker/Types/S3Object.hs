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
-- Module      : Amazonka.RobOMaker.Types.S3Object
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.S3Object where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an S3 object.
--
-- /See:/ 'newS3Object' smart constructor.
data S3Object = S3Object'
  { -- | The etag of the object.
    etag :: Prelude.Maybe Prelude.Text,
    -- | The bucket containing the object.
    bucket :: Prelude.Text,
    -- | The key of the object.
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
-- 'etag', 's3Object_etag' - The etag of the object.
--
-- 'bucket', 's3Object_bucket' - The bucket containing the object.
--
-- 'key', 's3Object_key' - The key of the object.
newS3Object ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  S3Object
newS3Object pBucket_ pKey_ =
  S3Object'
    { etag = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The etag of the object.
s3Object_etag :: Lens.Lens' S3Object (Prelude.Maybe Prelude.Text)
s3Object_etag = Lens.lens (\S3Object' {etag} -> etag) (\s@S3Object' {} a -> s {etag = a} :: S3Object)

-- | The bucket containing the object.
s3Object_bucket :: Lens.Lens' S3Object Prelude.Text
s3Object_bucket = Lens.lens (\S3Object' {bucket} -> bucket) (\s@S3Object' {} a -> s {bucket = a} :: S3Object)

-- | The key of the object.
s3Object_key :: Lens.Lens' S3Object Prelude.Text
s3Object_key = Lens.lens (\S3Object' {key} -> key) (\s@S3Object' {} a -> s {key = a} :: S3Object)

instance Core.FromJSON S3Object where
  parseJSON =
    Core.withObject
      "S3Object"
      ( \x ->
          S3Object'
            Prelude.<$> (x Core..:? "etag")
            Prelude.<*> (x Core..: "bucket")
            Prelude.<*> (x Core..: "key")
      )

instance Prelude.Hashable S3Object where
  hashWithSalt _salt S3Object' {..} =
    _salt `Prelude.hashWithSalt` etag
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData S3Object where
  rnf S3Object' {..} =
    Prelude.rnf etag
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Core.ToJSON S3Object where
  toJSON S3Object' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("etag" Core..=) Prelude.<$> etag,
            Prelude.Just ("bucket" Core..= bucket),
            Prelude.Just ("key" Core..= key)
          ]
      )
