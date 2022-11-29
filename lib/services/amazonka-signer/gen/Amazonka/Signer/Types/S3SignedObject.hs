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
-- Module      : Amazonka.Signer.Types.S3SignedObject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.S3SignedObject where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The S3 bucket name and key where code signing saved your signed code
-- image.
--
-- /See:/ 'newS3SignedObject' smart constructor.
data S3SignedObject = S3SignedObject'
  { -- | Key name that uniquely identifies a signed code image in your bucket.
    key :: Prelude.Maybe Prelude.Text,
    -- | Name of the S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3SignedObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 's3SignedObject_key' - Key name that uniquely identifies a signed code image in your bucket.
--
-- 'bucketName', 's3SignedObject_bucketName' - Name of the S3 bucket.
newS3SignedObject ::
  S3SignedObject
newS3SignedObject =
  S3SignedObject'
    { key = Prelude.Nothing,
      bucketName = Prelude.Nothing
    }

-- | Key name that uniquely identifies a signed code image in your bucket.
s3SignedObject_key :: Lens.Lens' S3SignedObject (Prelude.Maybe Prelude.Text)
s3SignedObject_key = Lens.lens (\S3SignedObject' {key} -> key) (\s@S3SignedObject' {} a -> s {key = a} :: S3SignedObject)

-- | Name of the S3 bucket.
s3SignedObject_bucketName :: Lens.Lens' S3SignedObject (Prelude.Maybe Prelude.Text)
s3SignedObject_bucketName = Lens.lens (\S3SignedObject' {bucketName} -> bucketName) (\s@S3SignedObject' {} a -> s {bucketName = a} :: S3SignedObject)

instance Core.FromJSON S3SignedObject where
  parseJSON =
    Core.withObject
      "S3SignedObject"
      ( \x ->
          S3SignedObject'
            Prelude.<$> (x Core..:? "key")
            Prelude.<*> (x Core..:? "bucketName")
      )

instance Prelude.Hashable S3SignedObject where
  hashWithSalt _salt S3SignedObject' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData S3SignedObject where
  rnf S3SignedObject' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf bucketName
