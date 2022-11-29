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
-- Module      : Amazonka.Transfer.Types.S3FileLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.S3FileLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the details for the file location for the file that\'s being
-- used in the workflow. Only applicable if you are using S3 storage.
--
-- /See:/ 'newS3FileLocation' smart constructor.
data S3FileLocation = S3FileLocation'
  { -- | The name assigned to the file when it was created in Amazon S3. You use
    -- the object key to retrieve the object.
    key :: Prelude.Maybe Prelude.Text,
    -- | Specifies the S3 bucket that contains the file being used.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The entity tag is a hash of the object. The ETag reflects changes only
    -- to the contents of an object, not its metadata.
    etag :: Prelude.Maybe Prelude.Text,
    -- | Specifies the file version.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3FileLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 's3FileLocation_key' - The name assigned to the file when it was created in Amazon S3. You use
-- the object key to retrieve the object.
--
-- 'bucket', 's3FileLocation_bucket' - Specifies the S3 bucket that contains the file being used.
--
-- 'etag', 's3FileLocation_etag' - The entity tag is a hash of the object. The ETag reflects changes only
-- to the contents of an object, not its metadata.
--
-- 'versionId', 's3FileLocation_versionId' - Specifies the file version.
newS3FileLocation ::
  S3FileLocation
newS3FileLocation =
  S3FileLocation'
    { key = Prelude.Nothing,
      bucket = Prelude.Nothing,
      etag = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The name assigned to the file when it was created in Amazon S3. You use
-- the object key to retrieve the object.
s3FileLocation_key :: Lens.Lens' S3FileLocation (Prelude.Maybe Prelude.Text)
s3FileLocation_key = Lens.lens (\S3FileLocation' {key} -> key) (\s@S3FileLocation' {} a -> s {key = a} :: S3FileLocation)

-- | Specifies the S3 bucket that contains the file being used.
s3FileLocation_bucket :: Lens.Lens' S3FileLocation (Prelude.Maybe Prelude.Text)
s3FileLocation_bucket = Lens.lens (\S3FileLocation' {bucket} -> bucket) (\s@S3FileLocation' {} a -> s {bucket = a} :: S3FileLocation)

-- | The entity tag is a hash of the object. The ETag reflects changes only
-- to the contents of an object, not its metadata.
s3FileLocation_etag :: Lens.Lens' S3FileLocation (Prelude.Maybe Prelude.Text)
s3FileLocation_etag = Lens.lens (\S3FileLocation' {etag} -> etag) (\s@S3FileLocation' {} a -> s {etag = a} :: S3FileLocation)

-- | Specifies the file version.
s3FileLocation_versionId :: Lens.Lens' S3FileLocation (Prelude.Maybe Prelude.Text)
s3FileLocation_versionId = Lens.lens (\S3FileLocation' {versionId} -> versionId) (\s@S3FileLocation' {} a -> s {versionId = a} :: S3FileLocation)

instance Core.FromJSON S3FileLocation where
  parseJSON =
    Core.withObject
      "S3FileLocation"
      ( \x ->
          S3FileLocation'
            Prelude.<$> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "Bucket")
            Prelude.<*> (x Core..:? "Etag")
            Prelude.<*> (x Core..:? "VersionId")
      )

instance Prelude.Hashable S3FileLocation where
  hashWithSalt _salt S3FileLocation' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` etag
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData S3FileLocation where
  rnf S3FileLocation' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf etag
      `Prelude.seq` Prelude.rnf versionId
