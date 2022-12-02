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
-- Module      : Amazonka.S3.Types.Part
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Part where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Container for elements related to a part.
--
-- /See:/ 'newPart' smart constructor.
data Part = Part'
  { -- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32C :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA1 :: Prelude.Maybe Prelude.Text,
    -- | This header can be used as a data integrity check to verify that the
    -- data received is the same data that was originally sent. This header
    -- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32 :: Prelude.Maybe Prelude.Text,
    -- | Part number identifying the part. This is a positive integer between 1
    -- and 10,000.
    partNumber :: Prelude.Maybe Prelude.Int,
    -- | Size in bytes of the uploaded part data.
    size :: Prelude.Maybe Prelude.Integer,
    -- | This header can be used as a data integrity check to verify that the
    -- data received is the same data that was originally sent. This header
    -- specifies the base64-encoded, 256-bit SHA-256 digest of the object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA256 :: Prelude.Maybe Prelude.Text,
    -- | Date and time at which the part was uploaded.
    lastModified :: Prelude.Maybe Data.ISO8601,
    -- | Entity tag returned when the part was uploaded.
    eTag :: Prelude.Maybe ETag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Part' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumCRC32C', 'part_checksumCRC32C' - The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA1', 'part_checksumSHA1' - The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumCRC32', 'part_checksumCRC32' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'partNumber', 'part_partNumber' - Part number identifying the part. This is a positive integer between 1
-- and 10,000.
--
-- 'size', 'part_size' - Size in bytes of the uploaded part data.
--
-- 'checksumSHA256', 'part_checksumSHA256' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 256-bit SHA-256 digest of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'lastModified', 'part_lastModified' - Date and time at which the part was uploaded.
--
-- 'eTag', 'part_eTag' - Entity tag returned when the part was uploaded.
newPart ::
  Part
newPart =
  Part'
    { checksumCRC32C = Prelude.Nothing,
      checksumSHA1 = Prelude.Nothing,
      checksumCRC32 = Prelude.Nothing,
      partNumber = Prelude.Nothing,
      size = Prelude.Nothing,
      checksumSHA256 = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      eTag = Prelude.Nothing
    }

-- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
part_checksumCRC32C :: Lens.Lens' Part (Prelude.Maybe Prelude.Text)
part_checksumCRC32C = Lens.lens (\Part' {checksumCRC32C} -> checksumCRC32C) (\s@Part' {} a -> s {checksumCRC32C = a} :: Part)

-- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
part_checksumSHA1 :: Lens.Lens' Part (Prelude.Maybe Prelude.Text)
part_checksumSHA1 = Lens.lens (\Part' {checksumSHA1} -> checksumSHA1) (\s@Part' {} a -> s {checksumSHA1 = a} :: Part)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
part_checksumCRC32 :: Lens.Lens' Part (Prelude.Maybe Prelude.Text)
part_checksumCRC32 = Lens.lens (\Part' {checksumCRC32} -> checksumCRC32) (\s@Part' {} a -> s {checksumCRC32 = a} :: Part)

-- | Part number identifying the part. This is a positive integer between 1
-- and 10,000.
part_partNumber :: Lens.Lens' Part (Prelude.Maybe Prelude.Int)
part_partNumber = Lens.lens (\Part' {partNumber} -> partNumber) (\s@Part' {} a -> s {partNumber = a} :: Part)

-- | Size in bytes of the uploaded part data.
part_size :: Lens.Lens' Part (Prelude.Maybe Prelude.Integer)
part_size = Lens.lens (\Part' {size} -> size) (\s@Part' {} a -> s {size = a} :: Part)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 256-bit SHA-256 digest of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
part_checksumSHA256 :: Lens.Lens' Part (Prelude.Maybe Prelude.Text)
part_checksumSHA256 = Lens.lens (\Part' {checksumSHA256} -> checksumSHA256) (\s@Part' {} a -> s {checksumSHA256 = a} :: Part)

-- | Date and time at which the part was uploaded.
part_lastModified :: Lens.Lens' Part (Prelude.Maybe Prelude.UTCTime)
part_lastModified = Lens.lens (\Part' {lastModified} -> lastModified) (\s@Part' {} a -> s {lastModified = a} :: Part) Prelude.. Lens.mapping Data._Time

-- | Entity tag returned when the part was uploaded.
part_eTag :: Lens.Lens' Part (Prelude.Maybe ETag)
part_eTag = Lens.lens (\Part' {eTag} -> eTag) (\s@Part' {} a -> s {eTag = a} :: Part)

instance Data.FromXML Part where
  parseXML x =
    Part'
      Prelude.<$> (x Data..@? "ChecksumCRC32C")
      Prelude.<*> (x Data..@? "ChecksumSHA1")
      Prelude.<*> (x Data..@? "ChecksumCRC32")
      Prelude.<*> (x Data..@? "PartNumber")
      Prelude.<*> (x Data..@? "Size")
      Prelude.<*> (x Data..@? "ChecksumSHA256")
      Prelude.<*> (x Data..@? "LastModified")
      Prelude.<*> (x Data..@? "ETag")

instance Prelude.Hashable Part where
  hashWithSalt _salt Part' {..} =
    _salt `Prelude.hashWithSalt` checksumCRC32C
      `Prelude.hashWithSalt` checksumSHA1
      `Prelude.hashWithSalt` checksumCRC32
      `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` checksumSHA256
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` eTag

instance Prelude.NFData Part where
  rnf Part' {..} =
    Prelude.rnf checksumCRC32C
      `Prelude.seq` Prelude.rnf checksumSHA1
      `Prelude.seq` Prelude.rnf checksumCRC32
      `Prelude.seq` Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf checksumSHA256
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf eTag
