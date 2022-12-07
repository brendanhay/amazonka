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
-- Module      : Amazonka.S3.Types.ObjectPart
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectPart where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | A container for elements related to an individual part.
--
-- /See:/ 'newObjectPart' smart constructor.
data ObjectPart = ObjectPart'
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
    -- | The part number identifying the part. This value is a positive integer
    -- between 1 and 10,000.
    partNumber :: Prelude.Maybe Prelude.Int,
    -- | The size of the uploaded part in bytes.
    size :: Prelude.Maybe Prelude.Integer,
    -- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA256 :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumCRC32C', 'objectPart_checksumCRC32C' - The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA1', 'objectPart_checksumSHA1' - The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumCRC32', 'objectPart_checksumCRC32' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'partNumber', 'objectPart_partNumber' - The part number identifying the part. This value is a positive integer
-- between 1 and 10,000.
--
-- 'size', 'objectPart_size' - The size of the uploaded part in bytes.
--
-- 'checksumSHA256', 'objectPart_checksumSHA256' - The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
newObjectPart ::
  ObjectPart
newObjectPart =
  ObjectPart'
    { checksumCRC32C = Prelude.Nothing,
      checksumSHA1 = Prelude.Nothing,
      checksumCRC32 = Prelude.Nothing,
      partNumber = Prelude.Nothing,
      size = Prelude.Nothing,
      checksumSHA256 = Prelude.Nothing
    }

-- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
objectPart_checksumCRC32C :: Lens.Lens' ObjectPart (Prelude.Maybe Prelude.Text)
objectPart_checksumCRC32C = Lens.lens (\ObjectPart' {checksumCRC32C} -> checksumCRC32C) (\s@ObjectPart' {} a -> s {checksumCRC32C = a} :: ObjectPart)

-- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
objectPart_checksumSHA1 :: Lens.Lens' ObjectPart (Prelude.Maybe Prelude.Text)
objectPart_checksumSHA1 = Lens.lens (\ObjectPart' {checksumSHA1} -> checksumSHA1) (\s@ObjectPart' {} a -> s {checksumSHA1 = a} :: ObjectPart)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
objectPart_checksumCRC32 :: Lens.Lens' ObjectPart (Prelude.Maybe Prelude.Text)
objectPart_checksumCRC32 = Lens.lens (\ObjectPart' {checksumCRC32} -> checksumCRC32) (\s@ObjectPart' {} a -> s {checksumCRC32 = a} :: ObjectPart)

-- | The part number identifying the part. This value is a positive integer
-- between 1 and 10,000.
objectPart_partNumber :: Lens.Lens' ObjectPart (Prelude.Maybe Prelude.Int)
objectPart_partNumber = Lens.lens (\ObjectPart' {partNumber} -> partNumber) (\s@ObjectPart' {} a -> s {partNumber = a} :: ObjectPart)

-- | The size of the uploaded part in bytes.
objectPart_size :: Lens.Lens' ObjectPart (Prelude.Maybe Prelude.Integer)
objectPart_size = Lens.lens (\ObjectPart' {size} -> size) (\s@ObjectPart' {} a -> s {size = a} :: ObjectPart)

-- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
objectPart_checksumSHA256 :: Lens.Lens' ObjectPart (Prelude.Maybe Prelude.Text)
objectPart_checksumSHA256 = Lens.lens (\ObjectPart' {checksumSHA256} -> checksumSHA256) (\s@ObjectPart' {} a -> s {checksumSHA256 = a} :: ObjectPart)

instance Data.FromXML ObjectPart where
  parseXML x =
    ObjectPart'
      Prelude.<$> (x Data..@? "ChecksumCRC32C")
      Prelude.<*> (x Data..@? "ChecksumSHA1")
      Prelude.<*> (x Data..@? "ChecksumCRC32")
      Prelude.<*> (x Data..@? "PartNumber")
      Prelude.<*> (x Data..@? "Size")
      Prelude.<*> (x Data..@? "ChecksumSHA256")

instance Prelude.Hashable ObjectPart where
  hashWithSalt _salt ObjectPart' {..} =
    _salt `Prelude.hashWithSalt` checksumCRC32C
      `Prelude.hashWithSalt` checksumSHA1
      `Prelude.hashWithSalt` checksumCRC32
      `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` checksumSHA256

instance Prelude.NFData ObjectPart where
  rnf ObjectPart' {..} =
    Prelude.rnf checksumCRC32C
      `Prelude.seq` Prelude.rnf checksumSHA1
      `Prelude.seq` Prelude.rnf checksumCRC32
      `Prelude.seq` Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf checksumSHA256
