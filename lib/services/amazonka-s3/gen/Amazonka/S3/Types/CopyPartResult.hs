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
-- Module      : Amazonka.S3.Types.CopyPartResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.CopyPartResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Container for all response elements.
--
-- /See:/ 'newCopyPartResult' smart constructor.
data CopyPartResult = CopyPartResult'
  { -- | The base64-encoded, 32-bit CRC32 checksum of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32 :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
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
    -- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA256 :: Prelude.Maybe Prelude.Text,
    -- | Entity tag of the object.
    eTag :: Prelude.Maybe ETag,
    -- | Date and time at which the object was uploaded.
    lastModified :: Prelude.Maybe Data.RFC822
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyPartResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumCRC32', 'copyPartResult_checksumCRC32' - The base64-encoded, 32-bit CRC32 checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumCRC32C', 'copyPartResult_checksumCRC32C' - The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA1', 'copyPartResult_checksumSHA1' - The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA256', 'copyPartResult_checksumSHA256' - The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'eTag', 'copyPartResult_eTag' - Entity tag of the object.
--
-- 'lastModified', 'copyPartResult_lastModified' - Date and time at which the object was uploaded.
newCopyPartResult ::
  CopyPartResult
newCopyPartResult =
  CopyPartResult'
    { checksumCRC32 = Prelude.Nothing,
      checksumCRC32C = Prelude.Nothing,
      checksumSHA1 = Prelude.Nothing,
      checksumSHA256 = Prelude.Nothing,
      eTag = Prelude.Nothing,
      lastModified = Prelude.Nothing
    }

-- | The base64-encoded, 32-bit CRC32 checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
copyPartResult_checksumCRC32 :: Lens.Lens' CopyPartResult (Prelude.Maybe Prelude.Text)
copyPartResult_checksumCRC32 = Lens.lens (\CopyPartResult' {checksumCRC32} -> checksumCRC32) (\s@CopyPartResult' {} a -> s {checksumCRC32 = a} :: CopyPartResult)

-- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
copyPartResult_checksumCRC32C :: Lens.Lens' CopyPartResult (Prelude.Maybe Prelude.Text)
copyPartResult_checksumCRC32C = Lens.lens (\CopyPartResult' {checksumCRC32C} -> checksumCRC32C) (\s@CopyPartResult' {} a -> s {checksumCRC32C = a} :: CopyPartResult)

-- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
copyPartResult_checksumSHA1 :: Lens.Lens' CopyPartResult (Prelude.Maybe Prelude.Text)
copyPartResult_checksumSHA1 = Lens.lens (\CopyPartResult' {checksumSHA1} -> checksumSHA1) (\s@CopyPartResult' {} a -> s {checksumSHA1 = a} :: CopyPartResult)

-- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
copyPartResult_checksumSHA256 :: Lens.Lens' CopyPartResult (Prelude.Maybe Prelude.Text)
copyPartResult_checksumSHA256 = Lens.lens (\CopyPartResult' {checksumSHA256} -> checksumSHA256) (\s@CopyPartResult' {} a -> s {checksumSHA256 = a} :: CopyPartResult)

-- | Entity tag of the object.
copyPartResult_eTag :: Lens.Lens' CopyPartResult (Prelude.Maybe ETag)
copyPartResult_eTag = Lens.lens (\CopyPartResult' {eTag} -> eTag) (\s@CopyPartResult' {} a -> s {eTag = a} :: CopyPartResult)

-- | Date and time at which the object was uploaded.
copyPartResult_lastModified :: Lens.Lens' CopyPartResult (Prelude.Maybe Prelude.UTCTime)
copyPartResult_lastModified = Lens.lens (\CopyPartResult' {lastModified} -> lastModified) (\s@CopyPartResult' {} a -> s {lastModified = a} :: CopyPartResult) Prelude.. Lens.mapping Data._Time

instance Data.FromXML CopyPartResult where
  parseXML x =
    CopyPartResult'
      Prelude.<$> (x Data..@? "ChecksumCRC32")
      Prelude.<*> (x Data..@? "ChecksumCRC32C")
      Prelude.<*> (x Data..@? "ChecksumSHA1")
      Prelude.<*> (x Data..@? "ChecksumSHA256")
      Prelude.<*> (x Data..@? "ETag")
      Prelude.<*> (x Data..@? "LastModified")

instance Prelude.Hashable CopyPartResult where
  hashWithSalt _salt CopyPartResult' {..} =
    _salt
      `Prelude.hashWithSalt` checksumCRC32
      `Prelude.hashWithSalt` checksumCRC32C
      `Prelude.hashWithSalt` checksumSHA1
      `Prelude.hashWithSalt` checksumSHA256
      `Prelude.hashWithSalt` eTag
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData CopyPartResult where
  rnf CopyPartResult' {..} =
    Prelude.rnf checksumCRC32
      `Prelude.seq` Prelude.rnf checksumCRC32C
      `Prelude.seq` Prelude.rnf checksumSHA1
      `Prelude.seq` Prelude.rnf checksumSHA256
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf lastModified
