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
-- Module      : Amazonka.S3.Types.CompletedPart
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.CompletedPart where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Details of the parts that were uploaded.
--
-- /See:/ 'newCompletedPart' smart constructor.
data CompletedPart = CompletedPart'
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
    -- | Part number that identifies the part. This is a positive integer between
    -- 1 and 10,000.
    partNumber :: Prelude.Int,
    -- | Entity tag returned when the part was uploaded.
    eTag :: ETag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompletedPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumCRC32', 'completedPart_checksumCRC32' - The base64-encoded, 32-bit CRC32 checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumCRC32C', 'completedPart_checksumCRC32C' - The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA1', 'completedPart_checksumSHA1' - The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA256', 'completedPart_checksumSHA256' - The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'partNumber', 'completedPart_partNumber' - Part number that identifies the part. This is a positive integer between
-- 1 and 10,000.
--
-- 'eTag', 'completedPart_eTag' - Entity tag returned when the part was uploaded.
newCompletedPart ::
  -- | 'partNumber'
  Prelude.Int ->
  -- | 'eTag'
  ETag ->
  CompletedPart
newCompletedPart pPartNumber_ pETag_ =
  CompletedPart'
    { checksumCRC32 = Prelude.Nothing,
      checksumCRC32C = Prelude.Nothing,
      checksumSHA1 = Prelude.Nothing,
      checksumSHA256 = Prelude.Nothing,
      partNumber = pPartNumber_,
      eTag = pETag_
    }

-- | The base64-encoded, 32-bit CRC32 checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
completedPart_checksumCRC32 :: Lens.Lens' CompletedPart (Prelude.Maybe Prelude.Text)
completedPart_checksumCRC32 = Lens.lens (\CompletedPart' {checksumCRC32} -> checksumCRC32) (\s@CompletedPart' {} a -> s {checksumCRC32 = a} :: CompletedPart)

-- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
completedPart_checksumCRC32C :: Lens.Lens' CompletedPart (Prelude.Maybe Prelude.Text)
completedPart_checksumCRC32C = Lens.lens (\CompletedPart' {checksumCRC32C} -> checksumCRC32C) (\s@CompletedPart' {} a -> s {checksumCRC32C = a} :: CompletedPart)

-- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
completedPart_checksumSHA1 :: Lens.Lens' CompletedPart (Prelude.Maybe Prelude.Text)
completedPart_checksumSHA1 = Lens.lens (\CompletedPart' {checksumSHA1} -> checksumSHA1) (\s@CompletedPart' {} a -> s {checksumSHA1 = a} :: CompletedPart)

-- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
completedPart_checksumSHA256 :: Lens.Lens' CompletedPart (Prelude.Maybe Prelude.Text)
completedPart_checksumSHA256 = Lens.lens (\CompletedPart' {checksumSHA256} -> checksumSHA256) (\s@CompletedPart' {} a -> s {checksumSHA256 = a} :: CompletedPart)

-- | Part number that identifies the part. This is a positive integer between
-- 1 and 10,000.
completedPart_partNumber :: Lens.Lens' CompletedPart Prelude.Int
completedPart_partNumber = Lens.lens (\CompletedPart' {partNumber} -> partNumber) (\s@CompletedPart' {} a -> s {partNumber = a} :: CompletedPart)

-- | Entity tag returned when the part was uploaded.
completedPart_eTag :: Lens.Lens' CompletedPart ETag
completedPart_eTag = Lens.lens (\CompletedPart' {eTag} -> eTag) (\s@CompletedPart' {} a -> s {eTag = a} :: CompletedPart)

instance Prelude.Hashable CompletedPart where
  hashWithSalt _salt CompletedPart' {..} =
    _salt `Prelude.hashWithSalt` checksumCRC32
      `Prelude.hashWithSalt` checksumCRC32C
      `Prelude.hashWithSalt` checksumSHA1
      `Prelude.hashWithSalt` checksumSHA256
      `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` eTag

instance Prelude.NFData CompletedPart where
  rnf CompletedPart' {..} =
    Prelude.rnf checksumCRC32
      `Prelude.seq` Prelude.rnf checksumCRC32C
      `Prelude.seq` Prelude.rnf checksumSHA1
      `Prelude.seq` Prelude.rnf checksumSHA256
      `Prelude.seq` Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf eTag

instance Data.ToXML CompletedPart where
  toXML CompletedPart' {..} =
    Prelude.mconcat
      [ "ChecksumCRC32" Data.@= checksumCRC32,
        "ChecksumCRC32C" Data.@= checksumCRC32C,
        "ChecksumSHA1" Data.@= checksumSHA1,
        "ChecksumSHA256" Data.@= checksumSHA256,
        "PartNumber" Data.@= partNumber,
        "ETag" Data.@= eTag
      ]
