{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.DiskImageDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImageDetail where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskImageFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a disk image.
--
-- /See:/ 'newDiskImageDetail' smart constructor.
data DiskImageDetail = DiskImageDetail'
  { -- | The size of the disk image, in GiB.
    bytes :: Prelude.Integer,
    -- | The disk image format.
    format :: DiskImageFormat,
    -- | A presigned URL for the import manifest stored in Amazon S3 and
    -- presented here as an Amazon S3 presigned URL. For information about
    -- creating a presigned URL for an Amazon S3 object, read the \"Query
    -- String Request Authentication Alternative\" section of the
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests>
    -- topic in the /Amazon Simple Storage Service Developer Guide/.
    --
    -- For information about the import manifest referenced by this API action,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
    importManifestUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DiskImageDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytes', 'diskImageDetail_bytes' - The size of the disk image, in GiB.
--
-- 'format', 'diskImageDetail_format' - The disk image format.
--
-- 'importManifestUrl', 'diskImageDetail_importManifestUrl' - A presigned URL for the import manifest stored in Amazon S3 and
-- presented here as an Amazon S3 presigned URL. For information about
-- creating a presigned URL for an Amazon S3 object, read the \"Query
-- String Request Authentication Alternative\" section of the
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests>
-- topic in the /Amazon Simple Storage Service Developer Guide/.
--
-- For information about the import manifest referenced by this API action,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
newDiskImageDetail ::
  -- | 'bytes'
  Prelude.Integer ->
  -- | 'format'
  DiskImageFormat ->
  -- | 'importManifestUrl'
  Prelude.Text ->
  DiskImageDetail
newDiskImageDetail
  pBytes_
  pFormat_
  pImportManifestUrl_ =
    DiskImageDetail'
      { bytes = pBytes_,
        format = pFormat_,
        importManifestUrl = pImportManifestUrl_
      }

-- | The size of the disk image, in GiB.
diskImageDetail_bytes :: Lens.Lens' DiskImageDetail Prelude.Integer
diskImageDetail_bytes = Lens.lens (\DiskImageDetail' {bytes} -> bytes) (\s@DiskImageDetail' {} a -> s {bytes = a} :: DiskImageDetail)

-- | The disk image format.
diskImageDetail_format :: Lens.Lens' DiskImageDetail DiskImageFormat
diskImageDetail_format = Lens.lens (\DiskImageDetail' {format} -> format) (\s@DiskImageDetail' {} a -> s {format = a} :: DiskImageDetail)

-- | A presigned URL for the import manifest stored in Amazon S3 and
-- presented here as an Amazon S3 presigned URL. For information about
-- creating a presigned URL for an Amazon S3 object, read the \"Query
-- String Request Authentication Alternative\" section of the
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests>
-- topic in the /Amazon Simple Storage Service Developer Guide/.
--
-- For information about the import manifest referenced by this API action,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
diskImageDetail_importManifestUrl :: Lens.Lens' DiskImageDetail Prelude.Text
diskImageDetail_importManifestUrl = Lens.lens (\DiskImageDetail' {importManifestUrl} -> importManifestUrl) (\s@DiskImageDetail' {} a -> s {importManifestUrl = a} :: DiskImageDetail)

instance Prelude.Hashable DiskImageDetail

instance Prelude.NFData DiskImageDetail

instance Prelude.ToQuery DiskImageDetail where
  toQuery DiskImageDetail' {..} =
    Prelude.mconcat
      [ "Bytes" Prelude.=: bytes,
        "Format" Prelude.=: format,
        "ImportManifestUrl" Prelude.=: importManifestUrl
      ]
