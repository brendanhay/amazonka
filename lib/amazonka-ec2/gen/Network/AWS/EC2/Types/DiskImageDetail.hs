{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImageDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImageDetail where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskImageFormat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a disk image.
--
--
--
-- /See:/ 'diskImageDetail' smart constructor.
data DiskImageDetail = DiskImageDetail'
  { _didBytes :: !Integer,
    _didFormat :: !DiskImageFormat,
    _didImportManifestURL :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DiskImageDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'didBytes' - The size of the disk image, in GiB.
--
-- * 'didFormat' - The disk image format.
--
-- * 'didImportManifestURL' - A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ . For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
diskImageDetail ::
  -- | 'didBytes'
  Integer ->
  -- | 'didFormat'
  DiskImageFormat ->
  -- | 'didImportManifestURL'
  Text ->
  DiskImageDetail
diskImageDetail pBytes_ pFormat_ pImportManifestURL_ =
  DiskImageDetail'
    { _didBytes = pBytes_,
      _didFormat = pFormat_,
      _didImportManifestURL = pImportManifestURL_
    }

-- | The size of the disk image, in GiB.
didBytes :: Lens' DiskImageDetail Integer
didBytes = lens _didBytes (\s a -> s {_didBytes = a})

-- | The disk image format.
didFormat :: Lens' DiskImageDetail DiskImageFormat
didFormat = lens _didFormat (\s a -> s {_didFormat = a})

-- | A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ . For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
didImportManifestURL :: Lens' DiskImageDetail Text
didImportManifestURL = lens _didImportManifestURL (\s a -> s {_didImportManifestURL = a})

instance Hashable DiskImageDetail

instance NFData DiskImageDetail

instance ToQuery DiskImageDetail where
  toQuery DiskImageDetail' {..} =
    mconcat
      [ "Bytes" =: _didBytes,
        "Format" =: _didFormat,
        "ImportManifestUrl" =: _didImportManifestURL
      ]
