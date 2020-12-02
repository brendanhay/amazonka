{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImageDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImageDescription where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskImageFormat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a disk image.
--
--
--
-- /See:/ 'diskImageDescription' smart constructor.
data DiskImageDescription = DiskImageDescription'
  { _dSize ::
      !(Maybe Integer),
    _dChecksum :: !(Maybe Text),
    _dFormat :: !(Maybe DiskImageFormat),
    _dImportManifestURL :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DiskImageDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSize' - The size of the disk image, in GiB.
--
-- * 'dChecksum' - The checksum computed for the disk image.
--
-- * 'dFormat' - The disk image format.
--
-- * 'dImportManifestURL' - A presigned URL for the import manifest stored in Amazon S3. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ . For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
diskImageDescription ::
  DiskImageDescription
diskImageDescription =
  DiskImageDescription'
    { _dSize = Nothing,
      _dChecksum = Nothing,
      _dFormat = Nothing,
      _dImportManifestURL = Nothing
    }

-- | The size of the disk image, in GiB.
dSize :: Lens' DiskImageDescription (Maybe Integer)
dSize = lens _dSize (\s a -> s {_dSize = a})

-- | The checksum computed for the disk image.
dChecksum :: Lens' DiskImageDescription (Maybe Text)
dChecksum = lens _dChecksum (\s a -> s {_dChecksum = a})

-- | The disk image format.
dFormat :: Lens' DiskImageDescription (Maybe DiskImageFormat)
dFormat = lens _dFormat (\s a -> s {_dFormat = a})

-- | A presigned URL for the import manifest stored in Amazon S3. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ . For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
dImportManifestURL :: Lens' DiskImageDescription (Maybe Text)
dImportManifestURL = lens _dImportManifestURL (\s a -> s {_dImportManifestURL = a})

instance FromXML DiskImageDescription where
  parseXML x =
    DiskImageDescription'
      <$> (x .@? "size")
      <*> (x .@? "checksum")
      <*> (x .@? "format")
      <*> (x .@? "importManifestUrl")

instance Hashable DiskImageDescription

instance NFData DiskImageDescription
