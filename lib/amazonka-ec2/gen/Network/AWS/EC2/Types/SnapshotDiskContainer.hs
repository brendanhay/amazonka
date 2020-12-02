{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotDiskContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotDiskContainer where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UserBucket
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The disk container object for the import snapshot request.
--
--
--
-- /See:/ 'snapshotDiskContainer' smart constructor.
data SnapshotDiskContainer = SnapshotDiskContainer'
  { _sdcFormat ::
      !(Maybe Text),
    _sdcURL :: !(Maybe Text),
    _sdcUserBucket :: !(Maybe UserBucket),
    _sdcDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SnapshotDiskContainer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcFormat' - The format of the disk image being imported. Valid values: @VHD@ | @VMDK@
--
-- * 'sdcURL' - The URL to the Amazon S3-based disk image being imported. It can either be a https URL (https://..) or an Amazon S3 URL (s3://..).
--
-- * 'sdcUserBucket' - The Amazon S3 bucket for the disk image.
--
-- * 'sdcDescription' - The description of the disk image being imported.
snapshotDiskContainer ::
  SnapshotDiskContainer
snapshotDiskContainer =
  SnapshotDiskContainer'
    { _sdcFormat = Nothing,
      _sdcURL = Nothing,
      _sdcUserBucket = Nothing,
      _sdcDescription = Nothing
    }

-- | The format of the disk image being imported. Valid values: @VHD@ | @VMDK@
sdcFormat :: Lens' SnapshotDiskContainer (Maybe Text)
sdcFormat = lens _sdcFormat (\s a -> s {_sdcFormat = a})

-- | The URL to the Amazon S3-based disk image being imported. It can either be a https URL (https://..) or an Amazon S3 URL (s3://..).
sdcURL :: Lens' SnapshotDiskContainer (Maybe Text)
sdcURL = lens _sdcURL (\s a -> s {_sdcURL = a})

-- | The Amazon S3 bucket for the disk image.
sdcUserBucket :: Lens' SnapshotDiskContainer (Maybe UserBucket)
sdcUserBucket = lens _sdcUserBucket (\s a -> s {_sdcUserBucket = a})

-- | The description of the disk image being imported.
sdcDescription :: Lens' SnapshotDiskContainer (Maybe Text)
sdcDescription = lens _sdcDescription (\s a -> s {_sdcDescription = a})

instance Hashable SnapshotDiskContainer

instance NFData SnapshotDiskContainer

instance ToQuery SnapshotDiskContainer where
  toQuery SnapshotDiskContainer' {..} =
    mconcat
      [ "Format" =: _sdcFormat,
        "Url" =: _sdcURL,
        "UserBucket" =: _sdcUserBucket,
        "Description" =: _sdcDescription
      ]
