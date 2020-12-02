{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImage where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskImageDetail
import Network.AWS.EC2.Types.VolumeDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a disk image.
--
--
--
-- /See:/ 'diskImage' smart constructor.
data DiskImage = DiskImage'
  { _diImage :: !(Maybe DiskImageDetail),
    _diVolume :: !(Maybe VolumeDetail),
    _diDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DiskImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diImage' - Information about the disk image.
--
-- * 'diVolume' - Information about the volume.
--
-- * 'diDescription' - A description of the disk image.
diskImage ::
  DiskImage
diskImage =
  DiskImage'
    { _diImage = Nothing,
      _diVolume = Nothing,
      _diDescription = Nothing
    }

-- | Information about the disk image.
diImage :: Lens' DiskImage (Maybe DiskImageDetail)
diImage = lens _diImage (\s a -> s {_diImage = a})

-- | Information about the volume.
diVolume :: Lens' DiskImage (Maybe VolumeDetail)
diVolume = lens _diVolume (\s a -> s {_diVolume = a})

-- | A description of the disk image.
diDescription :: Lens' DiskImage (Maybe Text)
diDescription = lens _diDescription (\s a -> s {_diDescription = a})

instance Hashable DiskImage

instance NFData DiskImage

instance ToQuery DiskImage where
  toQuery DiskImage' {..} =
    mconcat
      [ "Image" =: _diImage,
        "Volume" =: _diVolume,
        "Description" =: _diDescription
      ]
