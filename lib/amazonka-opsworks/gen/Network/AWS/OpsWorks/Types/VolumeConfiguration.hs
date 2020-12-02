{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.VolumeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.VolumeConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Amazon EBS volume configuration.
--
--
--
-- /See:/ 'volumeConfiguration' smart constructor.
data VolumeConfiguration = VolumeConfiguration'
  { _vcIOPS ::
      !(Maybe Int),
    _vcRAIdLevel :: !(Maybe Int),
    _vcEncrypted :: !(Maybe Bool),
    _vcVolumeType :: !(Maybe Text),
    _vcMountPoint :: !Text,
    _vcNumberOfDisks :: !Int,
    _vcSize :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcIOPS' - For PIOPS volumes, the IOPS per disk.
--
-- * 'vcRAIdLevel' - The volume <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
--
-- * 'vcEncrypted' - Specifies whether an Amazon EBS volume is encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
--
-- * 'vcVolumeType' - The volume type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .     * @standard@ - Magnetic. Magnetic volumes must have a minimum size of 1 GiB and a maximum size of 1024 GiB.     * @io1@ - Provisioned IOPS (SSD). PIOPS volumes must have a minimum size of 4 GiB and a maximum size of 16384 GiB.     * @gp2@ - General Purpose (SSD). General purpose volumes must have a minimum size of 1 GiB and a maximum size of 16384 GiB.     * @st1@ - Throughput Optimized hard disk drive (HDD). Throughput optimized HDD volumes must have a minimum size of 500 GiB and a maximum size of 16384 GiB.     * @sc1@ - Cold HDD. Cold HDD volumes must have a minimum size of 500 GiB and a maximum size of 16384 GiB.
--
-- * 'vcMountPoint' - The volume mount point. For example "/dev/sdh".
--
-- * 'vcNumberOfDisks' - The number of disks in the volume.
--
-- * 'vcSize' - The volume size.
volumeConfiguration ::
  -- | 'vcMountPoint'
  Text ->
  -- | 'vcNumberOfDisks'
  Int ->
  -- | 'vcSize'
  Int ->
  VolumeConfiguration
volumeConfiguration pMountPoint_ pNumberOfDisks_ pSize_ =
  VolumeConfiguration'
    { _vcIOPS = Nothing,
      _vcRAIdLevel = Nothing,
      _vcEncrypted = Nothing,
      _vcVolumeType = Nothing,
      _vcMountPoint = pMountPoint_,
      _vcNumberOfDisks = pNumberOfDisks_,
      _vcSize = pSize_
    }

-- | For PIOPS volumes, the IOPS per disk.
vcIOPS :: Lens' VolumeConfiguration (Maybe Int)
vcIOPS = lens _vcIOPS (\s a -> s {_vcIOPS = a})

-- | The volume <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
vcRAIdLevel :: Lens' VolumeConfiguration (Maybe Int)
vcRAIdLevel = lens _vcRAIdLevel (\s a -> s {_vcRAIdLevel = a})

-- | Specifies whether an Amazon EBS volume is encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
vcEncrypted :: Lens' VolumeConfiguration (Maybe Bool)
vcEncrypted = lens _vcEncrypted (\s a -> s {_vcEncrypted = a})

-- | The volume type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .     * @standard@ - Magnetic. Magnetic volumes must have a minimum size of 1 GiB and a maximum size of 1024 GiB.     * @io1@ - Provisioned IOPS (SSD). PIOPS volumes must have a minimum size of 4 GiB and a maximum size of 16384 GiB.     * @gp2@ - General Purpose (SSD). General purpose volumes must have a minimum size of 1 GiB and a maximum size of 16384 GiB.     * @st1@ - Throughput Optimized hard disk drive (HDD). Throughput optimized HDD volumes must have a minimum size of 500 GiB and a maximum size of 16384 GiB.     * @sc1@ - Cold HDD. Cold HDD volumes must have a minimum size of 500 GiB and a maximum size of 16384 GiB.
vcVolumeType :: Lens' VolumeConfiguration (Maybe Text)
vcVolumeType = lens _vcVolumeType (\s a -> s {_vcVolumeType = a})

-- | The volume mount point. For example "/dev/sdh".
vcMountPoint :: Lens' VolumeConfiguration Text
vcMountPoint = lens _vcMountPoint (\s a -> s {_vcMountPoint = a})

-- | The number of disks in the volume.
vcNumberOfDisks :: Lens' VolumeConfiguration Int
vcNumberOfDisks = lens _vcNumberOfDisks (\s a -> s {_vcNumberOfDisks = a})

-- | The volume size.
vcSize :: Lens' VolumeConfiguration Int
vcSize = lens _vcSize (\s a -> s {_vcSize = a})

instance FromJSON VolumeConfiguration where
  parseJSON =
    withObject
      "VolumeConfiguration"
      ( \x ->
          VolumeConfiguration'
            <$> (x .:? "Iops")
            <*> (x .:? "RaidLevel")
            <*> (x .:? "Encrypted")
            <*> (x .:? "VolumeType")
            <*> (x .: "MountPoint")
            <*> (x .: "NumberOfDisks")
            <*> (x .: "Size")
      )

instance Hashable VolumeConfiguration

instance NFData VolumeConfiguration

instance ToJSON VolumeConfiguration where
  toJSON VolumeConfiguration' {..} =
    object
      ( catMaybes
          [ ("Iops" .=) <$> _vcIOPS,
            ("RaidLevel" .=) <$> _vcRAIdLevel,
            ("Encrypted" .=) <$> _vcEncrypted,
            ("VolumeType" .=) <$> _vcVolumeType,
            Just ("MountPoint" .= _vcMountPoint),
            Just ("NumberOfDisks" .= _vcNumberOfDisks),
            Just ("Size" .= _vcSize)
          ]
      )
