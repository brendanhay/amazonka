{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.VolumeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.VolumeSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
--
--
--
-- /See:/ 'volumeSpecification' smart constructor.
data VolumeSpecification = VolumeSpecification'
  { _vsIOPS ::
      !(Maybe Int),
    _vsVolumeType :: !Text,
    _vsSizeInGB :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsIOPS' - The number of I/O operations per second (IOPS) that the volume supports.
--
-- * 'vsVolumeType' - The volume type. Volume types supported are gp2, io1, standard.
--
-- * 'vsSizeInGB' - The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
volumeSpecification ::
  -- | 'vsVolumeType'
  Text ->
  -- | 'vsSizeInGB'
  Int ->
  VolumeSpecification
volumeSpecification pVolumeType_ pSizeInGB_ =
  VolumeSpecification'
    { _vsIOPS = Nothing,
      _vsVolumeType = pVolumeType_,
      _vsSizeInGB = pSizeInGB_
    }

-- | The number of I/O operations per second (IOPS) that the volume supports.
vsIOPS :: Lens' VolumeSpecification (Maybe Int)
vsIOPS = lens _vsIOPS (\s a -> s {_vsIOPS = a})

-- | The volume type. Volume types supported are gp2, io1, standard.
vsVolumeType :: Lens' VolumeSpecification Text
vsVolumeType = lens _vsVolumeType (\s a -> s {_vsVolumeType = a})

-- | The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
vsSizeInGB :: Lens' VolumeSpecification Int
vsSizeInGB = lens _vsSizeInGB (\s a -> s {_vsSizeInGB = a})

instance FromJSON VolumeSpecification where
  parseJSON =
    withObject
      "VolumeSpecification"
      ( \x ->
          VolumeSpecification'
            <$> (x .:? "Iops") <*> (x .: "VolumeType") <*> (x .: "SizeInGB")
      )

instance Hashable VolumeSpecification

instance NFData VolumeSpecification

instance ToJSON VolumeSpecification where
  toJSON VolumeSpecification' {..} =
    object
      ( catMaybes
          [ ("Iops" .=) <$> _vsIOPS,
            Just ("VolumeType" .= _vsVolumeType),
            Just ("SizeInGB" .= _vsSizeInGB)
          ]
      )
