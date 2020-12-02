{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackVolume where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.AttackVolumeStatistics

-- | Information about the volume of attacks during the time period, included in an 'AttackStatisticsDataItem' . If the accompanying @AttackCount@ in the statistics object is zero, this setting might be empty.
--
--
--
-- /See:/ 'attackVolume' smart constructor.
data AttackVolume = AttackVolume'
  { _avPacketsPerSecond ::
      !(Maybe AttackVolumeStatistics),
    _avRequestsPerSecond :: !(Maybe AttackVolumeStatistics),
    _avBitsPerSecond :: !(Maybe AttackVolumeStatistics)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttackVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avPacketsPerSecond' - A statistics object that uses packets per second as the unit. This is included for network level attacks.
--
-- * 'avRequestsPerSecond' - A statistics object that uses requests per second as the unit. This is included for application level attacks, and is only available for accounts that are subscribed to Shield Advanced.
--
-- * 'avBitsPerSecond' - A statistics object that uses bits per second as the unit. This is included for network level attacks.
attackVolume ::
  AttackVolume
attackVolume =
  AttackVolume'
    { _avPacketsPerSecond = Nothing,
      _avRequestsPerSecond = Nothing,
      _avBitsPerSecond = Nothing
    }

-- | A statistics object that uses packets per second as the unit. This is included for network level attacks.
avPacketsPerSecond :: Lens' AttackVolume (Maybe AttackVolumeStatistics)
avPacketsPerSecond = lens _avPacketsPerSecond (\s a -> s {_avPacketsPerSecond = a})

-- | A statistics object that uses requests per second as the unit. This is included for application level attacks, and is only available for accounts that are subscribed to Shield Advanced.
avRequestsPerSecond :: Lens' AttackVolume (Maybe AttackVolumeStatistics)
avRequestsPerSecond = lens _avRequestsPerSecond (\s a -> s {_avRequestsPerSecond = a})

-- | A statistics object that uses bits per second as the unit. This is included for network level attacks.
avBitsPerSecond :: Lens' AttackVolume (Maybe AttackVolumeStatistics)
avBitsPerSecond = lens _avBitsPerSecond (\s a -> s {_avBitsPerSecond = a})

instance FromJSON AttackVolume where
  parseJSON =
    withObject
      "AttackVolume"
      ( \x ->
          AttackVolume'
            <$> (x .:? "PacketsPerSecond")
            <*> (x .:? "RequestsPerSecond")
            <*> (x .:? "BitsPerSecond")
      )

instance Hashable AttackVolume

instance NFData AttackVolume
