{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackStatisticsDataItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackStatisticsDataItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.AttackVolume

-- | A single attack statistics data record. This is returned by 'DescribeAttackStatistics' along with a time range indicating the time period that the attack statistics apply to.
--
--
--
-- /See:/ 'attackStatisticsDataItem' smart constructor.
data AttackStatisticsDataItem = AttackStatisticsDataItem'
  { _asdiAttackVolume ::
      !(Maybe AttackVolume),
    _asdiAttackCount :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttackStatisticsDataItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asdiAttackVolume' - Information about the volume of attacks during the time period. If the accompanying @AttackCount@ is zero, this setting might be empty.
--
-- * 'asdiAttackCount' - The number of attacks detected during the time period. This is always present, but might be zero.
attackStatisticsDataItem ::
  -- | 'asdiAttackCount'
  Integer ->
  AttackStatisticsDataItem
attackStatisticsDataItem pAttackCount_ =
  AttackStatisticsDataItem'
    { _asdiAttackVolume = Nothing,
      _asdiAttackCount = pAttackCount_
    }

-- | Information about the volume of attacks during the time period. If the accompanying @AttackCount@ is zero, this setting might be empty.
asdiAttackVolume :: Lens' AttackStatisticsDataItem (Maybe AttackVolume)
asdiAttackVolume = lens _asdiAttackVolume (\s a -> s {_asdiAttackVolume = a})

-- | The number of attacks detected during the time period. This is always present, but might be zero.
asdiAttackCount :: Lens' AttackStatisticsDataItem Integer
asdiAttackCount = lens _asdiAttackCount (\s a -> s {_asdiAttackCount = a})

instance FromJSON AttackStatisticsDataItem where
  parseJSON =
    withObject
      "AttackStatisticsDataItem"
      ( \x ->
          AttackStatisticsDataItem'
            <$> (x .:? "AttackVolume") <*> (x .: "AttackCount")
      )

instance Hashable AttackStatisticsDataItem

instance NFData AttackStatisticsDataItem
