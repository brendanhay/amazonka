{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackVolumeStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackVolumeStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Statistics objects for the various data types in 'AttackVolume' .
--
--
--
-- /See:/ 'attackVolumeStatistics' smart constructor.
newtype AttackVolumeStatistics = AttackVolumeStatistics'
  { _avsMax ::
      Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttackVolumeStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avsMax' - The maximum attack volume observed for the given unit.
attackVolumeStatistics ::
  -- | 'avsMax'
  Double ->
  AttackVolumeStatistics
attackVolumeStatistics pMax_ =
  AttackVolumeStatistics' {_avsMax = pMax_}

-- | The maximum attack volume observed for the given unit.
avsMax :: Lens' AttackVolumeStatistics Double
avsMax = lens _avsMax (\s a -> s {_avsMax = a})

instance FromJSON AttackVolumeStatistics where
  parseJSON =
    withObject
      "AttackVolumeStatistics"
      (\x -> AttackVolumeStatistics' <$> (x .: "Max"))

instance Hashable AttackVolumeStatistics

instance NFData AttackVolumeStatistics
