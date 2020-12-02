{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RegionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RegionInfo where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.AvailabilityZone
import Network.AWS.Lightsail.Types.RegionName
import Network.AWS.Prelude

-- | Describes the AWS Region.
--
--
--
-- /See:/ 'regionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { _riAvailabilityZones ::
      !(Maybe [AvailabilityZone]),
    _riName :: !(Maybe RegionName),
    _riRelationalDatabaseAvailabilityZones ::
      !(Maybe [AvailabilityZone]),
    _riDisplayName :: !(Maybe Text),
    _riContinentCode :: !(Maybe Text),
    _riDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riAvailabilityZones' - The Availability Zones. Follows the format @us-east-2a@ (case-sensitive).
--
-- * 'riName' - The region name (e.g., @us-east-2@ ).
--
-- * 'riRelationalDatabaseAvailabilityZones' - The Availability Zones for databases. Follows the format @us-east-2a@ (case-sensitive).
--
-- * 'riDisplayName' - The display name (e.g., @Ohio@ ).
--
-- * 'riContinentCode' - The continent code (e.g., @NA@ , meaning North America).
--
-- * 'riDescription' - The description of the AWS Region (e.g., @This region is recommended to serve users in the eastern United States and eastern Canada@ ).
regionInfo ::
  RegionInfo
regionInfo =
  RegionInfo'
    { _riAvailabilityZones = Nothing,
      _riName = Nothing,
      _riRelationalDatabaseAvailabilityZones = Nothing,
      _riDisplayName = Nothing,
      _riContinentCode = Nothing,
      _riDescription = Nothing
    }

-- | The Availability Zones. Follows the format @us-east-2a@ (case-sensitive).
riAvailabilityZones :: Lens' RegionInfo [AvailabilityZone]
riAvailabilityZones = lens _riAvailabilityZones (\s a -> s {_riAvailabilityZones = a}) . _Default . _Coerce

-- | The region name (e.g., @us-east-2@ ).
riName :: Lens' RegionInfo (Maybe RegionName)
riName = lens _riName (\s a -> s {_riName = a})

-- | The Availability Zones for databases. Follows the format @us-east-2a@ (case-sensitive).
riRelationalDatabaseAvailabilityZones :: Lens' RegionInfo [AvailabilityZone]
riRelationalDatabaseAvailabilityZones = lens _riRelationalDatabaseAvailabilityZones (\s a -> s {_riRelationalDatabaseAvailabilityZones = a}) . _Default . _Coerce

-- | The display name (e.g., @Ohio@ ).
riDisplayName :: Lens' RegionInfo (Maybe Text)
riDisplayName = lens _riDisplayName (\s a -> s {_riDisplayName = a})

-- | The continent code (e.g., @NA@ , meaning North America).
riContinentCode :: Lens' RegionInfo (Maybe Text)
riContinentCode = lens _riContinentCode (\s a -> s {_riContinentCode = a})

-- | The description of the AWS Region (e.g., @This region is recommended to serve users in the eastern United States and eastern Canada@ ).
riDescription :: Lens' RegionInfo (Maybe Text)
riDescription = lens _riDescription (\s a -> s {_riDescription = a})

instance FromJSON RegionInfo where
  parseJSON =
    withObject
      "RegionInfo"
      ( \x ->
          RegionInfo'
            <$> (x .:? "availabilityZones" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "relationalDatabaseAvailabilityZones" .!= mempty)
            <*> (x .:? "displayName")
            <*> (x .:? "continentCode")
            <*> (x .:? "description")
      )

instance Hashable RegionInfo

instance NFData RegionInfo
