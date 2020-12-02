{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the zone awareness configuration for the domain cluster, such as the number of availability zones.
--
--
--
-- /See:/ 'zoneAwarenessConfig' smart constructor.
newtype ZoneAwarenessConfig = ZoneAwarenessConfig'
  { _zacAvailabilityZoneCount ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ZoneAwarenessConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'zacAvailabilityZoneCount' - An integer value to indicate the number of availability zones for a domain when zone awareness is enabled. This should be equal to number of subnets if VPC endpoints is enabled
zoneAwarenessConfig ::
  ZoneAwarenessConfig
zoneAwarenessConfig =
  ZoneAwarenessConfig' {_zacAvailabilityZoneCount = Nothing}

-- | An integer value to indicate the number of availability zones for a domain when zone awareness is enabled. This should be equal to number of subnets if VPC endpoints is enabled
zacAvailabilityZoneCount :: Lens' ZoneAwarenessConfig (Maybe Int)
zacAvailabilityZoneCount = lens _zacAvailabilityZoneCount (\s a -> s {_zacAvailabilityZoneCount = a})

instance FromJSON ZoneAwarenessConfig where
  parseJSON =
    withObject
      "ZoneAwarenessConfig"
      (\x -> ZoneAwarenessConfig' <$> (x .:? "AvailabilityZoneCount"))

instance Hashable ZoneAwarenessConfig

instance NFData ZoneAwarenessConfig

instance ToJSON ZoneAwarenessConfig where
  toJSON ZoneAwarenessConfig' {..} =
    object
      ( catMaybes
          [("AvailabilityZoneCount" .=) <$> _zacAvailabilityZoneCount]
      )
