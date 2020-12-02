{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RegionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RegionInfo where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Region.
--
--
--
-- /See:/ 'regionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { _riRegionName :: !(Maybe Text),
    _riOptInStatus :: !(Maybe Text),
    _riEndpoint :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riRegionName' - The name of the Region.
--
-- * 'riOptInStatus' - The Region opt-in status. The possible values are @opt-in-not-required@ , @opted-in@ , and @not-opted-in@ .
--
-- * 'riEndpoint' - The Region service endpoint.
regionInfo ::
  RegionInfo
regionInfo =
  RegionInfo'
    { _riRegionName = Nothing,
      _riOptInStatus = Nothing,
      _riEndpoint = Nothing
    }

-- | The name of the Region.
riRegionName :: Lens' RegionInfo (Maybe Text)
riRegionName = lens _riRegionName (\s a -> s {_riRegionName = a})

-- | The Region opt-in status. The possible values are @opt-in-not-required@ , @opted-in@ , and @not-opted-in@ .
riOptInStatus :: Lens' RegionInfo (Maybe Text)
riOptInStatus = lens _riOptInStatus (\s a -> s {_riOptInStatus = a})

-- | The Region service endpoint.
riEndpoint :: Lens' RegionInfo (Maybe Text)
riEndpoint = lens _riEndpoint (\s a -> s {_riEndpoint = a})

instance FromXML RegionInfo where
  parseXML x =
    RegionInfo'
      <$> (x .@? "regionName")
      <*> (x .@? "optInStatus")
      <*> (x .@? "regionEndpoint")

instance Hashable RegionInfo

instance NFData RegionInfo
