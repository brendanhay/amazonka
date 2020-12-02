{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ResourceLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ResourceLocation where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.RegionName
import Network.AWS.Prelude

-- | Describes the resource location.
--
--
--
-- /See:/ 'resourceLocation' smart constructor.
data ResourceLocation = ResourceLocation'
  { _rlRegionName ::
      !(Maybe RegionName),
    _rlAvailabilityZone :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlRegionName' - The AWS Region name.
--
-- * 'rlAvailabilityZone' - The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
resourceLocation ::
  ResourceLocation
resourceLocation =
  ResourceLocation'
    { _rlRegionName = Nothing,
      _rlAvailabilityZone = Nothing
    }

-- | The AWS Region name.
rlRegionName :: Lens' ResourceLocation (Maybe RegionName)
rlRegionName = lens _rlRegionName (\s a -> s {_rlRegionName = a})

-- | The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
rlAvailabilityZone :: Lens' ResourceLocation (Maybe Text)
rlAvailabilityZone = lens _rlAvailabilityZone (\s a -> s {_rlAvailabilityZone = a})

instance FromJSON ResourceLocation where
  parseJSON =
    withObject
      "ResourceLocation"
      ( \x ->
          ResourceLocation'
            <$> (x .:? "regionName") <*> (x .:? "availabilityZone")
      )

instance Hashable ResourceLocation

instance NFData ResourceLocation
