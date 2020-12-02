{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Origin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Origin where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum
import Network.AWS.Lightsail.Types.RegionName
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Prelude

-- | Describes the origin resource of an Amazon Lightsail content delivery network (CDN) distribution.
--
--
-- An origin can be a Lightsail instance or load balancer. A distribution pulls content from an origin, caches it, and serves it to viewers via a worldwide network of edge servers.
--
--
-- /See:/ 'origin' smart constructor.
data Origin = Origin'
  { _oRegionName :: !(Maybe RegionName),
    _oResourceType :: !(Maybe ResourceType),
    _oName :: !(Maybe Text),
    _oProtocolPolicy :: !(Maybe OriginProtocolPolicyEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Origin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oRegionName' - The AWS Region name of the origin resource.
--
-- * 'oResourceType' - The resource type of the origin resource (e.g., /Instance/ ).
--
-- * 'oName' - The name of the origin resource.
--
-- * 'oProtocolPolicy' - The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
origin ::
  Origin
origin =
  Origin'
    { _oRegionName = Nothing,
      _oResourceType = Nothing,
      _oName = Nothing,
      _oProtocolPolicy = Nothing
    }

-- | The AWS Region name of the origin resource.
oRegionName :: Lens' Origin (Maybe RegionName)
oRegionName = lens _oRegionName (\s a -> s {_oRegionName = a})

-- | The resource type of the origin resource (e.g., /Instance/ ).
oResourceType :: Lens' Origin (Maybe ResourceType)
oResourceType = lens _oResourceType (\s a -> s {_oResourceType = a})

-- | The name of the origin resource.
oName :: Lens' Origin (Maybe Text)
oName = lens _oName (\s a -> s {_oName = a})

-- | The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
oProtocolPolicy :: Lens' Origin (Maybe OriginProtocolPolicyEnum)
oProtocolPolicy = lens _oProtocolPolicy (\s a -> s {_oProtocolPolicy = a})

instance FromJSON Origin where
  parseJSON =
    withObject
      "Origin"
      ( \x ->
          Origin'
            <$> (x .:? "regionName")
            <*> (x .:? "resourceType")
            <*> (x .:? "name")
            <*> (x .:? "protocolPolicy")
      )

instance Hashable Origin

instance NFData Origin
