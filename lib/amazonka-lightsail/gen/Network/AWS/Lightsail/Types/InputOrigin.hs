{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InputOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InputOrigin where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum
import Network.AWS.Lightsail.Types.RegionName
import Network.AWS.Prelude

-- | Describes the origin resource of an Amazon Lightsail content delivery network (CDN) distribution.
--
--
-- An origin can be a Lightsail instance or load balancer. A distribution pulls content from an origin, caches it, and serves it to viewers via a worldwide network of edge servers.
--
--
-- /See:/ 'inputOrigin' smart constructor.
data InputOrigin = InputOrigin'
  { _ioRegionName ::
      !(Maybe RegionName),
    _ioName :: !(Maybe Text),
    _ioProtocolPolicy :: !(Maybe OriginProtocolPolicyEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputOrigin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ioRegionName' - The AWS Region name of the origin resource.
--
-- * 'ioName' - The name of the origin resource.
--
-- * 'ioProtocolPolicy' - The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
inputOrigin ::
  InputOrigin
inputOrigin =
  InputOrigin'
    { _ioRegionName = Nothing,
      _ioName = Nothing,
      _ioProtocolPolicy = Nothing
    }

-- | The AWS Region name of the origin resource.
ioRegionName :: Lens' InputOrigin (Maybe RegionName)
ioRegionName = lens _ioRegionName (\s a -> s {_ioRegionName = a})

-- | The name of the origin resource.
ioName :: Lens' InputOrigin (Maybe Text)
ioName = lens _ioName (\s a -> s {_ioName = a})

-- | The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
ioProtocolPolicy :: Lens' InputOrigin (Maybe OriginProtocolPolicyEnum)
ioProtocolPolicy = lens _ioProtocolPolicy (\s a -> s {_ioProtocolPolicy = a})

instance Hashable InputOrigin

instance NFData InputOrigin

instance ToJSON InputOrigin where
  toJSON InputOrigin' {..} =
    object
      ( catMaybes
          [ ("regionName" .=) <$> _ioRegionName,
            ("name" .=) <$> _ioName,
            ("protocolPolicy" .=) <$> _ioProtocolPolicy
          ]
      )
