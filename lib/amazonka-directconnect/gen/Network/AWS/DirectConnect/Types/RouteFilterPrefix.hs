{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.RouteFilterPrefix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.RouteFilterPrefix where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a route filter prefix that a customer can advertise through Border Gateway Protocol (BGP) over a public virtual interface.
--
--
--
-- /See:/ 'routeFilterPrefix' smart constructor.
newtype RouteFilterPrefix = RouteFilterPrefix'
  { _rfpCidr ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RouteFilterPrefix' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfpCidr' - The CIDR block for the advertised route. Separate multiple routes using commas. An IPv6 CIDR must use /64 or shorter.
routeFilterPrefix ::
  RouteFilterPrefix
routeFilterPrefix = RouteFilterPrefix' {_rfpCidr = Nothing}

-- | The CIDR block for the advertised route. Separate multiple routes using commas. An IPv6 CIDR must use /64 or shorter.
rfpCidr :: Lens' RouteFilterPrefix (Maybe Text)
rfpCidr = lens _rfpCidr (\s a -> s {_rfpCidr = a})

instance FromJSON RouteFilterPrefix where
  parseJSON =
    withObject
      "RouteFilterPrefix"
      (\x -> RouteFilterPrefix' <$> (x .:? "cidr"))

instance Hashable RouteFilterPrefix

instance NFData RouteFilterPrefix

instance ToJSON RouteFilterPrefix where
  toJSON RouteFilterPrefix' {..} =
    object (catMaybes [("cidr" .=) <$> _rfpCidr])
