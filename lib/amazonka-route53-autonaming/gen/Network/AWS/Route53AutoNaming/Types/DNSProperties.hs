{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.DNSProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.DNSProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains the ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
--
--
--
-- /See:/ 'dnsProperties' smart constructor.
newtype DNSProperties = DNSProperties'
  { _dpHostedZoneId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DNSProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpHostedZoneId' - The ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
dnsProperties ::
  DNSProperties
dnsProperties = DNSProperties' {_dpHostedZoneId = Nothing}

-- | The ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
dpHostedZoneId :: Lens' DNSProperties (Maybe Text)
dpHostedZoneId = lens _dpHostedZoneId (\s a -> s {_dpHostedZoneId = a})

instance FromJSON DNSProperties where
  parseJSON =
    withObject
      "DNSProperties"
      (\x -> DNSProperties' <$> (x .:? "HostedZoneId"))

instance Hashable DNSProperties

instance NFData DNSProperties
