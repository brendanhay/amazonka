{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceChange where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.DNSConfigChange
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig

-- | A complex type that contains changes to an existing service.
--
--
--
-- /See:/ 'serviceChange' smart constructor.
data ServiceChange = ServiceChange'
  { _scHealthCheckConfig ::
      !(Maybe HealthCheckConfig),
    _scDNSConfig :: !(Maybe DNSConfigChange),
    _scDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scHealthCheckConfig' - Undocumented member.
--
-- * 'scDNSConfig' - A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
--
-- * 'scDescription' - A description for the service.
serviceChange ::
  ServiceChange
serviceChange =
  ServiceChange'
    { _scHealthCheckConfig = Nothing,
      _scDNSConfig = Nothing,
      _scDescription = Nothing
    }

-- | Undocumented member.
scHealthCheckConfig :: Lens' ServiceChange (Maybe HealthCheckConfig)
scHealthCheckConfig = lens _scHealthCheckConfig (\s a -> s {_scHealthCheckConfig = a})

-- | A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
scDNSConfig :: Lens' ServiceChange (Maybe DNSConfigChange)
scDNSConfig = lens _scDNSConfig (\s a -> s {_scDNSConfig = a})

-- | A description for the service.
scDescription :: Lens' ServiceChange (Maybe Text)
scDescription = lens _scDescription (\s a -> s {_scDescription = a})

instance Hashable ServiceChange

instance NFData ServiceChange

instance ToJSON ServiceChange where
  toJSON ServiceChange' {..} =
    object
      ( catMaybes
          [ ("HealthCheckConfig" .=) <$> _scHealthCheckConfig,
            ("DnsConfig" .=) <$> _scDNSConfig,
            ("Description" .=) <$> _scDescription
          ]
      )
