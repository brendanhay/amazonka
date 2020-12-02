{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DomainEndpointOptions where

import Network.AWS.CloudSearch.Types.TLSSecurityPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The domain's endpoint options.
--
--
--
-- /See:/ 'domainEndpointOptions' smart constructor.
data DomainEndpointOptions = DomainEndpointOptions'
  { _deoEnforceHTTPS ::
      !(Maybe Bool),
    _deoTLSSecurityPolicy ::
      !(Maybe TLSSecurityPolicy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainEndpointOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deoEnforceHTTPS' - Whether the domain is HTTPS only enabled.
--
-- * 'deoTLSSecurityPolicy' - The minimum required TLS version
domainEndpointOptions ::
  DomainEndpointOptions
domainEndpointOptions =
  DomainEndpointOptions'
    { _deoEnforceHTTPS = Nothing,
      _deoTLSSecurityPolicy = Nothing
    }

-- | Whether the domain is HTTPS only enabled.
deoEnforceHTTPS :: Lens' DomainEndpointOptions (Maybe Bool)
deoEnforceHTTPS = lens _deoEnforceHTTPS (\s a -> s {_deoEnforceHTTPS = a})

-- | The minimum required TLS version
deoTLSSecurityPolicy :: Lens' DomainEndpointOptions (Maybe TLSSecurityPolicy)
deoTLSSecurityPolicy = lens _deoTLSSecurityPolicy (\s a -> s {_deoTLSSecurityPolicy = a})

instance FromXML DomainEndpointOptions where
  parseXML x =
    DomainEndpointOptions'
      <$> (x .@? "EnforceHTTPS") <*> (x .@? "TLSSecurityPolicy")

instance Hashable DomainEndpointOptions

instance NFData DomainEndpointOptions

instance ToQuery DomainEndpointOptions where
  toQuery DomainEndpointOptions' {..} =
    mconcat
      [ "EnforceHTTPS" =: _deoEnforceHTTPS,
        "TLSSecurityPolicy" =: _deoTLSSecurityPolicy
      ]
