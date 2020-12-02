{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrivateIPAddressSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrivateIPAddressSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a secondary private IPv4 address for a network interface.
--
--
--
-- /See:/ 'privateIPAddressSpecification' smart constructor.
data PrivateIPAddressSpecification = PrivateIPAddressSpecification'
  { _piasPrimary ::
      !(Maybe Bool),
    _piasPrivateIPAddress ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PrivateIPAddressSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piasPrimary' - Indicates whether the private IPv4 address is the primary private IPv4 address. Only one IPv4 address can be designated as primary.
--
-- * 'piasPrivateIPAddress' - The private IPv4 addresses.
privateIPAddressSpecification ::
  PrivateIPAddressSpecification
privateIPAddressSpecification =
  PrivateIPAddressSpecification'
    { _piasPrimary = Nothing,
      _piasPrivateIPAddress = Nothing
    }

-- | Indicates whether the private IPv4 address is the primary private IPv4 address. Only one IPv4 address can be designated as primary.
piasPrimary :: Lens' PrivateIPAddressSpecification (Maybe Bool)
piasPrimary = lens _piasPrimary (\s a -> s {_piasPrimary = a})

-- | The private IPv4 addresses.
piasPrivateIPAddress :: Lens' PrivateIPAddressSpecification (Maybe Text)
piasPrivateIPAddress = lens _piasPrivateIPAddress (\s a -> s {_piasPrivateIPAddress = a})

instance FromXML PrivateIPAddressSpecification where
  parseXML x =
    PrivateIPAddressSpecification'
      <$> (x .@? "primary") <*> (x .@? "privateIpAddress")

instance Hashable PrivateIPAddressSpecification

instance NFData PrivateIPAddressSpecification

instance ToQuery PrivateIPAddressSpecification where
  toQuery PrivateIPAddressSpecification' {..} =
    mconcat
      [ "Primary" =: _piasPrimary,
        "PrivateIpAddress" =: _piasPrivateIPAddress
      ]
