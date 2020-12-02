{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrivateDNSNameConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrivateDNSNameConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DNSNameState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the private DNS name for the service endpoint. For more information about these parameters, see <https://docs.aws.amazon.com/vpc/latest/userguide/ndpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
--
-- /See:/ 'privateDNSNameConfiguration' smart constructor.
data PrivateDNSNameConfiguration = PrivateDNSNameConfiguration'
  { _pdncState ::
      !(Maybe DNSNameState),
    _pdncValue :: !(Maybe Text),
    _pdncName :: !(Maybe Text),
    _pdncType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PrivateDNSNameConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdncState' - The verification state of the VPC endpoint service. >Consumers of the endpoint service can use the private name only when the state is @verified@ .
--
-- * 'pdncValue' - The value the service provider adds to the private DNS name domain record before verification.
--
-- * 'pdncName' - The name of the record subdomain the service provider needs to create. The service provider adds the @value@ text to the @name@ .
--
-- * 'pdncType' - The endpoint service verification type, for example TXT.
privateDNSNameConfiguration ::
  PrivateDNSNameConfiguration
privateDNSNameConfiguration =
  PrivateDNSNameConfiguration'
    { _pdncState = Nothing,
      _pdncValue = Nothing,
      _pdncName = Nothing,
      _pdncType = Nothing
    }

-- | The verification state of the VPC endpoint service. >Consumers of the endpoint service can use the private name only when the state is @verified@ .
pdncState :: Lens' PrivateDNSNameConfiguration (Maybe DNSNameState)
pdncState = lens _pdncState (\s a -> s {_pdncState = a})

-- | The value the service provider adds to the private DNS name domain record before verification.
pdncValue :: Lens' PrivateDNSNameConfiguration (Maybe Text)
pdncValue = lens _pdncValue (\s a -> s {_pdncValue = a})

-- | The name of the record subdomain the service provider needs to create. The service provider adds the @value@ text to the @name@ .
pdncName :: Lens' PrivateDNSNameConfiguration (Maybe Text)
pdncName = lens _pdncName (\s a -> s {_pdncName = a})

-- | The endpoint service verification type, for example TXT.
pdncType :: Lens' PrivateDNSNameConfiguration (Maybe Text)
pdncType = lens _pdncType (\s a -> s {_pdncType = a})

instance FromXML PrivateDNSNameConfiguration where
  parseXML x =
    PrivateDNSNameConfiguration'
      <$> (x .@? "state")
      <*> (x .@? "value")
      <*> (x .@? "name")
      <*> (x .@? "type")

instance Hashable PrivateDNSNameConfiguration

instance NFData PrivateDNSNameConfiguration
