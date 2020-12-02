{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNAuthentication where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CertificateAuthentication
import Network.AWS.EC2.Types.ClientVPNAuthenticationType
import Network.AWS.EC2.Types.DirectoryServiceAuthentication
import Network.AWS.EC2.Types.FederatedAuthentication
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the authentication methods used by a Client VPN endpoint. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/client-authentication.html Authentication> in the /AWS Client VPN Administrator Guide/ .
--
--
--
-- /See:/ 'clientVPNAuthentication' smart constructor.
data ClientVPNAuthentication = ClientVPNAuthentication'
  { _cvaActiveDirectory ::
      !(Maybe DirectoryServiceAuthentication),
    _cvaFederatedAuthentication ::
      !(Maybe FederatedAuthentication),
    _cvaMutualAuthentication ::
      !(Maybe CertificateAuthentication),
    _cvaType ::
      !(Maybe ClientVPNAuthenticationType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientVPNAuthentication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvaActiveDirectory' - Information about the Active Directory, if applicable.
--
-- * 'cvaFederatedAuthentication' - Information about the IAM SAML identity provider, if applicable.
--
-- * 'cvaMutualAuthentication' - Information about the authentication certificates, if applicable.
--
-- * 'cvaType' - The authentication type used.
clientVPNAuthentication ::
  ClientVPNAuthentication
clientVPNAuthentication =
  ClientVPNAuthentication'
    { _cvaActiveDirectory = Nothing,
      _cvaFederatedAuthentication = Nothing,
      _cvaMutualAuthentication = Nothing,
      _cvaType = Nothing
    }

-- | Information about the Active Directory, if applicable.
cvaActiveDirectory :: Lens' ClientVPNAuthentication (Maybe DirectoryServiceAuthentication)
cvaActiveDirectory = lens _cvaActiveDirectory (\s a -> s {_cvaActiveDirectory = a})

-- | Information about the IAM SAML identity provider, if applicable.
cvaFederatedAuthentication :: Lens' ClientVPNAuthentication (Maybe FederatedAuthentication)
cvaFederatedAuthentication = lens _cvaFederatedAuthentication (\s a -> s {_cvaFederatedAuthentication = a})

-- | Information about the authentication certificates, if applicable.
cvaMutualAuthentication :: Lens' ClientVPNAuthentication (Maybe CertificateAuthentication)
cvaMutualAuthentication = lens _cvaMutualAuthentication (\s a -> s {_cvaMutualAuthentication = a})

-- | The authentication type used.
cvaType :: Lens' ClientVPNAuthentication (Maybe ClientVPNAuthenticationType)
cvaType = lens _cvaType (\s a -> s {_cvaType = a})

instance FromXML ClientVPNAuthentication where
  parseXML x =
    ClientVPNAuthentication'
      <$> (x .@? "activeDirectory")
      <*> (x .@? "federatedAuthentication")
      <*> (x .@? "mutualAuthentication")
      <*> (x .@? "type")

instance Hashable ClientVPNAuthentication

instance NFData ClientVPNAuthentication
