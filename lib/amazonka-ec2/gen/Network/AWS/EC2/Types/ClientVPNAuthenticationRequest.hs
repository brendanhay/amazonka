{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNAuthenticationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CertificateAuthenticationRequest
import Network.AWS.EC2.Types.ClientVPNAuthenticationType
import Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest
import Network.AWS.EC2.Types.FederatedAuthenticationRequest
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the authentication method to be used by a Client VPN endpoint. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/authentication-authrization.html#client-authentication Authentication> in the /AWS Client VPN Administrator Guide/ .
--
--
--
-- /See:/ 'clientVPNAuthenticationRequest' smart constructor.
data ClientVPNAuthenticationRequest = ClientVPNAuthenticationRequest'
  { _cvarActiveDirectory ::
      !( Maybe
           DirectoryServiceAuthenticationRequest
       ),
    _cvarFederatedAuthentication ::
      !( Maybe
           FederatedAuthenticationRequest
       ),
    _cvarMutualAuthentication ::
      !( Maybe
           CertificateAuthenticationRequest
       ),
    _cvarType ::
      !( Maybe
           ClientVPNAuthenticationType
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientVPNAuthenticationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvarActiveDirectory' - Information about the Active Directory to be used, if applicable. You must provide this information if __Type__ is @directory-service-authentication@ .
--
-- * 'cvarFederatedAuthentication' - Information about the IAM SAML identity provider to be used, if applicable. You must provide this information if __Type__ is @federated-authentication@ .
--
-- * 'cvarMutualAuthentication' - Information about the authentication certificates to be used, if applicable. You must provide this information if __Type__ is @certificate-authentication@ .
--
-- * 'cvarType' - The type of client authentication to be used.
clientVPNAuthenticationRequest ::
  ClientVPNAuthenticationRequest
clientVPNAuthenticationRequest =
  ClientVPNAuthenticationRequest'
    { _cvarActiveDirectory = Nothing,
      _cvarFederatedAuthentication = Nothing,
      _cvarMutualAuthentication = Nothing,
      _cvarType = Nothing
    }

-- | Information about the Active Directory to be used, if applicable. You must provide this information if __Type__ is @directory-service-authentication@ .
cvarActiveDirectory :: Lens' ClientVPNAuthenticationRequest (Maybe DirectoryServiceAuthenticationRequest)
cvarActiveDirectory = lens _cvarActiveDirectory (\s a -> s {_cvarActiveDirectory = a})

-- | Information about the IAM SAML identity provider to be used, if applicable. You must provide this information if __Type__ is @federated-authentication@ .
cvarFederatedAuthentication :: Lens' ClientVPNAuthenticationRequest (Maybe FederatedAuthenticationRequest)
cvarFederatedAuthentication = lens _cvarFederatedAuthentication (\s a -> s {_cvarFederatedAuthentication = a})

-- | Information about the authentication certificates to be used, if applicable. You must provide this information if __Type__ is @certificate-authentication@ .
cvarMutualAuthentication :: Lens' ClientVPNAuthenticationRequest (Maybe CertificateAuthenticationRequest)
cvarMutualAuthentication = lens _cvarMutualAuthentication (\s a -> s {_cvarMutualAuthentication = a})

-- | The type of client authentication to be used.
cvarType :: Lens' ClientVPNAuthenticationRequest (Maybe ClientVPNAuthenticationType)
cvarType = lens _cvarType (\s a -> s {_cvarType = a})

instance Hashable ClientVPNAuthenticationRequest

instance NFData ClientVPNAuthenticationRequest

instance ToQuery ClientVPNAuthenticationRequest where
  toQuery ClientVPNAuthenticationRequest' {..} =
    mconcat
      [ "ActiveDirectory" =: _cvarActiveDirectory,
        "FederatedAuthentication" =: _cvarFederatedAuthentication,
        "MutualAuthentication" =: _cvarMutualAuthentication,
        "Type" =: _cvarType
      ]
