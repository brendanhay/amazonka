{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CertificateAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CertificateAuthenticationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the client certificate to be used for authentication.
--
--
--
-- /See:/ 'certificateAuthenticationRequest' smart constructor.
newtype CertificateAuthenticationRequest = CertificateAuthenticationRequest'
  { _carClientRootCertificateChainARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CertificateAuthenticationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carClientRootCertificateChainARN' - The ARN of the client certificate. The certificate must be signed by a certificate authority (CA) and it must be provisioned in AWS Certificate Manager (ACM).
certificateAuthenticationRequest ::
  CertificateAuthenticationRequest
certificateAuthenticationRequest =
  CertificateAuthenticationRequest'
    { _carClientRootCertificateChainARN =
        Nothing
    }

-- | The ARN of the client certificate. The certificate must be signed by a certificate authority (CA) and it must be provisioned in AWS Certificate Manager (ACM).
carClientRootCertificateChainARN :: Lens' CertificateAuthenticationRequest (Maybe Text)
carClientRootCertificateChainARN = lens _carClientRootCertificateChainARN (\s a -> s {_carClientRootCertificateChainARN = a})

instance Hashable CertificateAuthenticationRequest

instance NFData CertificateAuthenticationRequest

instance ToQuery CertificateAuthenticationRequest where
  toQuery CertificateAuthenticationRequest' {..} =
    mconcat
      [ "ClientRootCertificateChainArn"
          =: _carClientRootCertificateChainARN
      ]
