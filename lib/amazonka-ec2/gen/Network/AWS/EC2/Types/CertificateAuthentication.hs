{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CertificateAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CertificateAuthentication where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the client certificate used for authentication.
--
--
--
-- /See:/ 'certificateAuthentication' smart constructor.
newtype CertificateAuthentication = CertificateAuthentication'
  { _caClientRootCertificateChain ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CertificateAuthentication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caClientRootCertificateChain' - The ARN of the client certificate.
certificateAuthentication ::
  CertificateAuthentication
certificateAuthentication =
  CertificateAuthentication'
    { _caClientRootCertificateChain =
        Nothing
    }

-- | The ARN of the client certificate.
caClientRootCertificateChain :: Lens' CertificateAuthentication (Maybe Text)
caClientRootCertificateChain = lens _caClientRootCertificateChain (\s a -> s {_caClientRootCertificateChain = a})

instance FromXML CertificateAuthentication where
  parseXML x =
    CertificateAuthentication'
      <$> (x .@? "clientRootCertificateChain")

instance Hashable CertificateAuthentication

instance NFData CertificateAuthentication
