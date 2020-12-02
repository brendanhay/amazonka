{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.HSMClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.HSMClientCertificate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Returns information about an HSM client certificate. The certificate is stored in a secure Hardware Storage Module (HSM), and used by the Amazon Redshift cluster to encrypt data files.
--
--
--
-- /See:/ 'hsmClientCertificate' smart constructor.
data HSMClientCertificate = HSMClientCertificate'
  { _hccHSMClientCertificateIdentifier ::
      !(Maybe Text),
    _hccHSMClientCertificatePublicKey ::
      !(Maybe Text),
    _hccTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HSMClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hccHSMClientCertificateIdentifier' - The identifier of the HSM client certificate.
--
-- * 'hccHSMClientCertificatePublicKey' - The public key that the Amazon Redshift cluster will use to connect to the HSM. You must register the public key in the HSM.
--
-- * 'hccTags' - The list of tags for the HSM client certificate.
hsmClientCertificate ::
  HSMClientCertificate
hsmClientCertificate =
  HSMClientCertificate'
    { _hccHSMClientCertificateIdentifier =
        Nothing,
      _hccHSMClientCertificatePublicKey = Nothing,
      _hccTags = Nothing
    }

-- | The identifier of the HSM client certificate.
hccHSMClientCertificateIdentifier :: Lens' HSMClientCertificate (Maybe Text)
hccHSMClientCertificateIdentifier = lens _hccHSMClientCertificateIdentifier (\s a -> s {_hccHSMClientCertificateIdentifier = a})

-- | The public key that the Amazon Redshift cluster will use to connect to the HSM. You must register the public key in the HSM.
hccHSMClientCertificatePublicKey :: Lens' HSMClientCertificate (Maybe Text)
hccHSMClientCertificatePublicKey = lens _hccHSMClientCertificatePublicKey (\s a -> s {_hccHSMClientCertificatePublicKey = a})

-- | The list of tags for the HSM client certificate.
hccTags :: Lens' HSMClientCertificate [Tag]
hccTags = lens _hccTags (\s a -> s {_hccTags = a}) . _Default . _Coerce

instance FromXML HSMClientCertificate where
  parseXML x =
    HSMClientCertificate'
      <$> (x .@? "HsmClientCertificateIdentifier")
      <*> (x .@? "HsmClientCertificatePublicKey")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable HSMClientCertificate

instance NFData HSMClientCertificate
