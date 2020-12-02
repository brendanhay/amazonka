{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Certificate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an SSL server certificate.
--
--
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
  { _cCertificateARN :: !(Maybe Text),
    _cIsDefault :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateARN' - The Amazon Resource Name (ARN) of the certificate.
--
-- * 'cIsDefault' - Indicates whether the certificate is the default certificate. Do not set this value when specifying a certificate as an input. This value is not included in the output when describing a listener, but is included when describing listener certificates.
certificate ::
  Certificate
certificate =
  Certificate' {_cCertificateARN = Nothing, _cIsDefault = Nothing}

-- | The Amazon Resource Name (ARN) of the certificate.
cCertificateARN :: Lens' Certificate (Maybe Text)
cCertificateARN = lens _cCertificateARN (\s a -> s {_cCertificateARN = a})

-- | Indicates whether the certificate is the default certificate. Do not set this value when specifying a certificate as an input. This value is not included in the output when describing a listener, but is included when describing listener certificates.
cIsDefault :: Lens' Certificate (Maybe Bool)
cIsDefault = lens _cIsDefault (\s a -> s {_cIsDefault = a})

instance FromXML Certificate where
  parseXML x =
    Certificate' <$> (x .@? "CertificateArn") <*> (x .@? "IsDefault")

instance Hashable Certificate

instance NFData Certificate

instance ToQuery Certificate where
  toQuery Certificate' {..} =
    mconcat
      ["CertificateArn" =: _cCertificateARN, "IsDefault" =: _cIsDefault]
