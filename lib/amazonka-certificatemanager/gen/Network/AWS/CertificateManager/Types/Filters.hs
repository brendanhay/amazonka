{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.Filters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.Filters where

import Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
import Network.AWS.CertificateManager.Types.KeyAlgorithm
import Network.AWS.CertificateManager.Types.KeyUsageName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This structure can be used in the 'ListCertificates' action to filter the output of the certificate list.
--
--
--
-- /See:/ 'filters' smart constructor.
data Filters = Filters'
  { _fKeyTypes :: !(Maybe [KeyAlgorithm]),
    _fKeyUsage :: !(Maybe [KeyUsageName]),
    _fExtendedKeyUsage :: !(Maybe [ExtendedKeyUsageName])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fKeyTypes' - Specify one or more algorithms that can be used to generate key pairs. Default filtering returns only @RSA_2048@ certificates. To return other certificate types, provide the desired type signatures in a comma-separated list. For example, @"keyTypes": ["RSA_2048,RSA_4096"]@ returns both @RSA_2048@ and @RSA_4096@ certificates.
--
-- * 'fKeyUsage' - Specify one or more 'KeyUsage' extension values.
--
-- * 'fExtendedKeyUsage' - Specify one or more 'ExtendedKeyUsage' extension values.
filters ::
  Filters
filters =
  Filters'
    { _fKeyTypes = Nothing,
      _fKeyUsage = Nothing,
      _fExtendedKeyUsage = Nothing
    }

-- | Specify one or more algorithms that can be used to generate key pairs. Default filtering returns only @RSA_2048@ certificates. To return other certificate types, provide the desired type signatures in a comma-separated list. For example, @"keyTypes": ["RSA_2048,RSA_4096"]@ returns both @RSA_2048@ and @RSA_4096@ certificates.
fKeyTypes :: Lens' Filters [KeyAlgorithm]
fKeyTypes = lens _fKeyTypes (\s a -> s {_fKeyTypes = a}) . _Default . _Coerce

-- | Specify one or more 'KeyUsage' extension values.
fKeyUsage :: Lens' Filters [KeyUsageName]
fKeyUsage = lens _fKeyUsage (\s a -> s {_fKeyUsage = a}) . _Default . _Coerce

-- | Specify one or more 'ExtendedKeyUsage' extension values.
fExtendedKeyUsage :: Lens' Filters [ExtendedKeyUsageName]
fExtendedKeyUsage = lens _fExtendedKeyUsage (\s a -> s {_fExtendedKeyUsage = a}) . _Default . _Coerce

instance Hashable Filters

instance NFData Filters

instance ToJSON Filters where
  toJSON Filters' {..} =
    object
      ( catMaybes
          [ ("keyTypes" .=) <$> _fKeyTypes,
            ("keyUsage" .=) <$> _fKeyUsage,
            ("extendedKeyUsage" .=) <$> _fExtendedKeyUsage
          ]
      )
