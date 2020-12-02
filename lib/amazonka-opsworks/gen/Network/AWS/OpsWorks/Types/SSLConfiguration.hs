{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.SSLConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.SSLConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an app's SSL configuration.
--
--
--
-- /See:/ 'sslConfiguration' smart constructor.
data SSLConfiguration = SSLConfiguration'
  { _scPrivateKey ::
      !(Maybe Text),
    _scCertificate :: !(Maybe Text),
    _scChain :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSLConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scPrivateKey' - The private key; the contents of the certificate's domain.kex file.
--
-- * 'scCertificate' - The contents of the certificate's domain.crt file.
--
-- * 'scChain' - Optional. Can be used to specify an intermediate certificate authority key or client authentication.
sslConfiguration ::
  SSLConfiguration
sslConfiguration =
  SSLConfiguration'
    { _scPrivateKey = Nothing,
      _scCertificate = Nothing,
      _scChain = Nothing
    }

-- | The private key; the contents of the certificate's domain.kex file.
scPrivateKey :: Lens' SSLConfiguration (Maybe Text)
scPrivateKey = lens _scPrivateKey (\s a -> s {_scPrivateKey = a})

-- | The contents of the certificate's domain.crt file.
scCertificate :: Lens' SSLConfiguration (Maybe Text)
scCertificate = lens _scCertificate (\s a -> s {_scCertificate = a})

-- | Optional. Can be used to specify an intermediate certificate authority key or client authentication.
scChain :: Lens' SSLConfiguration (Maybe Text)
scChain = lens _scChain (\s a -> s {_scChain = a})

instance FromJSON SSLConfiguration where
  parseJSON =
    withObject
      "SSLConfiguration"
      ( \x ->
          SSLConfiguration'
            <$> (x .:? "PrivateKey") <*> (x .:? "Certificate") <*> (x .:? "Chain")
      )

instance Hashable SSLConfiguration

instance NFData SSLConfiguration

instance ToJSON SSLConfiguration where
  toJSON SSLConfiguration' {..} =
    object
      ( catMaybes
          [ ("PrivateKey" .=) <$> _scPrivateKey,
            ("Certificate" .=) <$> _scCertificate,
            ("Chain" .=) <$> _scChain
          ]
      )
