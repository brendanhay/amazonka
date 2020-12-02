{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PasswordData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PasswordData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The password data for the Windows Server-based instance, including the ciphertext and the key pair name.
--
--
--
-- /See:/ 'passwordData' smart constructor.
data PasswordData = PasswordData'
  { _pdKeyPairName :: !(Maybe Text),
    _pdCiphertext :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PasswordData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdKeyPairName' - The name of the key pair that you used when creating your instance. If no key pair name was specified when creating the instance, Lightsail uses the default key pair (@LightsailDefaultKeyPair@ ). If you are using a custom key pair, you need to use your own means of decrypting your password using the @ciphertext@ . Lightsail creates the ciphertext by encrypting your password with the public key part of this key pair.
--
-- * 'pdCiphertext' - The encrypted password. Ciphertext will be an empty string if access to your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
passwordData ::
  PasswordData
passwordData =
  PasswordData' {_pdKeyPairName = Nothing, _pdCiphertext = Nothing}

-- | The name of the key pair that you used when creating your instance. If no key pair name was specified when creating the instance, Lightsail uses the default key pair (@LightsailDefaultKeyPair@ ). If you are using a custom key pair, you need to use your own means of decrypting your password using the @ciphertext@ . Lightsail creates the ciphertext by encrypting your password with the public key part of this key pair.
pdKeyPairName :: Lens' PasswordData (Maybe Text)
pdKeyPairName = lens _pdKeyPairName (\s a -> s {_pdKeyPairName = a})

-- | The encrypted password. Ciphertext will be an empty string if access to your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
pdCiphertext :: Lens' PasswordData (Maybe Text)
pdCiphertext = lens _pdCiphertext (\s a -> s {_pdCiphertext = a})

instance FromJSON PasswordData where
  parseJSON =
    withObject
      "PasswordData"
      ( \x ->
          PasswordData' <$> (x .:? "keyPairName") <*> (x .:? "ciphertext")
      )

instance Hashable PasswordData

instance NFData PasswordData
