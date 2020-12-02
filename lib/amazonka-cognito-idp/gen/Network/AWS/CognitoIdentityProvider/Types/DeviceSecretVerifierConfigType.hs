{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The device verifier against which it will be authenticated.
--
--
--
-- /See:/ 'deviceSecretVerifierConfigType' smart constructor.
data DeviceSecretVerifierConfigType = DeviceSecretVerifierConfigType'
  { _dsvctPasswordVerifier ::
      !(Maybe Text),
    _dsvctSalt :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceSecretVerifierConfigType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsvctPasswordVerifier' - The password verifier.
--
-- * 'dsvctSalt' - The salt.
deviceSecretVerifierConfigType ::
  DeviceSecretVerifierConfigType
deviceSecretVerifierConfigType =
  DeviceSecretVerifierConfigType'
    { _dsvctPasswordVerifier = Nothing,
      _dsvctSalt = Nothing
    }

-- | The password verifier.
dsvctPasswordVerifier :: Lens' DeviceSecretVerifierConfigType (Maybe Text)
dsvctPasswordVerifier = lens _dsvctPasswordVerifier (\s a -> s {_dsvctPasswordVerifier = a})

-- | The salt.
dsvctSalt :: Lens' DeviceSecretVerifierConfigType (Maybe Text)
dsvctSalt = lens _dsvctSalt (\s a -> s {_dsvctSalt = a})

instance Hashable DeviceSecretVerifierConfigType

instance NFData DeviceSecretVerifierConfigType

instance ToJSON DeviceSecretVerifierConfigType where
  toJSON DeviceSecretVerifierConfigType' {..} =
    object
      ( catMaybes
          [ ("PasswordVerifier" .=) <$> _dsvctPasswordVerifier,
            ("Salt" .=) <$> _dsvctSalt
          ]
      )
