{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FederatedAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FederatedAuthenticationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The IAM SAML identity provider used for federated authentication.
--
--
--
-- /See:/ 'federatedAuthenticationRequest' smart constructor.
data FederatedAuthenticationRequest = FederatedAuthenticationRequest'
  { _farSAMLProviderARN ::
      !(Maybe Text),
    _farSelfServiceSAMLProviderARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FederatedAuthenticationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'farSAMLProviderARN' - The Amazon Resource Name (ARN) of the IAM SAML identity provider.
--
-- * 'farSelfServiceSAMLProviderARN' - The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
federatedAuthenticationRequest ::
  FederatedAuthenticationRequest
federatedAuthenticationRequest =
  FederatedAuthenticationRequest'
    { _farSAMLProviderARN = Nothing,
      _farSelfServiceSAMLProviderARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
farSAMLProviderARN :: Lens' FederatedAuthenticationRequest (Maybe Text)
farSAMLProviderARN = lens _farSAMLProviderARN (\s a -> s {_farSAMLProviderARN = a})

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
farSelfServiceSAMLProviderARN :: Lens' FederatedAuthenticationRequest (Maybe Text)
farSelfServiceSAMLProviderARN = lens _farSelfServiceSAMLProviderARN (\s a -> s {_farSelfServiceSAMLProviderARN = a})

instance Hashable FederatedAuthenticationRequest

instance NFData FederatedAuthenticationRequest

instance ToQuery FederatedAuthenticationRequest where
  toQuery FederatedAuthenticationRequest' {..} =
    mconcat
      [ "SAMLProviderArn" =: _farSAMLProviderARN,
        "SelfServiceSAMLProviderArn" =: _farSelfServiceSAMLProviderARN
      ]
