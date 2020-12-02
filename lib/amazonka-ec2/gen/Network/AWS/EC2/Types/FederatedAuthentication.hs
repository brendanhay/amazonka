{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FederatedAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FederatedAuthentication where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the IAM SAML identity providers used for federated authentication.
--
--
--
-- /See:/ 'federatedAuthentication' smart constructor.
data FederatedAuthentication = FederatedAuthentication'
  { _faSamlProviderARN ::
      !(Maybe Text),
    _faSelfServiceSamlProviderARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FederatedAuthentication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faSamlProviderARN' - The Amazon Resource Name (ARN) of the IAM SAML identity provider.
--
-- * 'faSelfServiceSamlProviderARN' - The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
federatedAuthentication ::
  FederatedAuthentication
federatedAuthentication =
  FederatedAuthentication'
    { _faSamlProviderARN = Nothing,
      _faSelfServiceSamlProviderARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
faSamlProviderARN :: Lens' FederatedAuthentication (Maybe Text)
faSamlProviderARN = lens _faSamlProviderARN (\s a -> s {_faSamlProviderARN = a})

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
faSelfServiceSamlProviderARN :: Lens' FederatedAuthentication (Maybe Text)
faSelfServiceSamlProviderARN = lens _faSelfServiceSamlProviderARN (\s a -> s {_faSelfServiceSamlProviderARN = a})

instance FromXML FederatedAuthentication where
  parseXML x =
    FederatedAuthentication'
      <$> (x .@? "samlProviderArn") <*> (x .@? "selfServiceSamlProviderArn")

instance Hashable FederatedAuthentication

instance NFData FederatedAuthentication
