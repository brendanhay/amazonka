{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for information about an identity provider for a user pool.
--
--
--
-- /See:/ 'providerUserIdentifierType' smart constructor.
data ProviderUserIdentifierType = ProviderUserIdentifierType'
  { _puitProviderAttributeValue ::
      !(Maybe Text),
    _puitProviderAttributeName ::
      !(Maybe Text),
    _puitProviderName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProviderUserIdentifierType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'puitProviderAttributeValue' - The value of the provider attribute to link to, for example, @xxxxx_account@ .
--
-- * 'puitProviderAttributeName' - The name of the provider attribute to link to, for example, @NameID@ .
--
-- * 'puitProviderName' - The name of the provider, for example, Facebook, Google, or Login with Amazon.
providerUserIdentifierType ::
  ProviderUserIdentifierType
providerUserIdentifierType =
  ProviderUserIdentifierType'
    { _puitProviderAttributeValue =
        Nothing,
      _puitProviderAttributeName = Nothing,
      _puitProviderName = Nothing
    }

-- | The value of the provider attribute to link to, for example, @xxxxx_account@ .
puitProviderAttributeValue :: Lens' ProviderUserIdentifierType (Maybe Text)
puitProviderAttributeValue = lens _puitProviderAttributeValue (\s a -> s {_puitProviderAttributeValue = a})

-- | The name of the provider attribute to link to, for example, @NameID@ .
puitProviderAttributeName :: Lens' ProviderUserIdentifierType (Maybe Text)
puitProviderAttributeName = lens _puitProviderAttributeName (\s a -> s {_puitProviderAttributeName = a})

-- | The name of the provider, for example, Facebook, Google, or Login with Amazon.
puitProviderName :: Lens' ProviderUserIdentifierType (Maybe Text)
puitProviderName = lens _puitProviderName (\s a -> s {_puitProviderName = a})

instance Hashable ProviderUserIdentifierType

instance NFData ProviderUserIdentifierType

instance ToJSON ProviderUserIdentifierType where
  toJSON ProviderUserIdentifierType' {..} =
    object
      ( catMaybes
          [ ("ProviderAttributeValue" .=) <$> _puitProviderAttributeValue,
            ("ProviderAttributeName" .=) <$> _puitProviderAttributeName,
            ("ProviderName" .=) <$> _puitProviderName
          ]
      )
