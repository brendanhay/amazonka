{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ProviderDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ProviderDescription where

import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for identity provider details.
--
--
--
-- /See:/ 'providerDescription' smart constructor.
data ProviderDescription = ProviderDescription'
  { _pdLastModifiedDate ::
      !(Maybe POSIX),
    _pdProviderType ::
      !(Maybe IdentityProviderTypeType),
    _pdCreationDate :: !(Maybe POSIX),
    _pdProviderName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProviderDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdLastModifiedDate' - The date the provider was last modified.
--
-- * 'pdProviderType' - The identity provider type.
--
-- * 'pdCreationDate' - The date the provider was added to the user pool.
--
-- * 'pdProviderName' - The identity provider name.
providerDescription ::
  ProviderDescription
providerDescription =
  ProviderDescription'
    { _pdLastModifiedDate = Nothing,
      _pdProviderType = Nothing,
      _pdCreationDate = Nothing,
      _pdProviderName = Nothing
    }

-- | The date the provider was last modified.
pdLastModifiedDate :: Lens' ProviderDescription (Maybe UTCTime)
pdLastModifiedDate = lens _pdLastModifiedDate (\s a -> s {_pdLastModifiedDate = a}) . mapping _Time

-- | The identity provider type.
pdProviderType :: Lens' ProviderDescription (Maybe IdentityProviderTypeType)
pdProviderType = lens _pdProviderType (\s a -> s {_pdProviderType = a})

-- | The date the provider was added to the user pool.
pdCreationDate :: Lens' ProviderDescription (Maybe UTCTime)
pdCreationDate = lens _pdCreationDate (\s a -> s {_pdCreationDate = a}) . mapping _Time

-- | The identity provider name.
pdProviderName :: Lens' ProviderDescription (Maybe Text)
pdProviderName = lens _pdProviderName (\s a -> s {_pdProviderName = a})

instance FromJSON ProviderDescription where
  parseJSON =
    withObject
      "ProviderDescription"
      ( \x ->
          ProviderDescription'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "ProviderType")
            <*> (x .:? "CreationDate")
            <*> (x .:? "ProviderName")
      )

instance Hashable ProviderDescription

instance NFData ProviderDescription
