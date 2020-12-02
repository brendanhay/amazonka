{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ResourceServerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ResourceServerType where

import Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for information about a resource server for a user pool.
--
--
--
-- /See:/ 'resourceServerType' smart constructor.
data ResourceServerType = ResourceServerType'
  { _rstUserPoolId ::
      !(Maybe Text),
    _rstIdentifier :: !(Maybe Text),
    _rstScopes :: !(Maybe [ResourceServerScopeType]),
    _rstName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceServerType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rstUserPoolId' - The user pool ID for the user pool that hosts the resource server.
--
-- * 'rstIdentifier' - The identifier for the resource server.
--
-- * 'rstScopes' - A list of scopes that are defined for the resource server.
--
-- * 'rstName' - The name of the resource server.
resourceServerType ::
  ResourceServerType
resourceServerType =
  ResourceServerType'
    { _rstUserPoolId = Nothing,
      _rstIdentifier = Nothing,
      _rstScopes = Nothing,
      _rstName = Nothing
    }

-- | The user pool ID for the user pool that hosts the resource server.
rstUserPoolId :: Lens' ResourceServerType (Maybe Text)
rstUserPoolId = lens _rstUserPoolId (\s a -> s {_rstUserPoolId = a})

-- | The identifier for the resource server.
rstIdentifier :: Lens' ResourceServerType (Maybe Text)
rstIdentifier = lens _rstIdentifier (\s a -> s {_rstIdentifier = a})

-- | A list of scopes that are defined for the resource server.
rstScopes :: Lens' ResourceServerType [ResourceServerScopeType]
rstScopes = lens _rstScopes (\s a -> s {_rstScopes = a}) . _Default . _Coerce

-- | The name of the resource server.
rstName :: Lens' ResourceServerType (Maybe Text)
rstName = lens _rstName (\s a -> s {_rstName = a})

instance FromJSON ResourceServerType where
  parseJSON =
    withObject
      "ResourceServerType"
      ( \x ->
          ResourceServerType'
            <$> (x .:? "UserPoolId")
            <*> (x .:? "Identifier")
            <*> (x .:? "Scopes" .!= mempty)
            <*> (x .:? "Name")
      )

instance Hashable ResourceServerType

instance NFData ResourceServerType
