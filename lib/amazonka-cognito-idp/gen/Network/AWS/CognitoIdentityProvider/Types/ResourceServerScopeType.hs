{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A resource server scope.
--
--
--
-- /See:/ 'resourceServerScopeType' smart constructor.
data ResourceServerScopeType = ResourceServerScopeType'
  { _rsstScopeName ::
      !Text,
    _rsstScopeDescription :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceServerScopeType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsstScopeName' - The name of the scope.
--
-- * 'rsstScopeDescription' - A description of the scope.
resourceServerScopeType ::
  -- | 'rsstScopeName'
  Text ->
  -- | 'rsstScopeDescription'
  Text ->
  ResourceServerScopeType
resourceServerScopeType pScopeName_ pScopeDescription_ =
  ResourceServerScopeType'
    { _rsstScopeName = pScopeName_,
      _rsstScopeDescription = pScopeDescription_
    }

-- | The name of the scope.
rsstScopeName :: Lens' ResourceServerScopeType Text
rsstScopeName = lens _rsstScopeName (\s a -> s {_rsstScopeName = a})

-- | A description of the scope.
rsstScopeDescription :: Lens' ResourceServerScopeType Text
rsstScopeDescription = lens _rsstScopeDescription (\s a -> s {_rsstScopeDescription = a})

instance FromJSON ResourceServerScopeType where
  parseJSON =
    withObject
      "ResourceServerScopeType"
      ( \x ->
          ResourceServerScopeType'
            <$> (x .: "ScopeName") <*> (x .: "ScopeDescription")
      )

instance Hashable ResourceServerScopeType

instance NFData ResourceServerScopeType

instance ToJSON ResourceServerScopeType where
  toJSON ResourceServerScopeType' {..} =
    object
      ( catMaybes
          [ Just ("ScopeName" .= _rsstScopeName),
            Just ("ScopeDescription" .= _rsstScopeDescription)
          ]
      )
