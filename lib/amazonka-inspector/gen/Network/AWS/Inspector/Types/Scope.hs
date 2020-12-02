{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Scope
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Scope where

import Network.AWS.Inspector.Types.ScopeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type contains key-value pairs that identify various Amazon resources.
--
--
--
-- /See:/ 'scope' smart constructor.
data Scope = Scope'
  { _sValue :: !(Maybe Text),
    _sKey :: !(Maybe ScopeType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scope' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sValue' - The resource identifier for the specified scope type.
--
-- * 'sKey' - The type of the scope.
scope ::
  Scope
scope = Scope' {_sValue = Nothing, _sKey = Nothing}

-- | The resource identifier for the specified scope type.
sValue :: Lens' Scope (Maybe Text)
sValue = lens _sValue (\s a -> s {_sValue = a})

-- | The type of the scope.
sKey :: Lens' Scope (Maybe ScopeType)
sKey = lens _sKey (\s a -> s {_sKey = a})

instance FromJSON Scope where
  parseJSON =
    withObject
      "Scope"
      (\x -> Scope' <$> (x .:? "value") <*> (x .:? "key"))

instance Hashable Scope

instance NFData Scope
