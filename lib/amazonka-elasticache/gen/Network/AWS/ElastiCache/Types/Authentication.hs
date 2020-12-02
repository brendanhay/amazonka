{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Authentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Authentication where

import Network.AWS.ElastiCache.Types.AuthenticationType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether the user requires a password to authenticate.
--
--
--
-- /See:/ 'authentication' smart constructor.
data Authentication = Authentication'
  { _aPasswordCount ::
      !(Maybe Int),
    _aType :: !(Maybe AuthenticationType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Authentication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aPasswordCount' - The number of passwords belonging to the user. The maximum is two.
--
-- * 'aType' - Indicates whether the user requires a password to authenticate.
authentication ::
  Authentication
authentication =
  Authentication' {_aPasswordCount = Nothing, _aType = Nothing}

-- | The number of passwords belonging to the user. The maximum is two.
aPasswordCount :: Lens' Authentication (Maybe Int)
aPasswordCount = lens _aPasswordCount (\s a -> s {_aPasswordCount = a})

-- | Indicates whether the user requires a password to authenticate.
aType :: Lens' Authentication (Maybe AuthenticationType)
aType = lens _aType (\s a -> s {_aType = a})

instance FromXML Authentication where
  parseXML x =
    Authentication' <$> (x .@? "PasswordCount") <*> (x .@? "Type")

instance Hashable Authentication

instance NFData Authentication
