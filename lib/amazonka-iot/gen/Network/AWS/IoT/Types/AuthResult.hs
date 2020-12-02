{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthResult where

import Network.AWS.IoT.Types.Allowed
import Network.AWS.IoT.Types.AuthDecision
import Network.AWS.IoT.Types.AuthInfo
import Network.AWS.IoT.Types.Denied
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The authorizer result.
--
--
--
-- /See:/ 'authResult' smart constructor.
data AuthResult = AuthResult'
  { _arDenied :: !(Maybe Denied),
    _arAuthDecision :: !(Maybe AuthDecision),
    _arAllowed :: !(Maybe Allowed),
    _arMissingContextValues :: !(Maybe [Text]),
    _arAuthInfo :: !(Maybe AuthInfo)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arDenied' - The policies and statements that denied the specified action.
--
-- * 'arAuthDecision' - The final authorization decision of this scenario. Multiple statements are taken into account when determining the authorization decision. An explicit deny statement can override multiple allow statements.
--
-- * 'arAllowed' - The policies and statements that allowed the specified action.
--
-- * 'arMissingContextValues' - Contains any missing context values found while evaluating policy.
--
-- * 'arAuthInfo' - Authorization information.
authResult ::
  AuthResult
authResult =
  AuthResult'
    { _arDenied = Nothing,
      _arAuthDecision = Nothing,
      _arAllowed = Nothing,
      _arMissingContextValues = Nothing,
      _arAuthInfo = Nothing
    }

-- | The policies and statements that denied the specified action.
arDenied :: Lens' AuthResult (Maybe Denied)
arDenied = lens _arDenied (\s a -> s {_arDenied = a})

-- | The final authorization decision of this scenario. Multiple statements are taken into account when determining the authorization decision. An explicit deny statement can override multiple allow statements.
arAuthDecision :: Lens' AuthResult (Maybe AuthDecision)
arAuthDecision = lens _arAuthDecision (\s a -> s {_arAuthDecision = a})

-- | The policies and statements that allowed the specified action.
arAllowed :: Lens' AuthResult (Maybe Allowed)
arAllowed = lens _arAllowed (\s a -> s {_arAllowed = a})

-- | Contains any missing context values found while evaluating policy.
arMissingContextValues :: Lens' AuthResult [Text]
arMissingContextValues = lens _arMissingContextValues (\s a -> s {_arMissingContextValues = a}) . _Default . _Coerce

-- | Authorization information.
arAuthInfo :: Lens' AuthResult (Maybe AuthInfo)
arAuthInfo = lens _arAuthInfo (\s a -> s {_arAuthInfo = a})

instance FromJSON AuthResult where
  parseJSON =
    withObject
      "AuthResult"
      ( \x ->
          AuthResult'
            <$> (x .:? "denied")
            <*> (x .:? "authDecision")
            <*> (x .:? "allowed")
            <*> (x .:? "missingContextValues" .!= mempty)
            <*> (x .:? "authInfo")
      )

instance Hashable AuthResult

instance NFData AuthResult
