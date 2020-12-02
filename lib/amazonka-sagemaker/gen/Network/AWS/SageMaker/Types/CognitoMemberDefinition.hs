{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CognitoMemberDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CognitoMemberDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies a Amazon Cognito user group. A user group can be used in on or more work teams.
--
--
--
-- /See:/ 'cognitoMemberDefinition' smart constructor.
data CognitoMemberDefinition = CognitoMemberDefinition'
  { _cmdUserPool ::
      !Text,
    _cmdUserGroup :: !Text,
    _cmdClientId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CognitoMemberDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmdUserPool' - An identifier for a user pool. The user pool must be in the same region as the service that you are calling.
--
-- * 'cmdUserGroup' - An identifier for a user group.
--
-- * 'cmdClientId' - An identifier for an application client. You must create the app client ID using Amazon Cognito.
cognitoMemberDefinition ::
  -- | 'cmdUserPool'
  Text ->
  -- | 'cmdUserGroup'
  Text ->
  -- | 'cmdClientId'
  Text ->
  CognitoMemberDefinition
cognitoMemberDefinition pUserPool_ pUserGroup_ pClientId_ =
  CognitoMemberDefinition'
    { _cmdUserPool = pUserPool_,
      _cmdUserGroup = pUserGroup_,
      _cmdClientId = pClientId_
    }

-- | An identifier for a user pool. The user pool must be in the same region as the service that you are calling.
cmdUserPool :: Lens' CognitoMemberDefinition Text
cmdUserPool = lens _cmdUserPool (\s a -> s {_cmdUserPool = a})

-- | An identifier for a user group.
cmdUserGroup :: Lens' CognitoMemberDefinition Text
cmdUserGroup = lens _cmdUserGroup (\s a -> s {_cmdUserGroup = a})

-- | An identifier for an application client. You must create the app client ID using Amazon Cognito.
cmdClientId :: Lens' CognitoMemberDefinition Text
cmdClientId = lens _cmdClientId (\s a -> s {_cmdClientId = a})

instance FromJSON CognitoMemberDefinition where
  parseJSON =
    withObject
      "CognitoMemberDefinition"
      ( \x ->
          CognitoMemberDefinition'
            <$> (x .: "UserPool") <*> (x .: "UserGroup") <*> (x .: "ClientId")
      )

instance Hashable CognitoMemberDefinition

instance NFData CognitoMemberDefinition

instance ToJSON CognitoMemberDefinition where
  toJSON CognitoMemberDefinition' {..} =
    object
      ( catMaybes
          [ Just ("UserPool" .= _cmdUserPool),
            Just ("UserGroup" .= _cmdUserGroup),
            Just ("ClientId" .= _cmdClientId)
          ]
      )
