{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MemberDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MemberDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CognitoMemberDefinition
import Network.AWS.SageMaker.Types.OidcMemberDefinition

-- | Defines an Amazon Cognito or your own OIDC IdP user group that is part of a work team.
--
--
--
-- /See:/ 'memberDefinition' smart constructor.
data MemberDefinition = MemberDefinition'
  { _mdOidcMemberDefinition ::
      !(Maybe OidcMemberDefinition),
    _mdCognitoMemberDefinition ::
      !(Maybe CognitoMemberDefinition)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MemberDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdOidcMemberDefinition' - A list user groups that exist in your OIDC Identity Provider (IdP). One to ten groups can be used to create a single private work team. When you add a user group to the list of @Groups@ , you can add that user group to one or more private work teams. If you add a user group to a private work team, all workers in that user group are added to the work team.
--
-- * 'mdCognitoMemberDefinition' - The Amazon Cognito user group that is part of the work team.
memberDefinition ::
  MemberDefinition
memberDefinition =
  MemberDefinition'
    { _mdOidcMemberDefinition = Nothing,
      _mdCognitoMemberDefinition = Nothing
    }

-- | A list user groups that exist in your OIDC Identity Provider (IdP). One to ten groups can be used to create a single private work team. When you add a user group to the list of @Groups@ , you can add that user group to one or more private work teams. If you add a user group to a private work team, all workers in that user group are added to the work team.
mdOidcMemberDefinition :: Lens' MemberDefinition (Maybe OidcMemberDefinition)
mdOidcMemberDefinition = lens _mdOidcMemberDefinition (\s a -> s {_mdOidcMemberDefinition = a})

-- | The Amazon Cognito user group that is part of the work team.
mdCognitoMemberDefinition :: Lens' MemberDefinition (Maybe CognitoMemberDefinition)
mdCognitoMemberDefinition = lens _mdCognitoMemberDefinition (\s a -> s {_mdCognitoMemberDefinition = a})

instance FromJSON MemberDefinition where
  parseJSON =
    withObject
      "MemberDefinition"
      ( \x ->
          MemberDefinition'
            <$> (x .:? "OidcMemberDefinition")
            <*> (x .:? "CognitoMemberDefinition")
      )

instance Hashable MemberDefinition

instance NFData MemberDefinition

instance ToJSON MemberDefinition where
  toJSON MemberDefinition' {..} =
    object
      ( catMaybes
          [ ("OidcMemberDefinition" .=) <$> _mdOidcMemberDefinition,
            ("CognitoMemberDefinition" .=) <$> _mdCognitoMemberDefinition
          ]
      )
