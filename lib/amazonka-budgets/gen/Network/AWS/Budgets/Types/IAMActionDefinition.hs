{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.IAMActionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.IAMActionDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The AWS Identity and Access Management (IAM) action definition details.
--
--
--
-- /See:/ 'iamActionDefinition' smart constructor.
data IAMActionDefinition = IAMActionDefinition'
  { _iadGroups ::
      !(Maybe (List1 Text)),
    _iadRoles :: !(Maybe (List1 Text)),
    _iadUsers :: !(Maybe (List1 Text)),
    _iadPolicyARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IAMActionDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iadGroups' - A list of groups to be attached. There must be at least one group.
--
-- * 'iadRoles' - A list of roles to be attached. There must be at least one role.
--
-- * 'iadUsers' - A list of users to be attached. There must be at least one user.
--
-- * 'iadPolicyARN' - The Amazon Resource Name (ARN) of the policy to be attached.
iamActionDefinition ::
  -- | 'iadPolicyARN'
  Text ->
  IAMActionDefinition
iamActionDefinition pPolicyARN_ =
  IAMActionDefinition'
    { _iadGroups = Nothing,
      _iadRoles = Nothing,
      _iadUsers = Nothing,
      _iadPolicyARN = pPolicyARN_
    }

-- | A list of groups to be attached. There must be at least one group.
iadGroups :: Lens' IAMActionDefinition (Maybe (NonEmpty Text))
iadGroups = lens _iadGroups (\s a -> s {_iadGroups = a}) . mapping _List1

-- | A list of roles to be attached. There must be at least one role.
iadRoles :: Lens' IAMActionDefinition (Maybe (NonEmpty Text))
iadRoles = lens _iadRoles (\s a -> s {_iadRoles = a}) . mapping _List1

-- | A list of users to be attached. There must be at least one user.
iadUsers :: Lens' IAMActionDefinition (Maybe (NonEmpty Text))
iadUsers = lens _iadUsers (\s a -> s {_iadUsers = a}) . mapping _List1

-- | The Amazon Resource Name (ARN) of the policy to be attached.
iadPolicyARN :: Lens' IAMActionDefinition Text
iadPolicyARN = lens _iadPolicyARN (\s a -> s {_iadPolicyARN = a})

instance FromJSON IAMActionDefinition where
  parseJSON =
    withObject
      "IAMActionDefinition"
      ( \x ->
          IAMActionDefinition'
            <$> (x .:? "Groups")
            <*> (x .:? "Roles")
            <*> (x .:? "Users")
            <*> (x .: "PolicyArn")
      )

instance Hashable IAMActionDefinition

instance NFData IAMActionDefinition

instance ToJSON IAMActionDefinition where
  toJSON IAMActionDefinition' {..} =
    object
      ( catMaybes
          [ ("Groups" .=) <$> _iadGroups,
            ("Roles" .=) <$> _iadRoles,
            ("Users" .=) <$> _iadUsers,
            Just ("PolicyArn" .= _iadPolicyARN)
          ]
      )
