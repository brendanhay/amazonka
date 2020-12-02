{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.TeamMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.TeamMember where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a team member in a project.
--
--
--
-- /See:/ 'teamMember' smart constructor.
data TeamMember = TeamMember'
  { _tmRemoteAccessAllowed ::
      !(Maybe Bool),
    _tmUserARN :: !Text,
    _tmProjectRole :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TeamMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmRemoteAccessAllowed' - Whether the user is allowed to remotely access project resources using an SSH public/private key pair.
--
-- * 'tmUserARN' - The Amazon Resource Name (ARN) of the user in IAM.
--
-- * 'tmProjectRole' - The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
teamMember ::
  -- | 'tmUserARN'
  Text ->
  -- | 'tmProjectRole'
  Text ->
  TeamMember
teamMember pUserARN_ pProjectRole_ =
  TeamMember'
    { _tmRemoteAccessAllowed = Nothing,
      _tmUserARN = pUserARN_,
      _tmProjectRole = pProjectRole_
    }

-- | Whether the user is allowed to remotely access project resources using an SSH public/private key pair.
tmRemoteAccessAllowed :: Lens' TeamMember (Maybe Bool)
tmRemoteAccessAllowed = lens _tmRemoteAccessAllowed (\s a -> s {_tmRemoteAccessAllowed = a})

-- | The Amazon Resource Name (ARN) of the user in IAM.
tmUserARN :: Lens' TeamMember Text
tmUserARN = lens _tmUserARN (\s a -> s {_tmUserARN = a})

-- | The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
tmProjectRole :: Lens' TeamMember Text
tmProjectRole = lens _tmProjectRole (\s a -> s {_tmProjectRole = a})

instance FromJSON TeamMember where
  parseJSON =
    withObject
      "TeamMember"
      ( \x ->
          TeamMember'
            <$> (x .:? "remoteAccessAllowed")
            <*> (x .: "userArn")
            <*> (x .: "projectRole")
      )

instance Hashable TeamMember

instance NFData TeamMember
