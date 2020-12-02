{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.UserProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.UserProfileSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a user's profile in AWS CodeStar.
--
--
--
-- /See:/ 'userProfileSummary' smart constructor.
data UserProfileSummary = UserProfileSummary'
  { _upsSshPublicKey ::
      !(Maybe Text),
    _upsUserARN :: !(Maybe Text),
    _upsEmailAddress :: !(Maybe (Sensitive Text)),
    _upsDisplayName :: !(Maybe (Sensitive Text))
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserProfileSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upsSshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
--
-- * 'upsUserARN' - The Amazon Resource Name (ARN) of the user in IAM.
--
-- * 'upsEmailAddress' - The email address associated with the user.
--
-- * 'upsDisplayName' - The display name of a user in AWS CodeStar. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
userProfileSummary ::
  UserProfileSummary
userProfileSummary =
  UserProfileSummary'
    { _upsSshPublicKey = Nothing,
      _upsUserARN = Nothing,
      _upsEmailAddress = Nothing,
      _upsDisplayName = Nothing
    }

-- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
upsSshPublicKey :: Lens' UserProfileSummary (Maybe Text)
upsSshPublicKey = lens _upsSshPublicKey (\s a -> s {_upsSshPublicKey = a})

-- | The Amazon Resource Name (ARN) of the user in IAM.
upsUserARN :: Lens' UserProfileSummary (Maybe Text)
upsUserARN = lens _upsUserARN (\s a -> s {_upsUserARN = a})

-- | The email address associated with the user.
upsEmailAddress :: Lens' UserProfileSummary (Maybe Text)
upsEmailAddress = lens _upsEmailAddress (\s a -> s {_upsEmailAddress = a}) . mapping _Sensitive

-- | The display name of a user in AWS CodeStar. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
upsDisplayName :: Lens' UserProfileSummary (Maybe Text)
upsDisplayName = lens _upsDisplayName (\s a -> s {_upsDisplayName = a}) . mapping _Sensitive

instance FromJSON UserProfileSummary where
  parseJSON =
    withObject
      "UserProfileSummary"
      ( \x ->
          UserProfileSummary'
            <$> (x .:? "sshPublicKey")
            <*> (x .:? "userArn")
            <*> (x .:? "emailAddress")
            <*> (x .:? "displayName")
      )

instance Hashable UserProfileSummary

instance NFData UserProfileSummary
