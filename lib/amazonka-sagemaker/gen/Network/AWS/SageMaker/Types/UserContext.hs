{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserContext where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the user who created or modified an experiment, trial, or trial component.
--
--
--
-- /See:/ 'userContext' smart constructor.
data UserContext = UserContext'
  { _ucUserProfileName ::
      !(Maybe Text),
    _ucUserProfileARN :: !(Maybe Text),
    _ucDomainId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucUserProfileName' - The name of the user's profile.
--
-- * 'ucUserProfileARN' - The Amazon Resource Name (ARN) of the user's profile.
--
-- * 'ucDomainId' - The domain associated with the user.
userContext ::
  UserContext
userContext =
  UserContext'
    { _ucUserProfileName = Nothing,
      _ucUserProfileARN = Nothing,
      _ucDomainId = Nothing
    }

-- | The name of the user's profile.
ucUserProfileName :: Lens' UserContext (Maybe Text)
ucUserProfileName = lens _ucUserProfileName (\s a -> s {_ucUserProfileName = a})

-- | The Amazon Resource Name (ARN) of the user's profile.
ucUserProfileARN :: Lens' UserContext (Maybe Text)
ucUserProfileARN = lens _ucUserProfileARN (\s a -> s {_ucUserProfileARN = a})

-- | The domain associated with the user.
ucDomainId :: Lens' UserContext (Maybe Text)
ucDomainId = lens _ucDomainId (\s a -> s {_ucDomainId = a})

instance FromJSON UserContext where
  parseJSON =
    withObject
      "UserContext"
      ( \x ->
          UserContext'
            <$> (x .:? "UserProfileName")
            <*> (x .:? "UserProfileArn")
            <*> (x .:? "DomainId")
      )

instance Hashable UserContext

instance NFData UserContext
