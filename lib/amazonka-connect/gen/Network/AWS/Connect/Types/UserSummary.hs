{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.UserSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains summary information about a user.
--
--
--
-- /See:/ 'userSummary' smart constructor.
data UserSummary = UserSummary'
  { _usARN :: !(Maybe Text),
    _usUsername :: !(Maybe Text),
    _usId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usARN' - The Amazon Resource Name (ARN) of the user account.
--
-- * 'usUsername' - The Amazon Connect user name of the user account.
--
-- * 'usId' - The identifier of the user account.
userSummary ::
  UserSummary
userSummary =
  UserSummary'
    { _usARN = Nothing,
      _usUsername = Nothing,
      _usId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the user account.
usARN :: Lens' UserSummary (Maybe Text)
usARN = lens _usARN (\s a -> s {_usARN = a})

-- | The Amazon Connect user name of the user account.
usUsername :: Lens' UserSummary (Maybe Text)
usUsername = lens _usUsername (\s a -> s {_usUsername = a})

-- | The identifier of the user account.
usId :: Lens' UserSummary (Maybe Text)
usId = lens _usId (\s a -> s {_usId = a})

instance FromJSON UserSummary where
  parseJSON =
    withObject
      "UserSummary"
      ( \x ->
          UserSummary'
            <$> (x .:? "Arn") <*> (x .:? "Username") <*> (x .:? "Id")
      )

instance Hashable UserSummary

instance NFData UserSummary
