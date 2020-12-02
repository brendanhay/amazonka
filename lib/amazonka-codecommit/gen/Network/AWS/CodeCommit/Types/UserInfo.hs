{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.UserInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.UserInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the user who made a specified commit.
--
--
--
-- /See:/ 'userInfo' smart constructor.
data UserInfo = UserInfo'
  { _uiEmail :: !(Maybe Text),
    _uiDate :: !(Maybe Text),
    _uiName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiEmail' - The email address associated with the user who made the commit, if any.
--
-- * 'uiDate' - The date when the specified commit was commited, in timestamp format with GMT offset.
--
-- * 'uiName' - The name of the user who made the specified commit.
userInfo ::
  UserInfo
userInfo =
  UserInfo'
    { _uiEmail = Nothing,
      _uiDate = Nothing,
      _uiName = Nothing
    }

-- | The email address associated with the user who made the commit, if any.
uiEmail :: Lens' UserInfo (Maybe Text)
uiEmail = lens _uiEmail (\s a -> s {_uiEmail = a})

-- | The date when the specified commit was commited, in timestamp format with GMT offset.
uiDate :: Lens' UserInfo (Maybe Text)
uiDate = lens _uiDate (\s a -> s {_uiDate = a})

-- | The name of the user who made the specified commit.
uiName :: Lens' UserInfo (Maybe Text)
uiName = lens _uiName (\s a -> s {_uiName = a})

instance FromJSON UserInfo where
  parseJSON =
    withObject
      "UserInfo"
      ( \x ->
          UserInfo'
            <$> (x .:? "email") <*> (x .:? "date") <*> (x .:? "name")
      )

instance Hashable UserInfo

instance NFData UserInfo
