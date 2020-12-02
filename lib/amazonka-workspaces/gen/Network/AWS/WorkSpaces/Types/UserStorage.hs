{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.UserStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.UserStorage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the user storage for a WorkSpace bundle.
--
--
--
-- /See:/ 'userStorage' smart constructor.
newtype UserStorage = UserStorage' {_usCapacity :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserStorage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usCapacity' - The size of the user storage.
userStorage ::
  UserStorage
userStorage = UserStorage' {_usCapacity = Nothing}

-- | The size of the user storage.
usCapacity :: Lens' UserStorage (Maybe Text)
usCapacity = lens _usCapacity (\s a -> s {_usCapacity = a})

instance FromJSON UserStorage where
  parseJSON =
    withObject
      "UserStorage"
      (\x -> UserStorage' <$> (x .:? "Capacity"))

instance Hashable UserStorage

instance NFData UserStorage
