{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the metadata of the user.
--
--
--
-- /See:/ 'userMetadata' smart constructor.
data UserMetadata = UserMetadata'
  { _umGivenName :: !(Maybe Text),
    _umUsername :: !(Maybe Text),
    _umEmailAddress :: !(Maybe Text),
    _umId :: !(Maybe Text),
    _umSurname :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umGivenName' - The given name of the user before a rename operation.
--
-- * 'umUsername' - The name of the user.
--
-- * 'umEmailAddress' - The email address of the user.
--
-- * 'umId' - The ID of the user.
--
-- * 'umSurname' - The surname of the user.
userMetadata ::
  UserMetadata
userMetadata =
  UserMetadata'
    { _umGivenName = Nothing,
      _umUsername = Nothing,
      _umEmailAddress = Nothing,
      _umId = Nothing,
      _umSurname = Nothing
    }

-- | The given name of the user before a rename operation.
umGivenName :: Lens' UserMetadata (Maybe Text)
umGivenName = lens _umGivenName (\s a -> s {_umGivenName = a})

-- | The name of the user.
umUsername :: Lens' UserMetadata (Maybe Text)
umUsername = lens _umUsername (\s a -> s {_umUsername = a})

-- | The email address of the user.
umEmailAddress :: Lens' UserMetadata (Maybe Text)
umEmailAddress = lens _umEmailAddress (\s a -> s {_umEmailAddress = a})

-- | The ID of the user.
umId :: Lens' UserMetadata (Maybe Text)
umId = lens _umId (\s a -> s {_umId = a})

-- | The surname of the user.
umSurname :: Lens' UserMetadata (Maybe Text)
umSurname = lens _umSurname (\s a -> s {_umSurname = a})

instance FromJSON UserMetadata where
  parseJSON =
    withObject
      "UserMetadata"
      ( \x ->
          UserMetadata'
            <$> (x .:? "GivenName")
            <*> (x .:? "Username")
            <*> (x .:? "EmailAddress")
            <*> (x .:? "Id")
            <*> (x .:? "Surname")
      )

instance Hashable UserMetadata

instance NFData UserMetadata
