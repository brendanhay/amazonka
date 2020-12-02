{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.UserIdentityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserIdentityInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the identity of a user.
--
--
--
-- /See:/ 'userIdentityInfo' smart constructor.
data UserIdentityInfo = UserIdentityInfo'
  { _uiiEmail ::
      !(Maybe Text),
    _uiiLastName :: !(Maybe Text),
    _uiiFirstName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserIdentityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiiEmail' - The email address. If you are using SAML for identity management and include this parameter, an error is returned.
--
-- * 'uiiLastName' - The last name. This is required if you are using Amazon Connect or SAML for identity management.
--
-- * 'uiiFirstName' - The first name. This is required if you are using Amazon Connect or SAML for identity management.
userIdentityInfo ::
  UserIdentityInfo
userIdentityInfo =
  UserIdentityInfo'
    { _uiiEmail = Nothing,
      _uiiLastName = Nothing,
      _uiiFirstName = Nothing
    }

-- | The email address. If you are using SAML for identity management and include this parameter, an error is returned.
uiiEmail :: Lens' UserIdentityInfo (Maybe Text)
uiiEmail = lens _uiiEmail (\s a -> s {_uiiEmail = a})

-- | The last name. This is required if you are using Amazon Connect or SAML for identity management.
uiiLastName :: Lens' UserIdentityInfo (Maybe Text)
uiiLastName = lens _uiiLastName (\s a -> s {_uiiLastName = a})

-- | The first name. This is required if you are using Amazon Connect or SAML for identity management.
uiiFirstName :: Lens' UserIdentityInfo (Maybe Text)
uiiFirstName = lens _uiiFirstName (\s a -> s {_uiiFirstName = a})

instance FromJSON UserIdentityInfo where
  parseJSON =
    withObject
      "UserIdentityInfo"
      ( \x ->
          UserIdentityInfo'
            <$> (x .:? "Email") <*> (x .:? "LastName") <*> (x .:? "FirstName")
      )

instance Hashable UserIdentityInfo

instance NFData UserIdentityInfo

instance ToJSON UserIdentityInfo where
  toJSON UserIdentityInfo' {..} =
    object
      ( catMaybes
          [ ("Email" .=) <$> _uiiEmail,
            ("LastName" .=) <$> _uiiLastName,
            ("FirstName" .=) <$> _uiiFirstName
          ]
      )
