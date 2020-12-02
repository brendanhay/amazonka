{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ServiceAccountCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ServiceAccountCredentials where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the credentials for the service account used by the fleet or image builder to connect to the directory.
--
--
--
-- /See:/ 'serviceAccountCredentials' smart constructor.
data ServiceAccountCredentials = ServiceAccountCredentials'
  { _sacAccountName ::
      !(Sensitive Text),
    _sacAccountPassword ::
      !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceAccountCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sacAccountName' - The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
--
-- * 'sacAccountPassword' - The password for the account.
serviceAccountCredentials ::
  -- | 'sacAccountName'
  Text ->
  -- | 'sacAccountPassword'
  Text ->
  ServiceAccountCredentials
serviceAccountCredentials pAccountName_ pAccountPassword_ =
  ServiceAccountCredentials'
    { _sacAccountName =
        _Sensitive # pAccountName_,
      _sacAccountPassword = _Sensitive # pAccountPassword_
    }

-- | The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
sacAccountName :: Lens' ServiceAccountCredentials Text
sacAccountName = lens _sacAccountName (\s a -> s {_sacAccountName = a}) . _Sensitive

-- | The password for the account.
sacAccountPassword :: Lens' ServiceAccountCredentials Text
sacAccountPassword = lens _sacAccountPassword (\s a -> s {_sacAccountPassword = a}) . _Sensitive

instance FromJSON ServiceAccountCredentials where
  parseJSON =
    withObject
      "ServiceAccountCredentials"
      ( \x ->
          ServiceAccountCredentials'
            <$> (x .: "AccountName") <*> (x .: "AccountPassword")
      )

instance Hashable ServiceAccountCredentials

instance NFData ServiceAccountCredentials

instance ToJSON ServiceAccountCredentials where
  toJSON ServiceAccountCredentials' {..} =
    object
      ( catMaybes
          [ Just ("AccountName" .= _sacAccountName),
            Just ("AccountPassword" .= _sacAccountPassword)
          ]
      )
