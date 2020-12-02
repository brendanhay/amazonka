{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.TemporaryCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.TemporaryCredential where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the data needed by RDP clients such as the Microsoft Remote Desktop Connection to log in to the instance.
--
--
--
-- /See:/ 'temporaryCredential' smart constructor.
data TemporaryCredential = TemporaryCredential'
  { _tcInstanceId ::
      !(Maybe Text),
    _tcUsername :: !(Maybe Text),
    _tcPassword :: !(Maybe Text),
    _tcValidForInMinutes :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TemporaryCredential' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcInstanceId' - The instance's AWS OpsWorks Stacks ID.
--
-- * 'tcUsername' - The user name.
--
-- * 'tcPassword' - The password.
--
-- * 'tcValidForInMinutes' - The length of time (in minutes) that the grant is valid. When the grant expires, at the end of this period, the user will no longer be able to use the credentials to log in. If they are logged in at the time, they will be automatically logged out.
temporaryCredential ::
  TemporaryCredential
temporaryCredential =
  TemporaryCredential'
    { _tcInstanceId = Nothing,
      _tcUsername = Nothing,
      _tcPassword = Nothing,
      _tcValidForInMinutes = Nothing
    }

-- | The instance's AWS OpsWorks Stacks ID.
tcInstanceId :: Lens' TemporaryCredential (Maybe Text)
tcInstanceId = lens _tcInstanceId (\s a -> s {_tcInstanceId = a})

-- | The user name.
tcUsername :: Lens' TemporaryCredential (Maybe Text)
tcUsername = lens _tcUsername (\s a -> s {_tcUsername = a})

-- | The password.
tcPassword :: Lens' TemporaryCredential (Maybe Text)
tcPassword = lens _tcPassword (\s a -> s {_tcPassword = a})

-- | The length of time (in minutes) that the grant is valid. When the grant expires, at the end of this period, the user will no longer be able to use the credentials to log in. If they are logged in at the time, they will be automatically logged out.
tcValidForInMinutes :: Lens' TemporaryCredential (Maybe Int)
tcValidForInMinutes = lens _tcValidForInMinutes (\s a -> s {_tcValidForInMinutes = a})

instance FromJSON TemporaryCredential where
  parseJSON =
    withObject
      "TemporaryCredential"
      ( \x ->
          TemporaryCredential'
            <$> (x .:? "InstanceId")
            <*> (x .:? "Username")
            <*> (x .:? "Password")
            <*> (x .:? "ValidForInMinutes")
      )

instance Hashable TemporaryCredential

instance NFData TemporaryCredential
