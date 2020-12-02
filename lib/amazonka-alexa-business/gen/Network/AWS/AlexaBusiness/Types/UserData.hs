{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UserData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UserData where

import Network.AWS.AlexaBusiness.Types.EnrollmentStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information related to a user.
--
--
--
-- /See:/ 'userData' smart constructor.
data UserData = UserData'
  { _udEmail :: !(Maybe Text),
    _udLastName :: !(Maybe Text),
    _udEnrollmentId :: !(Maybe Text),
    _udUserARN :: !(Maybe Text),
    _udFirstName :: !(Maybe Text),
    _udEnrollmentStatus :: !(Maybe EnrollmentStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udEmail' - The email of a user.
--
-- * 'udLastName' - The last name of a user.
--
-- * 'udEnrollmentId' - The enrollment ARN of a user.
--
-- * 'udUserARN' - The ARN of a user.
--
-- * 'udFirstName' - The first name of a user.
--
-- * 'udEnrollmentStatus' - The enrollment status of a user.
userData ::
  UserData
userData =
  UserData'
    { _udEmail = Nothing,
      _udLastName = Nothing,
      _udEnrollmentId = Nothing,
      _udUserARN = Nothing,
      _udFirstName = Nothing,
      _udEnrollmentStatus = Nothing
    }

-- | The email of a user.
udEmail :: Lens' UserData (Maybe Text)
udEmail = lens _udEmail (\s a -> s {_udEmail = a})

-- | The last name of a user.
udLastName :: Lens' UserData (Maybe Text)
udLastName = lens _udLastName (\s a -> s {_udLastName = a})

-- | The enrollment ARN of a user.
udEnrollmentId :: Lens' UserData (Maybe Text)
udEnrollmentId = lens _udEnrollmentId (\s a -> s {_udEnrollmentId = a})

-- | The ARN of a user.
udUserARN :: Lens' UserData (Maybe Text)
udUserARN = lens _udUserARN (\s a -> s {_udUserARN = a})

-- | The first name of a user.
udFirstName :: Lens' UserData (Maybe Text)
udFirstName = lens _udFirstName (\s a -> s {_udFirstName = a})

-- | The enrollment status of a user.
udEnrollmentStatus :: Lens' UserData (Maybe EnrollmentStatus)
udEnrollmentStatus = lens _udEnrollmentStatus (\s a -> s {_udEnrollmentStatus = a})

instance FromJSON UserData where
  parseJSON =
    withObject
      "UserData"
      ( \x ->
          UserData'
            <$> (x .:? "Email")
            <*> (x .:? "LastName")
            <*> (x .:? "EnrollmentId")
            <*> (x .:? "UserArn")
            <*> (x .:? "FirstName")
            <*> (x .:? "EnrollmentStatus")
      )

instance Hashable UserData

instance NFData UserData
