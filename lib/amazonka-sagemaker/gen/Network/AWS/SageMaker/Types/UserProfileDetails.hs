{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserProfileDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserProfileDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.UserProfileStatus

-- | The user profile details.
--
--
--
-- /See:/ 'userProfileDetails' smart constructor.
data UserProfileDetails = UserProfileDetails'
  { _updCreationTime ::
      !(Maybe POSIX),
    _updStatus :: !(Maybe UserProfileStatus),
    _updUserProfileName :: !(Maybe Text),
    _updLastModifiedTime :: !(Maybe POSIX),
    _updDomainId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserProfileDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updCreationTime' - The creation time.
--
-- * 'updStatus' - The status.
--
-- * 'updUserProfileName' - The user profile name.
--
-- * 'updLastModifiedTime' - The last modified time.
--
-- * 'updDomainId' - The domain ID.
userProfileDetails ::
  UserProfileDetails
userProfileDetails =
  UserProfileDetails'
    { _updCreationTime = Nothing,
      _updStatus = Nothing,
      _updUserProfileName = Nothing,
      _updLastModifiedTime = Nothing,
      _updDomainId = Nothing
    }

-- | The creation time.
updCreationTime :: Lens' UserProfileDetails (Maybe UTCTime)
updCreationTime = lens _updCreationTime (\s a -> s {_updCreationTime = a}) . mapping _Time

-- | The status.
updStatus :: Lens' UserProfileDetails (Maybe UserProfileStatus)
updStatus = lens _updStatus (\s a -> s {_updStatus = a})

-- | The user profile name.
updUserProfileName :: Lens' UserProfileDetails (Maybe Text)
updUserProfileName = lens _updUserProfileName (\s a -> s {_updUserProfileName = a})

-- | The last modified time.
updLastModifiedTime :: Lens' UserProfileDetails (Maybe UTCTime)
updLastModifiedTime = lens _updLastModifiedTime (\s a -> s {_updLastModifiedTime = a}) . mapping _Time

-- | The domain ID.
updDomainId :: Lens' UserProfileDetails (Maybe Text)
updDomainId = lens _updDomainId (\s a -> s {_updDomainId = a})

instance FromJSON UserProfileDetails where
  parseJSON =
    withObject
      "UserProfileDetails"
      ( \x ->
          UserProfileDetails'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Status")
            <*> (x .:? "UserProfileName")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "DomainId")
      )

instance Hashable UserProfileDetails

instance NFData UserProfileDetails
