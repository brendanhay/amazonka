{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AppStatus
import Network.AWS.SageMaker.Types.AppType

-- | Details about an Amazon SageMaker app.
--
--
--
-- /See:/ 'appDetails' smart constructor.
data AppDetails = AppDetails'
  { _adCreationTime :: !(Maybe POSIX),
    _adStatus :: !(Maybe AppStatus),
    _adUserProfileName :: !(Maybe Text),
    _adAppName :: !(Maybe Text),
    _adDomainId :: !(Maybe Text),
    _adAppType :: !(Maybe AppType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AppDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adCreationTime' - The creation time.
--
-- * 'adStatus' - The status.
--
-- * 'adUserProfileName' - The user profile name.
--
-- * 'adAppName' - The name of the app.
--
-- * 'adDomainId' - The domain ID.
--
-- * 'adAppType' - The type of app.
appDetails ::
  AppDetails
appDetails =
  AppDetails'
    { _adCreationTime = Nothing,
      _adStatus = Nothing,
      _adUserProfileName = Nothing,
      _adAppName = Nothing,
      _adDomainId = Nothing,
      _adAppType = Nothing
    }

-- | The creation time.
adCreationTime :: Lens' AppDetails (Maybe UTCTime)
adCreationTime = lens _adCreationTime (\s a -> s {_adCreationTime = a}) . mapping _Time

-- | The status.
adStatus :: Lens' AppDetails (Maybe AppStatus)
adStatus = lens _adStatus (\s a -> s {_adStatus = a})

-- | The user profile name.
adUserProfileName :: Lens' AppDetails (Maybe Text)
adUserProfileName = lens _adUserProfileName (\s a -> s {_adUserProfileName = a})

-- | The name of the app.
adAppName :: Lens' AppDetails (Maybe Text)
adAppName = lens _adAppName (\s a -> s {_adAppName = a})

-- | The domain ID.
adDomainId :: Lens' AppDetails (Maybe Text)
adDomainId = lens _adDomainId (\s a -> s {_adDomainId = a})

-- | The type of app.
adAppType :: Lens' AppDetails (Maybe AppType)
adAppType = lens _adAppType (\s a -> s {_adAppType = a})

instance FromJSON AppDetails where
  parseJSON =
    withObject
      "AppDetails"
      ( \x ->
          AppDetails'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Status")
            <*> (x .:? "UserProfileName")
            <*> (x .:? "AppName")
            <*> (x .:? "DomainId")
            <*> (x .:? "AppType")
      )

instance Hashable AppDetails

instance NFData AppDetails
