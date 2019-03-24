{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Product where

import Network.AWS.AppStream.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an application in the application catalog.
--
--
--
-- /See:/ 'application' smart constructor.
data Application = Application'
  { _appEnabled          :: !(Maybe Bool)
  , _appLaunchPath       :: !(Maybe Text)
  , _appLaunchParameters :: !(Maybe Text)
  , _appName             :: !(Maybe Text)
  , _appDisplayName      :: !(Maybe Text)
  , _appMetadata         :: !(Maybe (Map Text Text))
  , _appIconURL          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Application' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appEnabled' - If there is a problem, the application can be disabled after image creation.
--
-- * 'appLaunchPath' - The path to the application executable in the instance.
--
-- * 'appLaunchParameters' - The arguments that are passed to the application at launch.
--
-- * 'appName' - The name of the application.
--
-- * 'appDisplayName' - The application name to display.
--
-- * 'appMetadata' - Additional attributes that describe the application.
--
-- * 'appIconURL' - The URL for the application icon. This URL might be time-limited.
application
    :: Application
application =
  Application'
    { _appEnabled = Nothing
    , _appLaunchPath = Nothing
    , _appLaunchParameters = Nothing
    , _appName = Nothing
    , _appDisplayName = Nothing
    , _appMetadata = Nothing
    , _appIconURL = Nothing
    }


-- | If there is a problem, the application can be disabled after image creation.
appEnabled :: Lens' Application (Maybe Bool)
appEnabled = lens _appEnabled (\ s a -> s{_appEnabled = a})

-- | The path to the application executable in the instance.
appLaunchPath :: Lens' Application (Maybe Text)
appLaunchPath = lens _appLaunchPath (\ s a -> s{_appLaunchPath = a})

-- | The arguments that are passed to the application at launch.
appLaunchParameters :: Lens' Application (Maybe Text)
appLaunchParameters = lens _appLaunchParameters (\ s a -> s{_appLaunchParameters = a})

-- | The name of the application.
appName :: Lens' Application (Maybe Text)
appName = lens _appName (\ s a -> s{_appName = a})

-- | The application name to display.
appDisplayName :: Lens' Application (Maybe Text)
appDisplayName = lens _appDisplayName (\ s a -> s{_appDisplayName = a})

-- | Additional attributes that describe the application.
appMetadata :: Lens' Application (HashMap Text Text)
appMetadata = lens _appMetadata (\ s a -> s{_appMetadata = a}) . _Default . _Map

-- | The URL for the application icon. This URL might be time-limited.
appIconURL :: Lens' Application (Maybe Text)
appIconURL = lens _appIconURL (\ s a -> s{_appIconURL = a})

instance FromJSON Application where
        parseJSON
          = withObject "Application"
              (\ x ->
                 Application' <$>
                   (x .:? "Enabled") <*> (x .:? "LaunchPath") <*>
                     (x .:? "LaunchParameters")
                     <*> (x .:? "Name")
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "Metadata" .!= mempty)
                     <*> (x .:? "IconURL"))

instance Hashable Application where

instance NFData Application where

-- | The persistent application settings for users of a stack.
--
--
--
-- /See:/ 'applicationSettings' smart constructor.
data ApplicationSettings = ApplicationSettings'
  { _aSettingsGroup :: !(Maybe Text)
  , _aEnabled       :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aSettingsGroup' - The path prefix for the S3 bucket where users
