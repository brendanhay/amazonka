{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.LaunchDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.LaunchDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the latest launch of an application.
--
--
--
-- /See:/ 'launchDetails' smart constructor.
data LaunchDetails = LaunchDetails'
  { _ldStackId :: !(Maybe Text),
    _ldLatestLaunchTime :: !(Maybe POSIX),
    _ldStackName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldStackId' - The ID of the latest stack launched for this application.
--
-- * 'ldLatestLaunchTime' - The latest time that this application was launched successfully.
--
-- * 'ldStackName' - The name of the latest stack launched for this application.
launchDetails ::
  LaunchDetails
launchDetails =
  LaunchDetails'
    { _ldStackId = Nothing,
      _ldLatestLaunchTime = Nothing,
      _ldStackName = Nothing
    }

-- | The ID of the latest stack launched for this application.
ldStackId :: Lens' LaunchDetails (Maybe Text)
ldStackId = lens _ldStackId (\s a -> s {_ldStackId = a})

-- | The latest time that this application was launched successfully.
ldLatestLaunchTime :: Lens' LaunchDetails (Maybe UTCTime)
ldLatestLaunchTime = lens _ldLatestLaunchTime (\s a -> s {_ldLatestLaunchTime = a}) . mapping _Time

-- | The name of the latest stack launched for this application.
ldStackName :: Lens' LaunchDetails (Maybe Text)
ldStackName = lens _ldStackName (\s a -> s {_ldStackName = a})

instance FromJSON LaunchDetails where
  parseJSON =
    withObject
      "LaunchDetails"
      ( \x ->
          LaunchDetails'
            <$> (x .:? "stackId")
            <*> (x .:? "latestLaunchTime")
            <*> (x .:? "stackName")
      )

instance Hashable LaunchDetails

instance NFData LaunchDetails
