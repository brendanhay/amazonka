{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupInformation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a group.
--
-- /See:/ 'groupInformation' smart constructor.
data GroupInformation = GroupInformation'
  { _giLatestVersionARN ::
      !(Maybe Text),
    _giARN :: !(Maybe Text),
    _giName :: !(Maybe Text),
    _giCreationTimestamp :: !(Maybe Text),
    _giId :: !(Maybe Text),
    _giLatestVersion :: !(Maybe Text),
    _giLastUpdatedTimestamp :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giLatestVersionARN' - The ARN of the latest version associated with the group.
--
-- * 'giARN' - The ARN of the group.
--
-- * 'giName' - The name of the group.
--
-- * 'giCreationTimestamp' - The time, in milliseconds since the epoch, when the group was created.
--
-- * 'giId' - The ID of the group.
--
-- * 'giLatestVersion' - The ID of the latest version associated with the group.
--
-- * 'giLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the group was last updated.
groupInformation ::
  GroupInformation
groupInformation =
  GroupInformation'
    { _giLatestVersionARN = Nothing,
      _giARN = Nothing,
      _giName = Nothing,
      _giCreationTimestamp = Nothing,
      _giId = Nothing,
      _giLatestVersion = Nothing,
      _giLastUpdatedTimestamp = Nothing
    }

-- | The ARN of the latest version associated with the group.
giLatestVersionARN :: Lens' GroupInformation (Maybe Text)
giLatestVersionARN = lens _giLatestVersionARN (\s a -> s {_giLatestVersionARN = a})

-- | The ARN of the group.
giARN :: Lens' GroupInformation (Maybe Text)
giARN = lens _giARN (\s a -> s {_giARN = a})

-- | The name of the group.
giName :: Lens' GroupInformation (Maybe Text)
giName = lens _giName (\s a -> s {_giName = a})

-- | The time, in milliseconds since the epoch, when the group was created.
giCreationTimestamp :: Lens' GroupInformation (Maybe Text)
giCreationTimestamp = lens _giCreationTimestamp (\s a -> s {_giCreationTimestamp = a})

-- | The ID of the group.
giId :: Lens' GroupInformation (Maybe Text)
giId = lens _giId (\s a -> s {_giId = a})

-- | The ID of the latest version associated with the group.
giLatestVersion :: Lens' GroupInformation (Maybe Text)
giLatestVersion = lens _giLatestVersion (\s a -> s {_giLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the group was last updated.
giLastUpdatedTimestamp :: Lens' GroupInformation (Maybe Text)
giLastUpdatedTimestamp = lens _giLastUpdatedTimestamp (\s a -> s {_giLastUpdatedTimestamp = a})

instance FromJSON GroupInformation where
  parseJSON =
    withObject
      "GroupInformation"
      ( \x ->
          GroupInformation'
            <$> (x .:? "LatestVersionArn")
            <*> (x .:? "Arn")
            <*> (x .:? "Name")
            <*> (x .:? "CreationTimestamp")
            <*> (x .:? "Id")
            <*> (x .:? "LatestVersion")
            <*> (x .:? "LastUpdatedTimestamp")
      )

instance Hashable GroupInformation

instance NFData GroupInformation
