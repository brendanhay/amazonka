{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.VersionInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.VersionInformation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a version.
--
-- /See:/ 'versionInformation' smart constructor.
data VersionInformation = VersionInformation'
  { _viARN ::
      !(Maybe Text),
    _viCreationTimestamp :: !(Maybe Text),
    _viVersion :: !(Maybe Text),
    _viId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VersionInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viARN' - The ARN of the version.
--
-- * 'viCreationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- * 'viVersion' - The ID of the version.
--
-- * 'viId' - The ID of the parent definition that the version is associated with.
versionInformation ::
  VersionInformation
versionInformation =
  VersionInformation'
    { _viARN = Nothing,
      _viCreationTimestamp = Nothing,
      _viVersion = Nothing,
      _viId = Nothing
    }

-- | The ARN of the version.
viARN :: Lens' VersionInformation (Maybe Text)
viARN = lens _viARN (\s a -> s {_viARN = a})

-- | The time, in milliseconds since the epoch, when the version was created.
viCreationTimestamp :: Lens' VersionInformation (Maybe Text)
viCreationTimestamp = lens _viCreationTimestamp (\s a -> s {_viCreationTimestamp = a})

-- | The ID of the version.
viVersion :: Lens' VersionInformation (Maybe Text)
viVersion = lens _viVersion (\s a -> s {_viVersion = a})

-- | The ID of the parent definition that the version is associated with.
viId :: Lens' VersionInformation (Maybe Text)
viId = lens _viId (\s a -> s {_viId = a})

instance FromJSON VersionInformation where
  parseJSON =
    withObject
      "VersionInformation"
      ( \x ->
          VersionInformation'
            <$> (x .:? "Arn")
            <*> (x .:? "CreationTimestamp")
            <*> (x .:? "Version")
            <*> (x .:? "Id")
      )

instance Hashable VersionInformation

instance NFData VersionInformation
