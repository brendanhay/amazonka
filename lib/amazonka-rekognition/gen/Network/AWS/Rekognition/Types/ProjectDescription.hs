{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProjectDescription where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.ProjectStatus

-- | A description of a Amazon Rekognition Custom Labels project.
--
--
--
-- /See:/ 'projectDescription' smart constructor.
data ProjectDescription = ProjectDescription'
  { _pdStatus ::
      !(Maybe ProjectStatus),
    _pdCreationTimestamp :: !(Maybe POSIX),
    _pdProjectARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdStatus' - The current status of the project.
--
-- * 'pdCreationTimestamp' - The Unix timestamp for the date and time that the project was created.
--
-- * 'pdProjectARN' - The Amazon Resource Name (ARN) of the project.
projectDescription ::
  ProjectDescription
projectDescription =
  ProjectDescription'
    { _pdStatus = Nothing,
      _pdCreationTimestamp = Nothing,
      _pdProjectARN = Nothing
    }

-- | The current status of the project.
pdStatus :: Lens' ProjectDescription (Maybe ProjectStatus)
pdStatus = lens _pdStatus (\s a -> s {_pdStatus = a})

-- | The Unix timestamp for the date and time that the project was created.
pdCreationTimestamp :: Lens' ProjectDescription (Maybe UTCTime)
pdCreationTimestamp = lens _pdCreationTimestamp (\s a -> s {_pdCreationTimestamp = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the project.
pdProjectARN :: Lens' ProjectDescription (Maybe Text)
pdProjectARN = lens _pdProjectARN (\s a -> s {_pdProjectARN = a})

instance FromJSON ProjectDescription where
  parseJSON =
    withObject
      "ProjectDescription"
      ( \x ->
          ProjectDescription'
            <$> (x .:? "Status")
            <*> (x .:? "CreationTimestamp")
            <*> (x .:? "ProjectArn")
      )

instance Hashable ProjectDescription

instance NFData ProjectDescription
