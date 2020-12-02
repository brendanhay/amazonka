{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.ProjectDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.ProjectDetails where

import Network.AWS.Lens
import Network.AWS.Mobile.Types.ProjectState
import Network.AWS.Mobile.Types.Resource
import Network.AWS.Prelude

-- | Detailed information about an AWS Mobile Hub project.
--
--
--
-- /See:/ 'projectDetails' smart constructor.
data ProjectDetails = ProjectDetails'
  { _pdState ::
      !(Maybe ProjectState),
    _pdResources :: !(Maybe [Resource]),
    _pdCreatedDate :: !(Maybe POSIX),
    _pdConsoleURL :: !(Maybe Text),
    _pdName :: !(Maybe Text),
    _pdRegion :: !(Maybe Text),
    _pdProjectId :: !(Maybe Text),
    _pdLastUpdatedDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdState' - Undocumented member.
--
-- * 'pdResources' - Undocumented member.
--
-- * 'pdCreatedDate' - Date the project was created.
--
-- * 'pdConsoleURL' - Website URL for this project in the AWS Mobile Hub console.
--
-- * 'pdName' - Undocumented member.
--
-- * 'pdRegion' - Undocumented member.
--
-- * 'pdProjectId' - Undocumented member.
--
-- * 'pdLastUpdatedDate' - Date of the last modification of the project.
projectDetails ::
  ProjectDetails
projectDetails =
  ProjectDetails'
    { _pdState = Nothing,
      _pdResources = Nothing,
      _pdCreatedDate = Nothing,
      _pdConsoleURL = Nothing,
      _pdName = Nothing,
      _pdRegion = Nothing,
      _pdProjectId = Nothing,
      _pdLastUpdatedDate = Nothing
    }

-- | Undocumented member.
pdState :: Lens' ProjectDetails (Maybe ProjectState)
pdState = lens _pdState (\s a -> s {_pdState = a})

-- | Undocumented member.
pdResources :: Lens' ProjectDetails [Resource]
pdResources = lens _pdResources (\s a -> s {_pdResources = a}) . _Default . _Coerce

-- | Date the project was created.
pdCreatedDate :: Lens' ProjectDetails (Maybe UTCTime)
pdCreatedDate = lens _pdCreatedDate (\s a -> s {_pdCreatedDate = a}) . mapping _Time

-- | Website URL for this project in the AWS Mobile Hub console.
pdConsoleURL :: Lens' ProjectDetails (Maybe Text)
pdConsoleURL = lens _pdConsoleURL (\s a -> s {_pdConsoleURL = a})

-- | Undocumented member.
pdName :: Lens' ProjectDetails (Maybe Text)
pdName = lens _pdName (\s a -> s {_pdName = a})

-- | Undocumented member.
pdRegion :: Lens' ProjectDetails (Maybe Text)
pdRegion = lens _pdRegion (\s a -> s {_pdRegion = a})

-- | Undocumented member.
pdProjectId :: Lens' ProjectDetails (Maybe Text)
pdProjectId = lens _pdProjectId (\s a -> s {_pdProjectId = a})

-- | Date of the last modification of the project.
pdLastUpdatedDate :: Lens' ProjectDetails (Maybe UTCTime)
pdLastUpdatedDate = lens _pdLastUpdatedDate (\s a -> s {_pdLastUpdatedDate = a}) . mapping _Time

instance FromJSON ProjectDetails where
  parseJSON =
    withObject
      "ProjectDetails"
      ( \x ->
          ProjectDetails'
            <$> (x .:? "state")
            <*> (x .:? "resources" .!= mempty)
            <*> (x .:? "createdDate")
            <*> (x .:? "consoleUrl")
            <*> (x .:? "name")
            <*> (x .:? "region")
            <*> (x .:? "projectId")
            <*> (x .:? "lastUpdatedDate")
      )

instance Hashable ProjectDetails

instance NFData ProjectDetails
