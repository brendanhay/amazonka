{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Mobile.Types.Product where

import Network.AWS.Lens
import Network.AWS.Mobile.Types.Sum
import Network.AWS.Prelude

-- | The details of the bundle.
--
--
--
-- /See:/ 'bundleDetails' smart constructor.
data BundleDetails = BundleDetails'
  { _bdAvailablePlatforms :: !(Maybe [Platform])
  , _bdBundleId           :: !(Maybe Text)
  , _bdVersion            :: !(Maybe Text)
  , _bdIconURL            :: !(Maybe Text)
  , _bdTitle              :: !(Maybe Text)
  , _bdDescription        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BundleDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdAvailablePlatforms' - Undocumented member.
--
-- * 'bdBundleId' - Undocumented member.
--
-- * 'bdVersion' - Undocumented member.
--
-- * 'bdIconURL' - Undocumented member.
--
-- * 'bdTitle' - Undocumented member.
--
-- * 'bdDescription' - Undocumented member.
bundleDetails
    :: BundleDetails
bundleDetails =
  BundleDetails'
    { _bdAvailablePlatforms = Nothing
    , _bdBundleId = Nothing
    , _bdVersion = Nothing
    , _bdIconURL = Nothing
    , _bdTitle = Nothing
    , _bdDescription = Nothing
    }


-- | Undocumented member.
bdAvailablePlatforms :: Lens' BundleDetails [Platform]
bdAvailablePlatforms = lens _bdAvailablePlatforms (\ s a -> s{_bdAvailablePlatforms = a}) . _Default . _Coerce

-- | Undocumented member.
bdBundleId :: Lens' BundleDetails (Maybe Text)
bdBundleId = lens _bdBundleId (\ s a -> s{_bdBundleId = a})

-- | Undocumented member.
bdVersion :: Lens' BundleDetails (Maybe Text)
bdVersion = lens _bdVersion (\ s a -> s{_bdVersion = a})

-- | Undocumented member.
bdIconURL :: Lens' BundleDetails (Maybe Text)
bdIconURL = lens _bdIconURL (\ s a -> s{_bdIconURL = a})

-- | Undocumented member.
bdTitle :: Lens' BundleDetails (Maybe Text)
bdTitle = lens _bdTitle (\ s a -> s{_bdTitle = a})

-- | Undocumented member.
bdDescription :: Lens' BundleDetails (Maybe Text)
bdDescription = lens _bdDescription (\ s a -> s{_bdDescription = a})

instance FromJSON BundleDetails where
        parseJSON
          = withObject "BundleDetails"
              (\ x ->
                 BundleDetails' <$>
                   (x .:? "availablePlatforms" .!= mempty) <*>
                     (x .:? "bundleId")
                     <*> (x .:? "version")
                     <*> (x .:? "iconUrl")
                     <*> (x .:? "title")
                     <*> (x .:? "description"))

instance Hashable BundleDetails where

instance NFData BundleDetails where

-- | Detailed information about an AWS Mobile Hub project.
--
--
--
-- /See:/ 'projectDetails' smart constructor.
data ProjectDetails = ProjectDetails'
  { _pdState           :: !(Maybe ProjectState)
  , _pdResources       :: !(Maybe [Resource])
  , _pdCreatedDate     :: !(Maybe POSIX)
  , _pdConsoleURL      :: !(Maybe Text)
  , _pdName            :: !(Maybe Text)
  , _pdRegion          :: !(Maybe Text)
  , _pdProjectId       :: !(Maybe Text)
  , _pdLastUpdatedDate :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
projectDetails
    :: ProjectDetails
projectDetails =
  ProjectDetails'
    { _pdState = Nothing
    , _pdResources = Nothing
    , _pdCreatedDate = Nothing
    , _pdConsoleURL = Nothing
    , _pdName = Nothing
    , _pdRegion = Nothing
    , _pdProjectId = Nothing
    , _pdLastUpdatedDate = Nothing
    }


-- | Undocumented member.
pdState :: Lens' ProjectDetails (Maybe ProjectState)
pdState = lens _pdState (\ s a -> s{_pdState = a})

-- | Undocumented member.
pdResources :: Lens' ProjectDetails [Resource]
pdResources = lens _pdResources (\ s a -> s{_pdResources = a}) . _Default . _Coerce

-- | Date the project was created.
pdCreatedDate :: Lens' ProjectDetails (Maybe UTCTime)
pdCreatedDate = lens _pdCreatedDate (\ s a -> s{_pdCreatedDate = a}) . mapping _Time

-- | Website URL for this project in the AWS Mobile Hub console.
pdConsoleURL :: Lens' ProjectDetails (Maybe Text)
pdConsoleURL = lens _pdConsoleURL (\ s a -> s{_pdConsoleURL = a})

-- | Undocumented member.
pdName :: Lens' ProjectDetails (Maybe Text)
pdName = lens _pdName (\ s a -> s{_pdName = a})

-- | Undocumented member.
pdRegion :: Lens' ProjectDetails (Maybe Text)
pdRegion = lens _pdRegion (\ s a -> s{_pdRegion = a})

-- | Undocumented member.
pdProjectId :: Lens' ProjectDetails (Maybe Text)
pdProjectId = lens _pdProjectId (\ s a -> s{_pdProjectId = a})

-- | Date of the last modification of the project.
pdLastUpdatedDate :: Lens' ProjectDetails (Maybe UTCTime)
pdLastUpdatedDate = lens _pdLastUpdatedDate (\ s a -> s{_pdLastUpdatedDate = a}) . mapping _Time

instance FromJSON ProjectDetails where
        parseJSON
          = withObject "ProjectDetails"
              (\ x ->
                 ProjectDetails' <$>
                   (x .:? "state") <*> (x .:? "resources" .!= mempty)
                     <*> (x .:? "createdDate")
                     <*> (x .:? "consoleUrl")
                     <*> (x .:? "name")
                     <*> (x .:? "region")
                     <*> (x .:? "projectId")
                     <*> (x .:? "lastUpdatedDate"))

instance Hashable ProjectDetails where

instance NFData ProjectDetails where

-- | Summary information about an AWS Mobile Hub project.
--
--
--
-- /See:/ 'projectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { _psName      :: !(Maybe Text)
  , _psProjectId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProjectSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psName' - Name of the project.
--
-- * 'psProjectId' - Unique project identifier.
projectSummary
    :: ProjectSummary
projectSummary = ProjectSummary' {_psName = Nothing, _psProjectId = Nothing}


-- | Name of the project.
psName :: Lens' ProjectSummary (Maybe Text)
psName = lens _psName (\ s a -> s{_psName = a})

-- | Unique project identifier.
psProjectId :: Lens' ProjectSummary (Maybe Text)
psProjectId = lens _psProjectId (\ s a -> s{_psProjectId = a})

instance FromJSON ProjectSummary where
        parseJSON
          = withObject "ProjectSummary"
              (\ x ->
                 ProjectSummary' <$>
                   (x .:? "name") <*> (x .:? "projectId"))

instance Hashable ProjectSummary where

instance NFData ProjectSummary where

-- | Information about an instance of an AWS resource associated with a project.
--
--
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rFeature    :: !(Maybe Text)
  , _rArn        :: !(Maybe Text)
  , _rName       :: !(Maybe Text)
  , _rAttributes :: !(Maybe (Map Text Text))
  , _rType       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rFeature' - Undocumented member.
--
-- * 'rArn' - Undocumented member.
--
-- * 'rName' - Undocumented member.
--
-- * 'rAttributes' - Undocumented member.
--
-- * 'rType' - Undocumented member.
resource
    :: Resource
resource =
  Resource'
    { _rFeature = Nothing
    , _rArn = Nothing
    , _rName = Nothing
    , _rAttributes = Nothing
    , _rType = Nothing
    }


-- | Undocumented member.
rFeature :: Lens' Resource (Maybe Text)
rFeature = lens _rFeature (\ s a -> s{_rFeature = a})

-- | Undocumented member.
rArn :: Lens' Resource (Maybe Text)
rArn = lens _rArn (\ s a -> s{_rArn = a})

-- | Undocumented member.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a})

-- | Undocumented member.
rAttributes :: Lens' Resource (HashMap Text Text)
rAttributes = lens _rAttributes (\ s a -> s{_rAttributes = a}) . _Default . _Map

-- | Undocumented member.
rType :: Lens' Resource (Maybe Text)
rType = lens _rType (\ s a -> s{_rType = a})

instance FromJSON Resource where
        parseJSON
          = withObject "Resource"
              (\ x ->
                 Resource' <$>
                   (x .:? "feature") <*> (x .:? "arn") <*>
                     (x .:? "name")
                     <*> (x .:? "attributes" .!= mempty)
                     <*> (x .:? "type"))

instance Hashable Resource where

instance NFData Resource where
