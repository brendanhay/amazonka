{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application, optionally including an AWS SAM file to create the first application version in the same call.
--
--
module Network.AWS.ServerlessApplicationRepository.CreateApplication
    (
    -- * Creating a Request
      createApplication
    , CreateApplication
    -- * Request Lenses
    , caHomePageURL
    , caReadmeBody
    , caLicenseURL
    , caSemanticVersion
    , caSourceCodeURL
    , caReadmeURL
    , caName
    , caAuthor
    , caLabels
    , caTemplateBody
    , caTemplateURL
    , caLicenseBody
    , caDescription
    , caSpdxLicenseId

    -- * Destructuring the Response
    , createApplicationResponse
    , CreateApplicationResponse
    -- * Response Lenses
    , carsCreationTime
    , carsHomePageURL
    , carsLicenseURL
    , carsReadmeURL
    , carsApplicationId
    , carsName
    , carsVersion
    , carsAuthor
    , carsLabels
    , carsDescription
    , carsSpdxLicenseId
    , carsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.Types.Product

-- | /See:/ 'createApplication' smart constructor.
data CreateApplication = CreateApplication'
  { _caHomePageURL     :: !(Maybe Text)
  , _caReadmeBody      :: !(Maybe Text)
  , _caLicenseURL      :: !(Maybe Text)
  , _caSemanticVersion :: !(Maybe Text)
  , _caSourceCodeURL   :: !(Maybe Text)
  , _caReadmeURL       :: !(Maybe Text)
  , _caName            :: !(Maybe Text)
  , _caAuthor          :: !(Maybe Text)
  , _caLabels          :: !(Maybe [Text])
  , _caTemplateBody    :: !(Maybe Text)
  , _caTemplateURL     :: !(Maybe Text)
  , _caLicenseBody     :: !(Maybe Text)
  , _caDescription     :: !(Maybe Text)
  , _caSpdxLicenseId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caHomePageURL' - A URL with more information about the application, for example  the location of your GitHub repository for the application.
--
-- * 'caReadmeBody' - A raw text Readme file that contains a more detailed description of the application and how it works in markdown language. Max size 5 MB
--
-- * 'caLicenseURL' - A link to a license file of the app that matches the spdxLicenseID of your application. Max size 5 MB
--
-- * 'caSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
--
-- * 'caSourceCodeURL' - A link to a public repository for the source code of your application.
--
-- * 'caReadmeURL' - A link to the Readme file that contains a more detailed description of the application and how it works in markdown language. Max size 5 MB
--
-- * 'caName' - The name of the application you want to publish. Min Length=1. Max Length=140 Pattern: "[a-zA-Z0-9\\-]+";
--
-- * 'caAuthor' - The name of the author publishing the app. Min Length=1. Max Length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- * 'caLabels' - Labels to improve discovery of apps in search results. Min Length=1. Max Length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- * 'caTemplateBody' - The raw packaged AWS SAM template of your application.
--
-- * 'caTemplateURL' - A link to the packaged AWS SAM template of your application.
--
-- * 'caLicenseBody' - A raw text file that contains the license of the app that matches the spdxLicenseID of your application. Max size 5 MB
--
-- * 'caDescription' - The description of the application. Min Length=1. Max Length=256
--
-- * 'caSpdxLicenseId' - A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
createApplication
    :: CreateApplication
createApplication =
  CreateApplication'
    { _caHomePageURL = Nothing
    , _caReadmeBody = Nothing
    , _caLicenseURL = Nothing
    , _caSemanticVersion = Nothing
    , _caSourceCodeURL = Nothing
    , _caReadmeURL = Nothing
    , _caName = Nothing
    , _caAuthor = Nothing
    , _caLabels = Nothing
    , _caTemplateBody = Nothing
    , _caTemplateURL = Nothing
    , _caLicenseBody = Nothing
    , _caDescription = Nothing
    , _caSpdxLicenseId = Nothing
    }


-- | A URL with more information about the application, for example  the location of your GitHub repository for the application.
caHomePageURL :: Lens' CreateApplication (Maybe Text)
caHomePageURL = lens _caHomePageURL (\ s a -> s{_caHomePageURL = a})

-- | A raw text Readme file that contains a more detailed description of the application and how it works in markdown language. Max size 5 MB
caReadmeBody :: Lens' CreateApplication (Maybe Text)
caReadmeBody = lens _caReadmeBody (\ s a -> s{_caReadmeBody = a})

-- | A link to a license file of the app that matches the spdxLicenseID of your application. Max size 5 MB
caLicenseURL :: Lens' CreateApplication (Maybe Text)
caLicenseURL = lens _caLicenseURL (\ s a -> s{_caLicenseURL = a})

-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
caSemanticVersion :: Lens' CreateApplication (Maybe Text)
caSemanticVersion = lens _caSemanticVersion (\ s a -> s{_caSemanticVersion = a})

-- | A link to a public repository for the source code of your application.
caSourceCodeURL :: Lens' CreateApplication (Maybe Text)
caSourceCodeURL = lens _caSourceCodeURL (\ s a -> s{_caSourceCodeURL = a})

-- | A link to the Readme file that contains a more detailed description of the application and how it works in markdown language. Max size 5 MB
caReadmeURL :: Lens' CreateApplication (Maybe Text)
caReadmeURL = lens _caReadmeURL (\ s a -> s{_caReadmeURL = a})

-- | The name of the application you want to publish. Min Length=1. Max Length=140 Pattern: "[a-zA-Z0-9\\-]+";
caName :: Lens' CreateApplication (Maybe Text)
caName = lens _caName (\ s a -> s{_caName = a})

-- | The name of the author publishing the app. Min Length=1. Max Length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
caAuthor :: Lens' CreateApplication (Maybe Text)
caAuthor = lens _caAuthor (\ s a -> s{_caAuthor = a})

-- | Labels to improve discovery of apps in search results. Min Length=1. Max Length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
caLabels :: Lens' CreateApplication [Text]
caLabels = lens _caLabels (\ s a -> s{_caLabels = a}) . _Default . _Coerce

-- | The raw packaged AWS SAM template of your application.
caTemplateBody :: Lens' CreateApplication (Maybe Text)
caTemplateBody = lens _caTemplateBody (\ s a -> s{_caTemplateBody = a})

-- | A link to the packaged AWS SAM template of your application.
caTemplateURL :: Lens' CreateApplication (Maybe Text)
caTemplateURL = lens _caTemplateURL (\ s a -> s{_caTemplateURL = a})

-- | A raw text file that contains the license of the app that matches the spdxLicenseID of your application. Max size 5 MB
caLicenseBody :: Lens' CreateApplication (Maybe Text)
caLicenseBody = lens _caLicenseBody (\ s a -> s{_caLicenseBody = a})

-- | The description of the application. Min Length=1. Max Length=256
caDescription :: Lens' CreateApplication (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
caSpdxLicenseId :: Lens' CreateApplication (Maybe Text)
caSpdxLicenseId = lens _caSpdxLicenseId (\ s a -> s{_caSpdxLicenseId = a})

instance AWSRequest CreateApplication where
        type Rs CreateApplication = CreateApplicationResponse
        request = postJSON serverlessApplicationRepository
        response
          = receiveJSON
              (\ s h x ->
                 CreateApplicationResponse' <$>
                   (x .?> "creationTime") <*> (x .?> "homePageUrl") <*>
                     (x .?> "licenseUrl")
                     <*> (x .?> "readmeUrl")
                     <*> (x .?> "applicationId")
                     <*> (x .?> "name")
                     <*> (x .?> "version")
                     <*> (x .?> "author")
                     <*> (x .?> "labels" .!@ mempty)
                     <*> (x .?> "description")
                     <*> (x .?> "spdxLicenseId")
                     <*> (pure (fromEnum s)))

instance Hashable CreateApplication where

instance NFData CreateApplication where

instance ToHeaders CreateApplication where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateApplication where
        toJSON CreateApplication'{..}
          = object
              (catMaybes
                 [("homePageUrl" .=) <$> _caHomePageURL,
                  ("readmeBody" .=) <$> _caReadmeBody,
                  ("licenseUrl" .=) <$> _caLicenseURL,
                  ("semanticVersion" .=) <$> _caSemanticVersion,
                  ("sourceCodeUrl" .=) <$> _caSourceCodeURL,
                  ("readmeUrl" .=) <$> _caReadmeURL,
                  ("name" .=) <$> _caName, ("author" .=) <$> _caAuthor,
                  ("labels" .=) <$> _caLabels,
                  ("templateBody" .=) <$> _caTemplateBody,
                  ("templateUrl" .=) <$> _caTemplateURL,
                  ("licenseBody" .=) <$> _caLicenseBody,
                  ("description" .=) <$> _caDescription,
                  ("spdxLicenseId" .=) <$> _caSpdxLicenseId])

instance ToPath CreateApplication where
        toPath = const "/applications"

instance ToQuery CreateApplication where
        toQuery = const mempty

-- | /See:/ 'createApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { _carsCreationTime   :: !(Maybe Text)
  , _carsHomePageURL    :: !(Maybe Text)
  , _carsLicenseURL     :: !(Maybe Text)
  , _carsReadmeURL      :: !(Maybe Text)
  , _carsApplicationId  :: !(Maybe Text)
  , _carsName           :: !(Maybe Text)
  , _carsVersion        :: !(Maybe Version)
  , _carsAuthor         :: !(Maybe Text)
  , _carsLabels         :: !(Maybe [Text])
  , _carsDescription    :: !(Maybe Text)
  , _carsSpdxLicenseId  :: !(Maybe Text)
  , _carsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsCreationTime' - The date/time this resource was created.
--
-- * 'carsHomePageURL' - A URL with more information about the application, for example  the location of your GitHub repository for the application.
--
-- * 'carsLicenseURL' - A link to a license file of the app that matches the spdxLicenseID of your application. Max size 5 MB
--
-- * 'carsReadmeURL' - A link to the readme file that contains a more detailed description of the application and how it works in Markdown language. Max size 5 MB
--
-- * 'carsApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'carsName' - The name of the application. Min Length=1. Max Length=140 Pattern: "[a-zA-Z0-9\\-]+";
--
-- * 'carsVersion' - Version information about the application.
--
-- * 'carsAuthor' - The name of the author publishing the app. Min Length=1. Max Length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- * 'carsLabels' - Labels to improve discovery of apps in search results. Min Length=1. Max Length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- * 'carsDescription' - The description of the application. Min Length=1. Max Length=256
--
-- * 'carsSpdxLicenseId' - A valid identifier from https://spdx.org/licenses/.
--
-- * 'carsResponseStatus' - -- | The response status code.
createApplicationResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateApplicationResponse
createApplicationResponse pResponseStatus_ =
  CreateApplicationResponse'
    { _carsCreationTime = Nothing
    , _carsHomePageURL = Nothing
    , _carsLicenseURL = Nothing
    , _carsReadmeURL = Nothing
    , _carsApplicationId = Nothing
    , _carsName = Nothing
    , _carsVersion = Nothing
    , _carsAuthor = Nothing
    , _carsLabels = Nothing
    , _carsDescription = Nothing
    , _carsSpdxLicenseId = Nothing
    , _carsResponseStatus = pResponseStatus_
    }


-- | The date/time this resource was created.
carsCreationTime :: Lens' CreateApplicationResponse (Maybe Text)
carsCreationTime = lens _carsCreationTime (\ s a -> s{_carsCreationTime = a})

-- | A URL with more information about the application, for example  the location of your GitHub repository for the application.
carsHomePageURL :: Lens' CreateApplicationResponse (Maybe Text)
carsHomePageURL = lens _carsHomePageURL (\ s a -> s{_carsHomePageURL = a})

-- | A link to a license file of the app that matches the spdxLicenseID of your application. Max size 5 MB
carsLicenseURL :: Lens' CreateApplicationResponse (Maybe Text)
carsLicenseURL = lens _carsLicenseURL (\ s a -> s{_carsLicenseURL = a})

-- | A link to the readme file that contains a more detailed description of the application and how it works in Markdown language. Max size 5 MB
carsReadmeURL :: Lens' CreateApplicationResponse (Maybe Text)
carsReadmeURL = lens _carsReadmeURL (\ s a -> s{_carsReadmeURL = a})

-- | The application Amazon Resource Name (ARN).
carsApplicationId :: Lens' CreateApplicationResponse (Maybe Text)
carsApplicationId = lens _carsApplicationId (\ s a -> s{_carsApplicationId = a})

-- | The name of the application. Min Length=1. Max Length=140 Pattern: "[a-zA-Z0-9\\-]+";
carsName :: Lens' CreateApplicationResponse (Maybe Text)
carsName = lens _carsName (\ s a -> s{_carsName = a})

-- | Version information about the application.
carsVersion :: Lens' CreateApplicationResponse (Maybe Version)
carsVersion = lens _carsVersion (\ s a -> s{_carsVersion = a})

-- | The name of the author publishing the app. Min Length=1. Max Length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
carsAuthor :: Lens' CreateApplicationResponse (Maybe Text)
carsAuthor = lens _carsAuthor (\ s a -> s{_carsAuthor = a})

-- | Labels to improve discovery of apps in search results. Min Length=1. Max Length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
carsLabels :: Lens' CreateApplicationResponse [Text]
carsLabels = lens _carsLabels (\ s a -> s{_carsLabels = a}) . _Default . _Coerce

-- | The description of the application. Min Length=1. Max Length=256
carsDescription :: Lens' CreateApplicationResponse (Maybe Text)
carsDescription = lens _carsDescription (\ s a -> s{_carsDescription = a})

-- | A valid identifier from https://spdx.org/licenses/.
carsSpdxLicenseId :: Lens' CreateApplicationResponse (Maybe Text)
carsSpdxLicenseId = lens _carsSpdxLicenseId (\ s a -> s{_carsSpdxLicenseId = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateApplicationResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CreateApplicationResponse where
