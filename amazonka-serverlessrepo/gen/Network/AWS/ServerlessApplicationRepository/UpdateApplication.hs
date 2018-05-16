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
-- Module      : Network.AWS.ServerlessApplicationRepository.UpdateApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
--
--
module Network.AWS.ServerlessApplicationRepository.UpdateApplication
    (
    -- * Creating a Request
      updateApplication
    , UpdateApplication
    -- * Request Lenses
    , uaHomePageURL
    , uaReadmeBody
    , uaReadmeURL
    , uaAuthor
    , uaLabels
    , uaDescription
    , uaApplicationId

    -- * Destructuring the Response
    , updateApplicationResponse
    , UpdateApplicationResponse
    -- * Response Lenses
    , uarsCreationTime
    , uarsHomePageURL
    , uarsLicenseURL
    , uarsReadmeURL
    , uarsApplicationId
    , uarsName
    , uarsVersion
    , uarsAuthor
    , uarsLabels
    , uarsDescription
    , uarsSpdxLicenseId
    , uarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.Types.Product

-- | /See:/ 'updateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { _uaHomePageURL   :: !(Maybe Text)
  , _uaReadmeBody    :: !(Maybe Text)
  , _uaReadmeURL     :: !(Maybe Text)
  , _uaAuthor        :: !(Maybe Text)
  , _uaLabels        :: !(Maybe [Text])
  , _uaDescription   :: !(Maybe Text)
  , _uaApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaHomePageURL' - A URL with more information about the application, for example  the location of your GitHub repository for the application.
--
-- * 'uaReadmeBody' - A raw text Readme file that contains a more detailed description of the application and how it works in markdown language. Max size 5 MB
--
-- * 'uaReadmeURL' - A link to the Readme file that contains a more detailed description of the application and how it works in markdown language. Max size 5 MB
--
-- * 'uaAuthor' - The name of the author publishing the app. Min Length=1. Max Length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- * 'uaLabels' - Labels to improve discovery of apps in search results. Min Length=1. Max Length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- * 'uaDescription' - The description of the application. Min Length=1. Max Length=256
--
-- * 'uaApplicationId' - The ID of the application to get.
updateApplication
    :: Text -- ^ 'uaApplicationId'
    -> UpdateApplication
updateApplication pApplicationId_ =
  UpdateApplication'
    { _uaHomePageURL = Nothing
    , _uaReadmeBody = Nothing
    , _uaReadmeURL = Nothing
    , _uaAuthor = Nothing
    , _uaLabels = Nothing
    , _uaDescription = Nothing
    , _uaApplicationId = pApplicationId_
    }


-- | A URL with more information about the application, for example  the location of your GitHub repository for the application.
uaHomePageURL :: Lens' UpdateApplication (Maybe Text)
uaHomePageURL = lens _uaHomePageURL (\ s a -> s{_uaHomePageURL = a})

-- | A raw text Readme file that contains a more detailed description of the application and how it works in markdown language. Max size 5 MB
uaReadmeBody :: Lens' UpdateApplication (Maybe Text)
uaReadmeBody = lens _uaReadmeBody (\ s a -> s{_uaReadmeBody = a})

-- | A link to the Readme file that contains a more detailed description of the application and how it works in markdown language. Max size 5 MB
uaReadmeURL :: Lens' UpdateApplication (Maybe Text)
uaReadmeURL = lens _uaReadmeURL (\ s a -> s{_uaReadmeURL = a})

-- | The name of the author publishing the app. Min Length=1. Max Length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
uaAuthor :: Lens' UpdateApplication (Maybe Text)
uaAuthor = lens _uaAuthor (\ s a -> s{_uaAuthor = a})

-- | Labels to improve discovery of apps in search results. Min Length=1. Max Length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
uaLabels :: Lens' UpdateApplication [Text]
uaLabels = lens _uaLabels (\ s a -> s{_uaLabels = a}) . _Default . _Coerce

-- | The description of the application. Min Length=1. Max Length=256
uaDescription :: Lens' UpdateApplication (Maybe Text)
uaDescription = lens _uaDescription (\ s a -> s{_uaDescription = a})

-- | The ID of the application to get.
uaApplicationId :: Lens' UpdateApplication Text
uaApplicationId = lens _uaApplicationId (\ s a -> s{_uaApplicationId = a})

instance AWSRequest UpdateApplication where
        type Rs UpdateApplication = UpdateApplicationResponse
        request = patchJSON serverlessApplicationRepository
        response
          = receiveJSON
              (\ s h x ->
                 UpdateApplicationResponse' <$>
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

instance Hashable UpdateApplication where

instance NFData UpdateApplication where

instance ToHeaders UpdateApplication where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateApplication where
        toJSON UpdateApplication'{..}
          = object
              (catMaybes
                 [("homePageUrl" .=) <$> _uaHomePageURL,
                  ("readmeBody" .=) <$> _uaReadmeBody,
                  ("readmeUrl" .=) <$> _uaReadmeURL,
                  ("author" .=) <$> _uaAuthor,
                  ("labels" .=) <$> _uaLabels,
                  ("description" .=) <$> _uaDescription])

instance ToPath UpdateApplication where
        toPath UpdateApplication'{..}
          = mconcat ["/applications/", toBS _uaApplicationId]

instance ToQuery UpdateApplication where
        toQuery = const mempty

-- | /See:/ 'updateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { _uarsCreationTime   :: !(Maybe Text)
  , _uarsHomePageURL    :: !(Maybe Text)
  , _uarsLicenseURL     :: !(Maybe Text)
  , _uarsReadmeURL      :: !(Maybe Text)
  , _uarsApplicationId  :: !(Maybe Text)
  , _uarsName           :: !(Maybe Text)
  , _uarsVersion        :: !(Maybe Version)
  , _uarsAuthor         :: !(Maybe Text)
  , _uarsLabels         :: !(Maybe [Text])
  , _uarsDescription    :: !(Maybe Text)
  , _uarsSpdxLicenseId  :: !(Maybe Text)
  , _uarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsCreationTime' - The date/time this resource was created.
--
-- * 'uarsHomePageURL' - A URL with more information about the application, for example  the location of your GitHub repository for the application.
--
-- * 'uarsLicenseURL' - A link to a license file of the app that matches the spdxLicenseID of your application. Max size 5 MB
--
-- * 'uarsReadmeURL' - A link to the readme file that contains a more detailed description of the application and how it works in Markdown language. Max size 5 MB
--
-- * 'uarsApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'uarsName' - The name of the application. Min Length=1. Max Length=140 Pattern: "[a-zA-Z0-9\\-]+";
--
-- * 'uarsVersion' - Version information about the application.
--
-- * 'uarsAuthor' - The name of the author publishing the app. Min Length=1. Max Length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- * 'uarsLabels' - Labels to improve discovery of apps in search results. Min Length=1. Max Length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- * 'uarsDescription' - The description of the application. Min Length=1. Max Length=256
--
-- * 'uarsSpdxLicenseId' - A valid identifier from https://spdx.org/licenses/.
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateApplicationResponse
    :: Int -- ^ 'uarsResponseStatus'
    -> UpdateApplicationResponse
updateApplicationResponse pResponseStatus_ =
  UpdateApplicationResponse'
    { _uarsCreationTime = Nothing
    , _uarsHomePageURL = Nothing
    , _uarsLicenseURL = Nothing
    , _uarsReadmeURL = Nothing
    , _uarsApplicationId = Nothing
    , _uarsName = Nothing
    , _uarsVersion = Nothing
    , _uarsAuthor = Nothing
    , _uarsLabels = Nothing
    , _uarsDescription = Nothing
    , _uarsSpdxLicenseId = Nothing
    , _uarsResponseStatus = pResponseStatus_
    }


-- | The date/time this resource was created.
uarsCreationTime :: Lens' UpdateApplicationResponse (Maybe Text)
uarsCreationTime = lens _uarsCreationTime (\ s a -> s{_uarsCreationTime = a})

-- | A URL with more information about the application, for example  the location of your GitHub repository for the application.
uarsHomePageURL :: Lens' UpdateApplicationResponse (Maybe Text)
uarsHomePageURL = lens _uarsHomePageURL (\ s a -> s{_uarsHomePageURL = a})

-- | A link to a license file of the app that matches the spdxLicenseID of your application. Max size 5 MB
uarsLicenseURL :: Lens' UpdateApplicationResponse (Maybe Text)
uarsLicenseURL = lens _uarsLicenseURL (\ s a -> s{_uarsLicenseURL = a})

-- | A link to the readme file that contains a more detailed description of the application and how it works in Markdown language. Max size 5 MB
uarsReadmeURL :: Lens' UpdateApplicationResponse (Maybe Text)
uarsReadmeURL = lens _uarsReadmeURL (\ s a -> s{_uarsReadmeURL = a})

-- | The application Amazon Resource Name (ARN).
uarsApplicationId :: Lens' UpdateApplicationResponse (Maybe Text)
uarsApplicationId = lens _uarsApplicationId (\ s a -> s{_uarsApplicationId = a})

-- | The name of the application. Min Length=1. Max Length=140 Pattern: "[a-zA-Z0-9\\-]+";
uarsName :: Lens' UpdateApplicationResponse (Maybe Text)
uarsName = lens _uarsName (\ s a -> s{_uarsName = a})

-- | Version information about the application.
uarsVersion :: Lens' UpdateApplicationResponse (Maybe Version)
uarsVersion = lens _uarsVersion (\ s a -> s{_uarsVersion = a})

-- | The name of the author publishing the app. Min Length=1. Max Length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
uarsAuthor :: Lens' UpdateApplicationResponse (Maybe Text)
uarsAuthor = lens _uarsAuthor (\ s a -> s{_uarsAuthor = a})

-- | Labels to improve discovery of apps in search results. Min Length=1. Max Length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
uarsLabels :: Lens' UpdateApplicationResponse [Text]
uarsLabels = lens _uarsLabels (\ s a -> s{_uarsLabels = a}) . _Default . _Coerce

-- | The description of the application. Min Length=1. Max Length=256
uarsDescription :: Lens' UpdateApplicationResponse (Maybe Text)
uarsDescription = lens _uarsDescription (\ s a -> s{_uarsDescription = a})

-- | A valid identifier from https://spdx.org/licenses/.
uarsSpdxLicenseId :: Lens' UpdateApplicationResponse (Maybe Text)
uarsSpdxLicenseId = lens _uarsSpdxLicenseId (\ s a -> s{_uarsSpdxLicenseId = a})

-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateApplicationResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\ s a -> s{_uarsResponseStatus = a})

instance NFData UpdateApplicationResponse where
