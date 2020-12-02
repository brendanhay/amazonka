{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
module Network.AWS.ServerlessApplicationRepository.UpdateApplication
  ( -- * Creating a Request
    updateApplication,
    UpdateApplication,

    -- * Request Lenses
    uHomePageURL,
    uReadmeBody,
    uReadmeURL,
    uAuthor,
    uLabels,
    uDescription,
    uApplicationId,

    -- * Destructuring the Response
    updateApplicationResponse,
    UpdateApplicationResponse,

    -- * Response Lenses
    uarsCreationTime,
    uarsHomePageURL,
    uarsLicenseURL,
    uarsReadmeURL,
    uarsApplicationId,
    uarsName,
    uarsVersion,
    uarsAuthor,
    uarsLabels,
    uarsVerifiedAuthorURL,
    uarsDescription,
    uarsSpdxLicenseId,
    uarsIsVerifiedAuthor,
    uarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'updateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { _uHomePageURL ::
      !(Maybe Text),
    _uReadmeBody :: !(Maybe Text),
    _uReadmeURL :: !(Maybe Text),
    _uAuthor :: !(Maybe Text),
    _uLabels :: !(Maybe [Text]),
    _uDescription :: !(Maybe Text),
    _uApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uHomePageURL' - A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- * 'uReadmeBody' - A text readme file in Markdown language that contains a more detailed description of the application and how it works. Maximum size 5 MB
--
-- * 'uReadmeURL' - A link to the readme file in Markdown language that contains a more detailed description of the application and how it works. Maximum size 5 MB
--
-- * 'uAuthor' - The name of the author publishing the app. Minimum length=1. Maximum length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- * 'uLabels' - Labels to improve discovery of apps in search results. Minimum length=1. Maximum length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- * 'uDescription' - The description of the application. Minimum length=1. Maximum length=256
--
-- * 'uApplicationId' - The Amazon Resource Name (ARN) of the application.
updateApplication ::
  -- | 'uApplicationId'
  Text ->
  UpdateApplication
updateApplication pApplicationId_ =
  UpdateApplication'
    { _uHomePageURL = Nothing,
      _uReadmeBody = Nothing,
      _uReadmeURL = Nothing,
      _uAuthor = Nothing,
      _uLabels = Nothing,
      _uDescription = Nothing,
      _uApplicationId = pApplicationId_
    }

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
uHomePageURL :: Lens' UpdateApplication (Maybe Text)
uHomePageURL = lens _uHomePageURL (\s a -> s {_uHomePageURL = a})

-- | A text readme file in Markdown language that contains a more detailed description of the application and how it works. Maximum size 5 MB
uReadmeBody :: Lens' UpdateApplication (Maybe Text)
uReadmeBody = lens _uReadmeBody (\s a -> s {_uReadmeBody = a})

-- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works. Maximum size 5 MB
uReadmeURL :: Lens' UpdateApplication (Maybe Text)
uReadmeURL = lens _uReadmeURL (\s a -> s {_uReadmeURL = a})

-- | The name of the author publishing the app. Minimum length=1. Maximum length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
uAuthor :: Lens' UpdateApplication (Maybe Text)
uAuthor = lens _uAuthor (\s a -> s {_uAuthor = a})

-- | Labels to improve discovery of apps in search results. Minimum length=1. Maximum length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
uLabels :: Lens' UpdateApplication [Text]
uLabels = lens _uLabels (\s a -> s {_uLabels = a}) . _Default . _Coerce

-- | The description of the application. Minimum length=1. Maximum length=256
uDescription :: Lens' UpdateApplication (Maybe Text)
uDescription = lens _uDescription (\s a -> s {_uDescription = a})

-- | The Amazon Resource Name (ARN) of the application.
uApplicationId :: Lens' UpdateApplication Text
uApplicationId = lens _uApplicationId (\s a -> s {_uApplicationId = a})

instance AWSRequest UpdateApplication where
  type Rs UpdateApplication = UpdateApplicationResponse
  request = patchJSON serverlessApplicationRepository
  response =
    receiveJSON
      ( \s h x ->
          UpdateApplicationResponse'
            <$> (x .?> "creationTime")
            <*> (x .?> "homePageUrl")
            <*> (x .?> "licenseUrl")
            <*> (x .?> "readmeUrl")
            <*> (x .?> "applicationId")
            <*> (x .?> "name")
            <*> (x .?> "version")
            <*> (x .?> "author")
            <*> (x .?> "labels" .!@ mempty)
            <*> (x .?> "verifiedAuthorUrl")
            <*> (x .?> "description")
            <*> (x .?> "spdxLicenseId")
            <*> (x .?> "isVerifiedAuthor")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateApplication

instance NFData UpdateApplication

instance ToHeaders UpdateApplication where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    object
      ( catMaybes
          [ ("homePageUrl" .=) <$> _uHomePageURL,
            ("readmeBody" .=) <$> _uReadmeBody,
            ("readmeUrl" .=) <$> _uReadmeURL,
            ("author" .=) <$> _uAuthor,
            ("labels" .=) <$> _uLabels,
            ("description" .=) <$> _uDescription
          ]
      )

instance ToPath UpdateApplication where
  toPath UpdateApplication' {..} =
    mconcat ["/applications/", toBS _uApplicationId]

instance ToQuery UpdateApplication where
  toQuery = const mempty

-- | /See:/ 'updateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { _uarsCreationTime ::
      !(Maybe Text),
    _uarsHomePageURL :: !(Maybe Text),
    _uarsLicenseURL :: !(Maybe Text),
    _uarsReadmeURL :: !(Maybe Text),
    _uarsApplicationId :: !(Maybe Text),
    _uarsName :: !(Maybe Text),
    _uarsVersion :: !(Maybe Version),
    _uarsAuthor :: !(Maybe Text),
    _uarsLabels :: !(Maybe [Text]),
    _uarsVerifiedAuthorURL :: !(Maybe Text),
    _uarsDescription :: !(Maybe Text),
    _uarsSpdxLicenseId :: !(Maybe Text),
    _uarsIsVerifiedAuthor :: !(Maybe Bool),
    _uarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsCreationTime' - The date and time this resource was created.
--
-- * 'uarsHomePageURL' - A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- * 'uarsLicenseURL' - A link to a license file of the app that matches the spdxLicenseID value of your application. Maximum size 5 MB
--
-- * 'uarsReadmeURL' - A link to the readme file in Markdown language that contains a more detailed description of the application and how it works. Maximum size 5 MB
--
-- * 'uarsApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'uarsName' - The name of the application. Minimum length=1. Maximum length=140 Pattern: "[a-zA-Z0-9\\-]+";
--
-- * 'uarsVersion' - Version information about the application.
--
-- * 'uarsAuthor' - The name of the author publishing the app. Minimum length=1. Maximum length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- * 'uarsLabels' - Labels to improve discovery of apps in search results. Minimum length=1. Maximum length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- * 'uarsVerifiedAuthorURL' - The URL to the public profile of a verified author. This URL is submitted by the author.
--
-- * 'uarsDescription' - The description of the application. Minimum length=1. Maximum length=256
--
-- * 'uarsSpdxLicenseId' - A valid identifier from https://spdx.org/licenses/.
--
-- * 'uarsIsVerifiedAuthor' - Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateApplicationResponse ::
  -- | 'uarsResponseStatus'
  Int ->
  UpdateApplicationResponse
updateApplicationResponse pResponseStatus_ =
  UpdateApplicationResponse'
    { _uarsCreationTime = Nothing,
      _uarsHomePageURL = Nothing,
      _uarsLicenseURL = Nothing,
      _uarsReadmeURL = Nothing,
      _uarsApplicationId = Nothing,
      _uarsName = Nothing,
      _uarsVersion = Nothing,
      _uarsAuthor = Nothing,
      _uarsLabels = Nothing,
      _uarsVerifiedAuthorURL = Nothing,
      _uarsDescription = Nothing,
      _uarsSpdxLicenseId = Nothing,
      _uarsIsVerifiedAuthor = Nothing,
      _uarsResponseStatus = pResponseStatus_
    }

-- | The date and time this resource was created.
uarsCreationTime :: Lens' UpdateApplicationResponse (Maybe Text)
uarsCreationTime = lens _uarsCreationTime (\s a -> s {_uarsCreationTime = a})

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
uarsHomePageURL :: Lens' UpdateApplicationResponse (Maybe Text)
uarsHomePageURL = lens _uarsHomePageURL (\s a -> s {_uarsHomePageURL = a})

-- | A link to a license file of the app that matches the spdxLicenseID value of your application. Maximum size 5 MB
uarsLicenseURL :: Lens' UpdateApplicationResponse (Maybe Text)
uarsLicenseURL = lens _uarsLicenseURL (\s a -> s {_uarsLicenseURL = a})

-- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works. Maximum size 5 MB
uarsReadmeURL :: Lens' UpdateApplicationResponse (Maybe Text)
uarsReadmeURL = lens _uarsReadmeURL (\s a -> s {_uarsReadmeURL = a})

-- | The application Amazon Resource Name (ARN).
uarsApplicationId :: Lens' UpdateApplicationResponse (Maybe Text)
uarsApplicationId = lens _uarsApplicationId (\s a -> s {_uarsApplicationId = a})

-- | The name of the application. Minimum length=1. Maximum length=140 Pattern: "[a-zA-Z0-9\\-]+";
uarsName :: Lens' UpdateApplicationResponse (Maybe Text)
uarsName = lens _uarsName (\s a -> s {_uarsName = a})

-- | Version information about the application.
uarsVersion :: Lens' UpdateApplicationResponse (Maybe Version)
uarsVersion = lens _uarsVersion (\s a -> s {_uarsVersion = a})

-- | The name of the author publishing the app. Minimum length=1. Maximum length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
uarsAuthor :: Lens' UpdateApplicationResponse (Maybe Text)
uarsAuthor = lens _uarsAuthor (\s a -> s {_uarsAuthor = a})

-- | Labels to improve discovery of apps in search results. Minimum length=1. Maximum length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
uarsLabels :: Lens' UpdateApplicationResponse [Text]
uarsLabels = lens _uarsLabels (\s a -> s {_uarsLabels = a}) . _Default . _Coerce

-- | The URL to the public profile of a verified author. This URL is submitted by the author.
uarsVerifiedAuthorURL :: Lens' UpdateApplicationResponse (Maybe Text)
uarsVerifiedAuthorURL = lens _uarsVerifiedAuthorURL (\s a -> s {_uarsVerifiedAuthorURL = a})

-- | The description of the application. Minimum length=1. Maximum length=256
uarsDescription :: Lens' UpdateApplicationResponse (Maybe Text)
uarsDescription = lens _uarsDescription (\s a -> s {_uarsDescription = a})

-- | A valid identifier from https://spdx.org/licenses/.
uarsSpdxLicenseId :: Lens' UpdateApplicationResponse (Maybe Text)
uarsSpdxLicenseId = lens _uarsSpdxLicenseId (\s a -> s {_uarsSpdxLicenseId = a})

-- | Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
uarsIsVerifiedAuthor :: Lens' UpdateApplicationResponse (Maybe Bool)
uarsIsVerifiedAuthor = lens _uarsIsVerifiedAuthor (\s a -> s {_uarsIsVerifiedAuthor = a})

-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateApplicationResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\s a -> s {_uarsResponseStatus = a})

instance NFData UpdateApplicationResponse
