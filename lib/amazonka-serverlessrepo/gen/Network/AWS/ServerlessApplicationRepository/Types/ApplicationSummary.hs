{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ApplicationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ApplicationSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary of details about the application.
--
--
--
-- /See:/ 'applicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { _asCreationTime ::
      !(Maybe Text),
    _asHomePageURL :: !(Maybe Text),
    _asLabels :: !(Maybe [Text]),
    _asSpdxLicenseId :: !(Maybe Text),
    _asDescription :: !Text,
    _asAuthor :: !Text,
    _asApplicationId :: !Text,
    _asName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asCreationTime' - The date and time this resource was created.
--
-- * 'asHomePageURL' - A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- * 'asLabels' - Labels to improve discovery of apps in search results. Minimum length=1. Maximum length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- * 'asSpdxLicenseId' - A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
--
-- * 'asDescription' - The description of the application. Minimum length=1. Maximum length=256
--
-- * 'asAuthor' - The name of the author publishing the app. Minimum length=1. Maximum length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- * 'asApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'asName' - The name of the application. Minimum length=1. Maximum length=140 Pattern: "[a-zA-Z0-9\\-]+";
applicationSummary ::
  -- | 'asDescription'
  Text ->
  -- | 'asAuthor'
  Text ->
  -- | 'asApplicationId'
  Text ->
  -- | 'asName'
  Text ->
  ApplicationSummary
applicationSummary pDescription_ pAuthor_ pApplicationId_ pName_ =
  ApplicationSummary'
    { _asCreationTime = Nothing,
      _asHomePageURL = Nothing,
      _asLabels = Nothing,
      _asSpdxLicenseId = Nothing,
      _asDescription = pDescription_,
      _asAuthor = pAuthor_,
      _asApplicationId = pApplicationId_,
      _asName = pName_
    }

-- | The date and time this resource was created.
asCreationTime :: Lens' ApplicationSummary (Maybe Text)
asCreationTime = lens _asCreationTime (\s a -> s {_asCreationTime = a})

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
asHomePageURL :: Lens' ApplicationSummary (Maybe Text)
asHomePageURL = lens _asHomePageURL (\s a -> s {_asHomePageURL = a})

-- | Labels to improve discovery of apps in search results. Minimum length=1. Maximum length=127. Maximum number of labels: 10 Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
asLabels :: Lens' ApplicationSummary [Text]
asLabels = lens _asLabels (\s a -> s {_asLabels = a}) . _Default . _Coerce

-- | A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
asSpdxLicenseId :: Lens' ApplicationSummary (Maybe Text)
asSpdxLicenseId = lens _asSpdxLicenseId (\s a -> s {_asSpdxLicenseId = a})

-- | The description of the application. Minimum length=1. Maximum length=256
asDescription :: Lens' ApplicationSummary Text
asDescription = lens _asDescription (\s a -> s {_asDescription = a})

-- | The name of the author publishing the app. Minimum length=1. Maximum length=127. Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
asAuthor :: Lens' ApplicationSummary Text
asAuthor = lens _asAuthor (\s a -> s {_asAuthor = a})

-- | The application Amazon Resource Name (ARN).
asApplicationId :: Lens' ApplicationSummary Text
asApplicationId = lens _asApplicationId (\s a -> s {_asApplicationId = a})

-- | The name of the application. Minimum length=1. Maximum length=140 Pattern: "[a-zA-Z0-9\\-]+";
asName :: Lens' ApplicationSummary Text
asName = lens _asName (\s a -> s {_asName = a})

instance FromJSON ApplicationSummary where
  parseJSON =
    withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            <$> (x .:? "creationTime")
            <*> (x .:? "homePageUrl")
            <*> (x .:? "labels" .!= mempty)
            <*> (x .:? "spdxLicenseId")
            <*> (x .: "description")
            <*> (x .: "author")
            <*> (x .: "applicationId")
            <*> (x .: "name")
      )

instance Hashable ApplicationSummary

instance NFData ApplicationSummary
