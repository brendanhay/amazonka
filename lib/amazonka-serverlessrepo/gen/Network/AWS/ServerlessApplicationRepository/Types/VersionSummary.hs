{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.VersionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.VersionSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An application version summary.
--
--
--
-- /See:/ 'versionSummary' smart constructor.
data VersionSummary = VersionSummary'
  { _vsSourceCodeURL ::
      !(Maybe Text),
    _vsCreationTime :: !Text,
    _vsApplicationId :: !Text,
    _vsSemanticVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VersionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsSourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- * 'vsCreationTime' - The date and time this resource was created.
--
-- * 'vsApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'vsSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
versionSummary ::
  -- | 'vsCreationTime'
  Text ->
  -- | 'vsApplicationId'
  Text ->
  -- | 'vsSemanticVersion'
  Text ->
  VersionSummary
versionSummary pCreationTime_ pApplicationId_ pSemanticVersion_ =
  VersionSummary'
    { _vsSourceCodeURL = Nothing,
      _vsCreationTime = pCreationTime_,
      _vsApplicationId = pApplicationId_,
      _vsSemanticVersion = pSemanticVersion_
    }

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
vsSourceCodeURL :: Lens' VersionSummary (Maybe Text)
vsSourceCodeURL = lens _vsSourceCodeURL (\s a -> s {_vsSourceCodeURL = a})

-- | The date and time this resource was created.
vsCreationTime :: Lens' VersionSummary Text
vsCreationTime = lens _vsCreationTime (\s a -> s {_vsCreationTime = a})

-- | The application Amazon Resource Name (ARN).
vsApplicationId :: Lens' VersionSummary Text
vsApplicationId = lens _vsApplicationId (\s a -> s {_vsApplicationId = a})

-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
vsSemanticVersion :: Lens' VersionSummary Text
vsSemanticVersion = lens _vsSemanticVersion (\s a -> s {_vsSemanticVersion = a})

instance FromJSON VersionSummary where
  parseJSON =
    withObject
      "VersionSummary"
      ( \x ->
          VersionSummary'
            <$> (x .:? "sourceCodeUrl")
            <*> (x .: "creationTime")
            <*> (x .: "applicationId")
            <*> (x .: "semanticVersion")
      )

instance Hashable VersionSummary

instance NFData VersionSummary
