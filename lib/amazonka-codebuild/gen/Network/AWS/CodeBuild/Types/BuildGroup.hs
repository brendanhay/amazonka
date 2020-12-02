{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildGroup where

import Network.AWS.CodeBuild.Types.BuildSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a batch build build group. Build groups are used to combine builds that can run in parallel, while still being able to set dependencies on other build groups.
--
--
--
-- /See:/ 'buildGroup' smart constructor.
data BuildGroup = BuildGroup'
  { _bgIdentifier :: !(Maybe Text),
    _bgDependsOn :: !(Maybe [Text]),
    _bgIgnoreFailure :: !(Maybe Bool),
    _bgCurrentBuildSummary :: !(Maybe BuildSummary),
    _bgPriorBuildSummaryList :: !(Maybe [BuildSummary])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuildGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgIdentifier' - Contains the identifier of the build group.
--
-- * 'bgDependsOn' - An array of strings that contain the identifiers of the build groups that this build group depends on.
--
-- * 'bgIgnoreFailure' - Specifies if failures in this build group can be ignored.
--
-- * 'bgCurrentBuildSummary' - A @BuildSummary@ object that contains a summary of the current build group.
--
-- * 'bgPriorBuildSummaryList' - An array of @BuildSummary@ objects that contain summaries of previous build groups.
buildGroup ::
  BuildGroup
buildGroup =
  BuildGroup'
    { _bgIdentifier = Nothing,
      _bgDependsOn = Nothing,
      _bgIgnoreFailure = Nothing,
      _bgCurrentBuildSummary = Nothing,
      _bgPriorBuildSummaryList = Nothing
    }

-- | Contains the identifier of the build group.
bgIdentifier :: Lens' BuildGroup (Maybe Text)
bgIdentifier = lens _bgIdentifier (\s a -> s {_bgIdentifier = a})

-- | An array of strings that contain the identifiers of the build groups that this build group depends on.
bgDependsOn :: Lens' BuildGroup [Text]
bgDependsOn = lens _bgDependsOn (\s a -> s {_bgDependsOn = a}) . _Default . _Coerce

-- | Specifies if failures in this build group can be ignored.
bgIgnoreFailure :: Lens' BuildGroup (Maybe Bool)
bgIgnoreFailure = lens _bgIgnoreFailure (\s a -> s {_bgIgnoreFailure = a})

-- | A @BuildSummary@ object that contains a summary of the current build group.
bgCurrentBuildSummary :: Lens' BuildGroup (Maybe BuildSummary)
bgCurrentBuildSummary = lens _bgCurrentBuildSummary (\s a -> s {_bgCurrentBuildSummary = a})

-- | An array of @BuildSummary@ objects that contain summaries of previous build groups.
bgPriorBuildSummaryList :: Lens' BuildGroup [BuildSummary]
bgPriorBuildSummaryList = lens _bgPriorBuildSummaryList (\s a -> s {_bgPriorBuildSummaryList = a}) . _Default . _Coerce

instance FromJSON BuildGroup where
  parseJSON =
    withObject
      "BuildGroup"
      ( \x ->
          BuildGroup'
            <$> (x .:? "identifier")
            <*> (x .:? "dependsOn" .!= mempty)
            <*> (x .:? "ignoreFailure")
            <*> (x .:? "currentBuildSummary")
            <*> (x .:? "priorBuildSummaryList" .!= mempty)
      )

instance Hashable BuildGroup

instance NFData BuildGroup
