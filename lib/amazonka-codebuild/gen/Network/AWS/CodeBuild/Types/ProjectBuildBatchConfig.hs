{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig where

import Network.AWS.CodeBuild.Types.BatchRestrictions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains configuration information about a batch build project.
--
--
--
-- /See:/ 'projectBuildBatchConfig' smart constructor.
data ProjectBuildBatchConfig = ProjectBuildBatchConfig'
  { _pbbcCombineArtifacts ::
      !(Maybe Bool),
    _pbbcTimeoutInMins :: !(Maybe Int),
    _pbbcRestrictions ::
      !(Maybe BatchRestrictions),
    _pbbcServiceRole :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectBuildBatchConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbbcCombineArtifacts' - Specifies if the build artifacts for the batch build should be combined into a single artifact location.
--
-- * 'pbbcTimeoutInMins' - Specifies the maximum amount of time, in minutes, that the batch build must be completed in.
--
-- * 'pbbcRestrictions' - A @BatchRestrictions@ object that specifies the restrictions for the batch build.
--
-- * 'pbbcServiceRole' - Specifies the service role ARN for the batch build project.
projectBuildBatchConfig ::
  ProjectBuildBatchConfig
projectBuildBatchConfig =
  ProjectBuildBatchConfig'
    { _pbbcCombineArtifacts = Nothing,
      _pbbcTimeoutInMins = Nothing,
      _pbbcRestrictions = Nothing,
      _pbbcServiceRole = Nothing
    }

-- | Specifies if the build artifacts for the batch build should be combined into a single artifact location.
pbbcCombineArtifacts :: Lens' ProjectBuildBatchConfig (Maybe Bool)
pbbcCombineArtifacts = lens _pbbcCombineArtifacts (\s a -> s {_pbbcCombineArtifacts = a})

-- | Specifies the maximum amount of time, in minutes, that the batch build must be completed in.
pbbcTimeoutInMins :: Lens' ProjectBuildBatchConfig (Maybe Int)
pbbcTimeoutInMins = lens _pbbcTimeoutInMins (\s a -> s {_pbbcTimeoutInMins = a})

-- | A @BatchRestrictions@ object that specifies the restrictions for the batch build.
pbbcRestrictions :: Lens' ProjectBuildBatchConfig (Maybe BatchRestrictions)
pbbcRestrictions = lens _pbbcRestrictions (\s a -> s {_pbbcRestrictions = a})

-- | Specifies the service role ARN for the batch build project.
pbbcServiceRole :: Lens' ProjectBuildBatchConfig (Maybe Text)
pbbcServiceRole = lens _pbbcServiceRole (\s a -> s {_pbbcServiceRole = a})

instance FromJSON ProjectBuildBatchConfig where
  parseJSON =
    withObject
      "ProjectBuildBatchConfig"
      ( \x ->
          ProjectBuildBatchConfig'
            <$> (x .:? "combineArtifacts")
            <*> (x .:? "timeoutInMins")
            <*> (x .:? "restrictions")
            <*> (x .:? "serviceRole")
      )

instance Hashable ProjectBuildBatchConfig

instance NFData ProjectBuildBatchConfig

instance ToJSON ProjectBuildBatchConfig where
  toJSON ProjectBuildBatchConfig' {..} =
    object
      ( catMaybes
          [ ("combineArtifacts" .=) <$> _pbbcCombineArtifacts,
            ("timeoutInMins" .=) <$> _pbbcTimeoutInMins,
            ("restrictions" .=) <$> _pbbcRestrictions,
            ("serviceRole" .=) <$> _pbbcServiceRole
          ]
      )
