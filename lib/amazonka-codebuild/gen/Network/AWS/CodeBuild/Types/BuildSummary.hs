{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildSummary where

import Network.AWS.CodeBuild.Types.ResolvedArtifact
import Network.AWS.CodeBuild.Types.StatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains summary information about a batch build group.
--
--
--
-- /See:/ 'buildSummary' smart constructor.
data BuildSummary = BuildSummary'
  { _bsSecondaryArtifacts ::
      !(Maybe [ResolvedArtifact]),
    _bsPrimaryArtifact :: !(Maybe ResolvedArtifact),
    _bsArn :: !(Maybe Text),
    _bsBuildStatus :: !(Maybe StatusType),
    _bsRequestedOn :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuildSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsSecondaryArtifacts' - An array of @ResolvedArtifact@ objects that represents the secondary build artifacts for the build group.
--
-- * 'bsPrimaryArtifact' - A @ResolvedArtifact@ object that represents the primary build artifacts for the build group.
--
-- * 'bsArn' - The batch build ARN.
--
-- * 'bsBuildStatus' - The status of the build group.     * FAILED    * The build group failed.     * FAULT    * The build group faulted.     * IN_PROGRESS    * The build group is still in progress.     * STOPPED    * The build group stopped.     * SUCCEEDED    * The build group succeeded.     * TIMED_OUT    * The build group timed out.
--
-- * 'bsRequestedOn' - When the build was started, expressed in Unix time format.
buildSummary ::
  BuildSummary
buildSummary =
  BuildSummary'
    { _bsSecondaryArtifacts = Nothing,
      _bsPrimaryArtifact = Nothing,
      _bsArn = Nothing,
      _bsBuildStatus = Nothing,
      _bsRequestedOn = Nothing
    }

-- | An array of @ResolvedArtifact@ objects that represents the secondary build artifacts for the build group.
bsSecondaryArtifacts :: Lens' BuildSummary [ResolvedArtifact]
bsSecondaryArtifacts = lens _bsSecondaryArtifacts (\s a -> s {_bsSecondaryArtifacts = a}) . _Default . _Coerce

-- | A @ResolvedArtifact@ object that represents the primary build artifacts for the build group.
bsPrimaryArtifact :: Lens' BuildSummary (Maybe ResolvedArtifact)
bsPrimaryArtifact = lens _bsPrimaryArtifact (\s a -> s {_bsPrimaryArtifact = a})

-- | The batch build ARN.
bsArn :: Lens' BuildSummary (Maybe Text)
bsArn = lens _bsArn (\s a -> s {_bsArn = a})

-- | The status of the build group.     * FAILED    * The build group failed.     * FAULT    * The build group faulted.     * IN_PROGRESS    * The build group is still in progress.     * STOPPED    * The build group stopped.     * SUCCEEDED    * The build group succeeded.     * TIMED_OUT    * The build group timed out.
bsBuildStatus :: Lens' BuildSummary (Maybe StatusType)
bsBuildStatus = lens _bsBuildStatus (\s a -> s {_bsBuildStatus = a})

-- | When the build was started, expressed in Unix time format.
bsRequestedOn :: Lens' BuildSummary (Maybe UTCTime)
bsRequestedOn = lens _bsRequestedOn (\s a -> s {_bsRequestedOn = a}) . mapping _Time

instance FromJSON BuildSummary where
  parseJSON =
    withObject
      "BuildSummary"
      ( \x ->
          BuildSummary'
            <$> (x .:? "secondaryArtifacts" .!= mempty)
            <*> (x .:? "primaryArtifact")
            <*> (x .:? "arn")
            <*> (x .:? "buildStatus")
            <*> (x .:? "requestedOn")
      )

instance Hashable BuildSummary

instance NFData BuildSummary
