{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildPhase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildPhase where

import Network.AWS.CodeBuild.Types.BuildPhaseType
import Network.AWS.CodeBuild.Types.PhaseContext
import Network.AWS.CodeBuild.Types.StatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a stage for a build.
--
--
--
-- /See:/ 'buildPhase' smart constructor.
data BuildPhase = BuildPhase'
  { _bpContexts ::
      !(Maybe [PhaseContext]),
    _bpStartTime :: !(Maybe POSIX),
    _bpPhaseStatus :: !(Maybe StatusType),
    _bpPhaseType :: !(Maybe BuildPhaseType),
    _bpEndTime :: !(Maybe POSIX),
    _bpDurationInSeconds :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuildPhase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpContexts' - Additional information about a build phase, especially to help troubleshoot a failed build.
--
-- * 'bpStartTime' - When the build phase started, expressed in Unix time format.
--
-- * 'bpPhaseStatus' - The current status of the build phase. Valid values include:     * FAILED    * The build phase failed.     * FAULT    * The build phase faulted.     * IN_PROGRESS    * The build phase is still in progress.     * QUEUED    * The build has been submitted and is queued behind other submitted builds.     * STOPPED    * The build phase stopped.     * SUCCEEDED    * The build phase succeeded.     * TIMED_OUT    * The build phase timed out.
--
-- * 'bpPhaseType' - The name of the build phase. Valid values include:     * @BUILD@ : Core build activities typically occur in this build phase.     * @COMPLETED@ : The build has been completed.     * @DOWNLOAD_SOURCE@ : Source code is being downloaded in this build phase.     * @FINALIZING@ : The build process is completing in this build phase.     * @INSTALL@ : Installation activities typically occur in this build phase.     * @POST_BUILD@ : Post-build activities typically occur in this build phase.     * @PRE_BUILD@ : Pre-build activities typically occur in this build phase.     * @PROVISIONING@ : The build environment is being set up.     * @QUEUED@ : The build has been submitted and is queued behind other submitted builds.     * @SUBMITTED@ : The build has been submitted.     * @UPLOAD_ARTIFACTS@ : Build output artifacts are being uploaded to the output location.
--
-- * 'bpEndTime' - When the build phase ended, expressed in Unix time format.
--
-- * 'bpDurationInSeconds' - How long, in seconds, between the starting and ending times of the build's phase.
buildPhase ::
  BuildPhase
buildPhase =
  BuildPhase'
    { _bpContexts = Nothing,
      _bpStartTime = Nothing,
      _bpPhaseStatus = Nothing,
      _bpPhaseType = Nothing,
      _bpEndTime = Nothing,
      _bpDurationInSeconds = Nothing
    }

-- | Additional information about a build phase, especially to help troubleshoot a failed build.
bpContexts :: Lens' BuildPhase [PhaseContext]
bpContexts = lens _bpContexts (\s a -> s {_bpContexts = a}) . _Default . _Coerce

-- | When the build phase started, expressed in Unix time format.
bpStartTime :: Lens' BuildPhase (Maybe UTCTime)
bpStartTime = lens _bpStartTime (\s a -> s {_bpStartTime = a}) . mapping _Time

-- | The current status of the build phase. Valid values include:     * FAILED    * The build phase failed.     * FAULT    * The build phase faulted.     * IN_PROGRESS    * The build phase is still in progress.     * QUEUED    * The build has been submitted and is queued behind other submitted builds.     * STOPPED    * The build phase stopped.     * SUCCEEDED    * The build phase succeeded.     * TIMED_OUT    * The build phase timed out.
bpPhaseStatus :: Lens' BuildPhase (Maybe StatusType)
bpPhaseStatus = lens _bpPhaseStatus (\s a -> s {_bpPhaseStatus = a})

-- | The name of the build phase. Valid values include:     * @BUILD@ : Core build activities typically occur in this build phase.     * @COMPLETED@ : The build has been completed.     * @DOWNLOAD_SOURCE@ : Source code is being downloaded in this build phase.     * @FINALIZING@ : The build process is completing in this build phase.     * @INSTALL@ : Installation activities typically occur in this build phase.     * @POST_BUILD@ : Post-build activities typically occur in this build phase.     * @PRE_BUILD@ : Pre-build activities typically occur in this build phase.     * @PROVISIONING@ : The build environment is being set up.     * @QUEUED@ : The build has been submitted and is queued behind other submitted builds.     * @SUBMITTED@ : The build has been submitted.     * @UPLOAD_ARTIFACTS@ : Build output artifacts are being uploaded to the output location.
bpPhaseType :: Lens' BuildPhase (Maybe BuildPhaseType)
bpPhaseType = lens _bpPhaseType (\s a -> s {_bpPhaseType = a})

-- | When the build phase ended, expressed in Unix time format.
bpEndTime :: Lens' BuildPhase (Maybe UTCTime)
bpEndTime = lens _bpEndTime (\s a -> s {_bpEndTime = a}) . mapping _Time

-- | How long, in seconds, between the starting and ending times of the build's phase.
bpDurationInSeconds :: Lens' BuildPhase (Maybe Integer)
bpDurationInSeconds = lens _bpDurationInSeconds (\s a -> s {_bpDurationInSeconds = a})

instance FromJSON BuildPhase where
  parseJSON =
    withObject
      "BuildPhase"
      ( \x ->
          BuildPhase'
            <$> (x .:? "contexts" .!= mempty)
            <*> (x .:? "startTime")
            <*> (x .:? "phaseStatus")
            <*> (x .:? "phaseType")
            <*> (x .:? "endTime")
            <*> (x .:? "durationInSeconds")
      )

instance Hashable BuildPhase

instance NFData BuildPhase
