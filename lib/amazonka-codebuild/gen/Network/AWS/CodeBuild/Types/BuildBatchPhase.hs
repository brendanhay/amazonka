{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchPhase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatchPhase where

import Network.AWS.CodeBuild.Types.BuildBatchPhaseType
import Network.AWS.CodeBuild.Types.PhaseContext
import Network.AWS.CodeBuild.Types.StatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a stage for a batch build.
--
--
--
-- /See:/ 'buildBatchPhase' smart constructor.
data BuildBatchPhase = BuildBatchPhase'
  { _bbpContexts ::
      !(Maybe [PhaseContext]),
    _bbpStartTime :: !(Maybe POSIX),
    _bbpPhaseStatus :: !(Maybe StatusType),
    _bbpPhaseType :: !(Maybe BuildBatchPhaseType),
    _bbpEndTime :: !(Maybe POSIX),
    _bbpDurationInSeconds :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuildBatchPhase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bbpContexts' - Additional information about the batch build phase. Especially to help troubleshoot a failed btach build.
--
-- * 'bbpStartTime' - When the batch build phase started, expressed in Unix time format.
--
-- * 'bbpPhaseStatus' - The current status of the batch build phase. Valid values include:     * FAILED    * The build phase failed.     * FAULT    * The build phase faulted.     * IN_PROGRESS    * The build phase is still in progress.     * QUEUED    * The build has been submitted and is queued behind other submitted builds.     * STOPPED    * The build phase stopped.     * SUCCEEDED    * The build phase succeeded.     * TIMED_OUT    * The build phase timed out.
--
-- * 'bbpPhaseType' - The name of the batch build phase. Valid values include:     * COMBINE_ARTIFACTS    * Build output artifacts are being combined and uploaded to the output location.     * DOWNLOAD_BATCHSPEC    * The batch build specification is being downloaded.     * FAILED    * One or more of the builds failed.     * IN_PROGRESS    * The batch build is in progress.     * STOPPED    * The batch build was stopped.     * SUBMITTED    * The btach build has been submitted.     * SUCCEEDED    * The batch build succeeded.
--
-- * 'bbpEndTime' - When the batch build phase ended, expressed in Unix time format.
--
-- * 'bbpDurationInSeconds' - How long, in seconds, between the starting and ending times of the batch build's phase.
buildBatchPhase ::
  BuildBatchPhase
buildBatchPhase =
  BuildBatchPhase'
    { _bbpContexts = Nothing,
      _bbpStartTime = Nothing,
      _bbpPhaseStatus = Nothing,
      _bbpPhaseType = Nothing,
      _bbpEndTime = Nothing,
      _bbpDurationInSeconds = Nothing
    }

-- | Additional information about the batch build phase. Especially to help troubleshoot a failed btach build.
bbpContexts :: Lens' BuildBatchPhase [PhaseContext]
bbpContexts = lens _bbpContexts (\s a -> s {_bbpContexts = a}) . _Default . _Coerce

-- | When the batch build phase started, expressed in Unix time format.
bbpStartTime :: Lens' BuildBatchPhase (Maybe UTCTime)
bbpStartTime = lens _bbpStartTime (\s a -> s {_bbpStartTime = a}) . mapping _Time

-- | The current status of the batch build phase. Valid values include:     * FAILED    * The build phase failed.     * FAULT    * The build phase faulted.     * IN_PROGRESS    * The build phase is still in progress.     * QUEUED    * The build has been submitted and is queued behind other submitted builds.     * STOPPED    * The build phase stopped.     * SUCCEEDED    * The build phase succeeded.     * TIMED_OUT    * The build phase timed out.
bbpPhaseStatus :: Lens' BuildBatchPhase (Maybe StatusType)
bbpPhaseStatus = lens _bbpPhaseStatus (\s a -> s {_bbpPhaseStatus = a})

-- | The name of the batch build phase. Valid values include:     * COMBINE_ARTIFACTS    * Build output artifacts are being combined and uploaded to the output location.     * DOWNLOAD_BATCHSPEC    * The batch build specification is being downloaded.     * FAILED    * One or more of the builds failed.     * IN_PROGRESS    * The batch build is in progress.     * STOPPED    * The batch build was stopped.     * SUBMITTED    * The btach build has been submitted.     * SUCCEEDED    * The batch build succeeded.
bbpPhaseType :: Lens' BuildBatchPhase (Maybe BuildBatchPhaseType)
bbpPhaseType = lens _bbpPhaseType (\s a -> s {_bbpPhaseType = a})

-- | When the batch build phase ended, expressed in Unix time format.
bbpEndTime :: Lens' BuildBatchPhase (Maybe UTCTime)
bbpEndTime = lens _bbpEndTime (\s a -> s {_bbpEndTime = a}) . mapping _Time

-- | How long, in seconds, between the starting and ending times of the batch build's phase.
bbpDurationInSeconds :: Lens' BuildBatchPhase (Maybe Integer)
bbpDurationInSeconds = lens _bbpDurationInSeconds (\s a -> s {_bbpDurationInSeconds = a})

instance FromJSON BuildBatchPhase where
  parseJSON =
    withObject
      "BuildBatchPhase"
      ( \x ->
          BuildBatchPhase'
            <$> (x .:? "contexts" .!= mempty)
            <*> (x .:? "startTime")
            <*> (x .:? "phaseStatus")
            <*> (x .:? "phaseType")
            <*> (x .:? "endTime")
            <*> (x .:? "durationInSeconds")
      )

instance Hashable BuildBatchPhase

instance NFData BuildBatchPhase
