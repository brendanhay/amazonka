{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EventsDetectionJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EventsDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about an events detection job.
--
-- /See:/ 'newEventsDetectionJobProperties' smart constructor.
data EventsDetectionJobProperties = EventsDetectionJobProperties'
  { -- | The language code of the input documents.
    languageCode :: Core.Maybe LanguageCode,
    -- | The input data configuration that you supplied when you created the
    -- events detection job.
    inputDataConfig :: Core.Maybe InputDataConfig,
    -- | A description of the status of the events detection job.
    message :: Core.Maybe Core.Text,
    -- | The current status of the events detection job.
    jobStatus :: Core.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the
    -- events detection job.
    outputDataConfig :: Core.Maybe OutputDataConfig,
    -- | The time that the events detection job completed.
    endTime :: Core.Maybe Core.POSIX,
    -- | The time that the events detection job was submitted for processing.
    submitTime :: Core.Maybe Core.POSIX,
    -- | The types of events that are detected by the job.
    targetEventTypes :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The name you assigned the events detection job.
    jobName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identify and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Maybe Core.Text,
    -- | The identifier assigned to the events detection job.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventsDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'eventsDetectionJobProperties_languageCode' - The language code of the input documents.
--
-- 'inputDataConfig', 'eventsDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- events detection job.
--
-- 'message', 'eventsDetectionJobProperties_message' - A description of the status of the events detection job.
--
-- 'jobStatus', 'eventsDetectionJobProperties_jobStatus' - The current status of the events detection job.
--
-- 'outputDataConfig', 'eventsDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- events detection job.
--
-- 'endTime', 'eventsDetectionJobProperties_endTime' - The time that the events detection job completed.
--
-- 'submitTime', 'eventsDetectionJobProperties_submitTime' - The time that the events detection job was submitted for processing.
--
-- 'targetEventTypes', 'eventsDetectionJobProperties_targetEventTypes' - The types of events that are detected by the job.
--
-- 'jobName', 'eventsDetectionJobProperties_jobName' - The name you assigned the events detection job.
--
-- 'dataAccessRoleArn', 'eventsDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identify and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- 'jobId', 'eventsDetectionJobProperties_jobId' - The identifier assigned to the events detection job.
newEventsDetectionJobProperties ::
  EventsDetectionJobProperties
newEventsDetectionJobProperties =
  EventsDetectionJobProperties'
    { languageCode =
        Core.Nothing,
      inputDataConfig = Core.Nothing,
      message = Core.Nothing,
      jobStatus = Core.Nothing,
      outputDataConfig = Core.Nothing,
      endTime = Core.Nothing,
      submitTime = Core.Nothing,
      targetEventTypes = Core.Nothing,
      jobName = Core.Nothing,
      dataAccessRoleArn = Core.Nothing,
      jobId = Core.Nothing
    }

-- | The language code of the input documents.
eventsDetectionJobProperties_languageCode :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe LanguageCode)
eventsDetectionJobProperties_languageCode = Lens.lens (\EventsDetectionJobProperties' {languageCode} -> languageCode) (\s@EventsDetectionJobProperties' {} a -> s {languageCode = a} :: EventsDetectionJobProperties)

-- | The input data configuration that you supplied when you created the
-- events detection job.
eventsDetectionJobProperties_inputDataConfig :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe InputDataConfig)
eventsDetectionJobProperties_inputDataConfig = Lens.lens (\EventsDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@EventsDetectionJobProperties' {} a -> s {inputDataConfig = a} :: EventsDetectionJobProperties)

-- | A description of the status of the events detection job.
eventsDetectionJobProperties_message :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Core.Text)
eventsDetectionJobProperties_message = Lens.lens (\EventsDetectionJobProperties' {message} -> message) (\s@EventsDetectionJobProperties' {} a -> s {message = a} :: EventsDetectionJobProperties)

-- | The current status of the events detection job.
eventsDetectionJobProperties_jobStatus :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe JobStatus)
eventsDetectionJobProperties_jobStatus = Lens.lens (\EventsDetectionJobProperties' {jobStatus} -> jobStatus) (\s@EventsDetectionJobProperties' {} a -> s {jobStatus = a} :: EventsDetectionJobProperties)

-- | The output data configuration that you supplied when you created the
-- events detection job.
eventsDetectionJobProperties_outputDataConfig :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe OutputDataConfig)
eventsDetectionJobProperties_outputDataConfig = Lens.lens (\EventsDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@EventsDetectionJobProperties' {} a -> s {outputDataConfig = a} :: EventsDetectionJobProperties)

-- | The time that the events detection job completed.
eventsDetectionJobProperties_endTime :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Core.UTCTime)
eventsDetectionJobProperties_endTime = Lens.lens (\EventsDetectionJobProperties' {endTime} -> endTime) (\s@EventsDetectionJobProperties' {} a -> s {endTime = a} :: EventsDetectionJobProperties) Core.. Lens.mapping Core._Time

-- | The time that the events detection job was submitted for processing.
eventsDetectionJobProperties_submitTime :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Core.UTCTime)
eventsDetectionJobProperties_submitTime = Lens.lens (\EventsDetectionJobProperties' {submitTime} -> submitTime) (\s@EventsDetectionJobProperties' {} a -> s {submitTime = a} :: EventsDetectionJobProperties) Core.. Lens.mapping Core._Time

-- | The types of events that are detected by the job.
eventsDetectionJobProperties_targetEventTypes :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe (Core.NonEmpty Core.Text))
eventsDetectionJobProperties_targetEventTypes = Lens.lens (\EventsDetectionJobProperties' {targetEventTypes} -> targetEventTypes) (\s@EventsDetectionJobProperties' {} a -> s {targetEventTypes = a} :: EventsDetectionJobProperties) Core.. Lens.mapping Lens._Coerce

-- | The name you assigned the events detection job.
eventsDetectionJobProperties_jobName :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Core.Text)
eventsDetectionJobProperties_jobName = Lens.lens (\EventsDetectionJobProperties' {jobName} -> jobName) (\s@EventsDetectionJobProperties' {} a -> s {jobName = a} :: EventsDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the AWS Identify and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
eventsDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Core.Text)
eventsDetectionJobProperties_dataAccessRoleArn = Lens.lens (\EventsDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@EventsDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: EventsDetectionJobProperties)

-- | The identifier assigned to the events detection job.
eventsDetectionJobProperties_jobId :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Core.Text)
eventsDetectionJobProperties_jobId = Lens.lens (\EventsDetectionJobProperties' {jobId} -> jobId) (\s@EventsDetectionJobProperties' {} a -> s {jobId = a} :: EventsDetectionJobProperties)

instance Core.FromJSON EventsDetectionJobProperties where
  parseJSON =
    Core.withObject
      "EventsDetectionJobProperties"
      ( \x ->
          EventsDetectionJobProperties'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (x Core..:? "OutputDataConfig")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "SubmitTime")
            Core.<*> (x Core..:? "TargetEventTypes")
            Core.<*> (x Core..:? "JobName")
            Core.<*> (x Core..:? "DataAccessRoleArn")
            Core.<*> (x Core..:? "JobId")
      )

instance Core.Hashable EventsDetectionJobProperties

instance Core.NFData EventsDetectionJobProperties
