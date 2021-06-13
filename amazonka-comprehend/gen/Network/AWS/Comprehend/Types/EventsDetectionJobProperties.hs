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
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about an events detection job.
--
-- /See:/ 'newEventsDetectionJobProperties' smart constructor.
data EventsDetectionJobProperties = EventsDetectionJobProperties'
  { -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The input data configuration that you supplied when you created the
    -- events detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | A description of the status of the events detection job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the events detection job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the
    -- events detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the events detection job completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The time that the events detection job was submitted for processing.
    submitTime :: Prelude.Maybe Core.POSIX,
    -- | The types of events that are detected by the job.
    targetEventTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name you assigned the events detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identify and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the events detection job.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      message = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      endTime = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      targetEventTypes = Prelude.Nothing,
      jobName = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | The language code of the input documents.
eventsDetectionJobProperties_languageCode :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe LanguageCode)
eventsDetectionJobProperties_languageCode = Lens.lens (\EventsDetectionJobProperties' {languageCode} -> languageCode) (\s@EventsDetectionJobProperties' {} a -> s {languageCode = a} :: EventsDetectionJobProperties)

-- | The input data configuration that you supplied when you created the
-- events detection job.
eventsDetectionJobProperties_inputDataConfig :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe InputDataConfig)
eventsDetectionJobProperties_inputDataConfig = Lens.lens (\EventsDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@EventsDetectionJobProperties' {} a -> s {inputDataConfig = a} :: EventsDetectionJobProperties)

-- | A description of the status of the events detection job.
eventsDetectionJobProperties_message :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.Text)
eventsDetectionJobProperties_message = Lens.lens (\EventsDetectionJobProperties' {message} -> message) (\s@EventsDetectionJobProperties' {} a -> s {message = a} :: EventsDetectionJobProperties)

-- | The current status of the events detection job.
eventsDetectionJobProperties_jobStatus :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe JobStatus)
eventsDetectionJobProperties_jobStatus = Lens.lens (\EventsDetectionJobProperties' {jobStatus} -> jobStatus) (\s@EventsDetectionJobProperties' {} a -> s {jobStatus = a} :: EventsDetectionJobProperties)

-- | The output data configuration that you supplied when you created the
-- events detection job.
eventsDetectionJobProperties_outputDataConfig :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe OutputDataConfig)
eventsDetectionJobProperties_outputDataConfig = Lens.lens (\EventsDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@EventsDetectionJobProperties' {} a -> s {outputDataConfig = a} :: EventsDetectionJobProperties)

-- | The time that the events detection job completed.
eventsDetectionJobProperties_endTime :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
eventsDetectionJobProperties_endTime = Lens.lens (\EventsDetectionJobProperties' {endTime} -> endTime) (\s@EventsDetectionJobProperties' {} a -> s {endTime = a} :: EventsDetectionJobProperties) Prelude.. Lens.mapping Core._Time

-- | The time that the events detection job was submitted for processing.
eventsDetectionJobProperties_submitTime :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
eventsDetectionJobProperties_submitTime = Lens.lens (\EventsDetectionJobProperties' {submitTime} -> submitTime) (\s@EventsDetectionJobProperties' {} a -> s {submitTime = a} :: EventsDetectionJobProperties) Prelude.. Lens.mapping Core._Time

-- | The types of events that are detected by the job.
eventsDetectionJobProperties_targetEventTypes :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventsDetectionJobProperties_targetEventTypes = Lens.lens (\EventsDetectionJobProperties' {targetEventTypes} -> targetEventTypes) (\s@EventsDetectionJobProperties' {} a -> s {targetEventTypes = a} :: EventsDetectionJobProperties) Prelude.. Lens.mapping Lens._Coerce

-- | The name you assigned the events detection job.
eventsDetectionJobProperties_jobName :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.Text)
eventsDetectionJobProperties_jobName = Lens.lens (\EventsDetectionJobProperties' {jobName} -> jobName) (\s@EventsDetectionJobProperties' {} a -> s {jobName = a} :: EventsDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the AWS Identify and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
eventsDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.Text)
eventsDetectionJobProperties_dataAccessRoleArn = Lens.lens (\EventsDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@EventsDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: EventsDetectionJobProperties)

-- | The identifier assigned to the events detection job.
eventsDetectionJobProperties_jobId :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.Text)
eventsDetectionJobProperties_jobId = Lens.lens (\EventsDetectionJobProperties' {jobId} -> jobId) (\s@EventsDetectionJobProperties' {} a -> s {jobId = a} :: EventsDetectionJobProperties)

instance Core.FromJSON EventsDetectionJobProperties where
  parseJSON =
    Core.withObject
      "EventsDetectionJobProperties"
      ( \x ->
          EventsDetectionJobProperties'
            Prelude.<$> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "JobStatus")
            Prelude.<*> (x Core..:? "OutputDataConfig")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "SubmitTime")
            Prelude.<*> (x Core..:? "TargetEventTypes")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "JobId")
      )

instance
  Prelude.Hashable
    EventsDetectionJobProperties

instance Prelude.NFData EventsDetectionJobProperties
