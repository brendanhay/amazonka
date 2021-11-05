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
-- Module      : Amazonka.Comprehend.Types.EventsDetectionJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EventsDetectionJobProperties where

import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.LanguageCode
import Amazonka.Comprehend.Types.OutputDataConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an events detection job.
--
-- /See:/ 'newEventsDetectionJobProperties' smart constructor.
data EventsDetectionJobProperties = EventsDetectionJobProperties'
  { -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The identifier assigned to the events detection job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the events detection job. It is a
    -- unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:events-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:events-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The name you assigned the events detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The types of events that are detected by the job.
    targetEventTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The input data configuration that you supplied when you created the
    -- events detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The time that the events detection job completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The output data configuration that you supplied when you created the
    -- events detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identify and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the events detection job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | A description of the status of the events detection job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time that the events detection job was submitted for processing.
    submitTime :: Prelude.Maybe Core.POSIX
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
-- 'jobId', 'eventsDetectionJobProperties_jobId' - The identifier assigned to the events detection job.
--
-- 'jobArn', 'eventsDetectionJobProperties_jobArn' - The Amazon Resource Name (ARN) of the events detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:events-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:events-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'jobName', 'eventsDetectionJobProperties_jobName' - The name you assigned the events detection job.
--
-- 'targetEventTypes', 'eventsDetectionJobProperties_targetEventTypes' - The types of events that are detected by the job.
--
-- 'inputDataConfig', 'eventsDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- events detection job.
--
-- 'endTime', 'eventsDetectionJobProperties_endTime' - The time that the events detection job completed.
--
-- 'outputDataConfig', 'eventsDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- events detection job.
--
-- 'dataAccessRoleArn', 'eventsDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identify and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- 'jobStatus', 'eventsDetectionJobProperties_jobStatus' - The current status of the events detection job.
--
-- 'message', 'eventsDetectionJobProperties_message' - A description of the status of the events detection job.
--
-- 'submitTime', 'eventsDetectionJobProperties_submitTime' - The time that the events detection job was submitted for processing.
newEventsDetectionJobProperties ::
  EventsDetectionJobProperties
newEventsDetectionJobProperties =
  EventsDetectionJobProperties'
    { languageCode =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobName = Prelude.Nothing,
      targetEventTypes = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      endTime = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      message = Prelude.Nothing,
      submitTime = Prelude.Nothing
    }

-- | The language code of the input documents.
eventsDetectionJobProperties_languageCode :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe LanguageCode)
eventsDetectionJobProperties_languageCode = Lens.lens (\EventsDetectionJobProperties' {languageCode} -> languageCode) (\s@EventsDetectionJobProperties' {} a -> s {languageCode = a} :: EventsDetectionJobProperties)

-- | The identifier assigned to the events detection job.
eventsDetectionJobProperties_jobId :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.Text)
eventsDetectionJobProperties_jobId = Lens.lens (\EventsDetectionJobProperties' {jobId} -> jobId) (\s@EventsDetectionJobProperties' {} a -> s {jobId = a} :: EventsDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the events detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:events-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:events-detection-job\/1234abcd12ab34cd56ef1234567890ab@
eventsDetectionJobProperties_jobArn :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.Text)
eventsDetectionJobProperties_jobArn = Lens.lens (\EventsDetectionJobProperties' {jobArn} -> jobArn) (\s@EventsDetectionJobProperties' {} a -> s {jobArn = a} :: EventsDetectionJobProperties)

-- | The name you assigned the events detection job.
eventsDetectionJobProperties_jobName :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.Text)
eventsDetectionJobProperties_jobName = Lens.lens (\EventsDetectionJobProperties' {jobName} -> jobName) (\s@EventsDetectionJobProperties' {} a -> s {jobName = a} :: EventsDetectionJobProperties)

-- | The types of events that are detected by the job.
eventsDetectionJobProperties_targetEventTypes :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventsDetectionJobProperties_targetEventTypes = Lens.lens (\EventsDetectionJobProperties' {targetEventTypes} -> targetEventTypes) (\s@EventsDetectionJobProperties' {} a -> s {targetEventTypes = a} :: EventsDetectionJobProperties) Prelude.. Lens.mapping Lens.coerced

-- | The input data configuration that you supplied when you created the
-- events detection job.
eventsDetectionJobProperties_inputDataConfig :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe InputDataConfig)
eventsDetectionJobProperties_inputDataConfig = Lens.lens (\EventsDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@EventsDetectionJobProperties' {} a -> s {inputDataConfig = a} :: EventsDetectionJobProperties)

-- | The time that the events detection job completed.
eventsDetectionJobProperties_endTime :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
eventsDetectionJobProperties_endTime = Lens.lens (\EventsDetectionJobProperties' {endTime} -> endTime) (\s@EventsDetectionJobProperties' {} a -> s {endTime = a} :: EventsDetectionJobProperties) Prelude.. Lens.mapping Core._Time

-- | The output data configuration that you supplied when you created the
-- events detection job.
eventsDetectionJobProperties_outputDataConfig :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe OutputDataConfig)
eventsDetectionJobProperties_outputDataConfig = Lens.lens (\EventsDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@EventsDetectionJobProperties' {} a -> s {outputDataConfig = a} :: EventsDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the AWS Identify and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
eventsDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.Text)
eventsDetectionJobProperties_dataAccessRoleArn = Lens.lens (\EventsDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@EventsDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: EventsDetectionJobProperties)

-- | The current status of the events detection job.
eventsDetectionJobProperties_jobStatus :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe JobStatus)
eventsDetectionJobProperties_jobStatus = Lens.lens (\EventsDetectionJobProperties' {jobStatus} -> jobStatus) (\s@EventsDetectionJobProperties' {} a -> s {jobStatus = a} :: EventsDetectionJobProperties)

-- | A description of the status of the events detection job.
eventsDetectionJobProperties_message :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.Text)
eventsDetectionJobProperties_message = Lens.lens (\EventsDetectionJobProperties' {message} -> message) (\s@EventsDetectionJobProperties' {} a -> s {message = a} :: EventsDetectionJobProperties)

-- | The time that the events detection job was submitted for processing.
eventsDetectionJobProperties_submitTime :: Lens.Lens' EventsDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
eventsDetectionJobProperties_submitTime = Lens.lens (\EventsDetectionJobProperties' {submitTime} -> submitTime) (\s@EventsDetectionJobProperties' {} a -> s {submitTime = a} :: EventsDetectionJobProperties) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON EventsDetectionJobProperties where
  parseJSON =
    Core.withObject
      "EventsDetectionJobProperties"
      ( \x ->
          EventsDetectionJobProperties'
            Prelude.<$> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "JobArn")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "TargetEventTypes")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "OutputDataConfig")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "JobStatus")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "SubmitTime")
      )

instance
  Prelude.Hashable
    EventsDetectionJobProperties

instance Prelude.NFData EventsDetectionJobProperties
