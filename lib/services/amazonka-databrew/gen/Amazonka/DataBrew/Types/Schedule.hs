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
-- Module      : Amazonka.DataBrew.Types.Schedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.Schedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents one or more dates and times when a job is to run.
--
-- /See:/ 'newSchedule' smart constructor.
data Schedule = Schedule'
  { -- | Metadata tags that have been applied to the schedule.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date and time when the schedule was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The ID of the Amazon Web Services account that owns the schedule.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the schedule was created.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who last modified the
    -- schedule.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the schedule.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | A list of jobs to be run, according to the schedule.
    jobNames :: Prelude.Maybe [Prelude.Text],
    -- | The dates and times when the job is to run. For more information, see
    -- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
    -- in the /Glue DataBrew Developer Guide/.
    cronExpression :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user who created the schedule.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The name of the schedule.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Schedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'schedule_tags' - Metadata tags that have been applied to the schedule.
--
-- 'lastModifiedDate', 'schedule_lastModifiedDate' - The date and time when the schedule was last modified.
--
-- 'accountId', 'schedule_accountId' - The ID of the Amazon Web Services account that owns the schedule.
--
-- 'createDate', 'schedule_createDate' - The date and time that the schedule was created.
--
-- 'lastModifiedBy', 'schedule_lastModifiedBy' - The Amazon Resource Name (ARN) of the user who last modified the
-- schedule.
--
-- 'resourceArn', 'schedule_resourceArn' - The Amazon Resource Name (ARN) of the schedule.
--
-- 'jobNames', 'schedule_jobNames' - A list of jobs to be run, according to the schedule.
--
-- 'cronExpression', 'schedule_cronExpression' - The dates and times when the job is to run. For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
-- in the /Glue DataBrew Developer Guide/.
--
-- 'createdBy', 'schedule_createdBy' - The Amazon Resource Name (ARN) of the user who created the schedule.
--
-- 'name', 'schedule_name' - The name of the schedule.
newSchedule ::
  -- | 'name'
  Prelude.Text ->
  Schedule
newSchedule pName_ =
  Schedule'
    { tags = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      accountId = Prelude.Nothing,
      createDate = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      jobNames = Prelude.Nothing,
      cronExpression = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      name = pName_
    }

-- | Metadata tags that have been applied to the schedule.
schedule_tags :: Lens.Lens' Schedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
schedule_tags = Lens.lens (\Schedule' {tags} -> tags) (\s@Schedule' {} a -> s {tags = a} :: Schedule) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the schedule was last modified.
schedule_lastModifiedDate :: Lens.Lens' Schedule (Prelude.Maybe Prelude.UTCTime)
schedule_lastModifiedDate = Lens.lens (\Schedule' {lastModifiedDate} -> lastModifiedDate) (\s@Schedule' {} a -> s {lastModifiedDate = a} :: Schedule) Prelude.. Lens.mapping Data._Time

-- | The ID of the Amazon Web Services account that owns the schedule.
schedule_accountId :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_accountId = Lens.lens (\Schedule' {accountId} -> accountId) (\s@Schedule' {} a -> s {accountId = a} :: Schedule)

-- | The date and time that the schedule was created.
schedule_createDate :: Lens.Lens' Schedule (Prelude.Maybe Prelude.UTCTime)
schedule_createDate = Lens.lens (\Schedule' {createDate} -> createDate) (\s@Schedule' {} a -> s {createDate = a} :: Schedule) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the user who last modified the
-- schedule.
schedule_lastModifiedBy :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_lastModifiedBy = Lens.lens (\Schedule' {lastModifiedBy} -> lastModifiedBy) (\s@Schedule' {} a -> s {lastModifiedBy = a} :: Schedule)

-- | The Amazon Resource Name (ARN) of the schedule.
schedule_resourceArn :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_resourceArn = Lens.lens (\Schedule' {resourceArn} -> resourceArn) (\s@Schedule' {} a -> s {resourceArn = a} :: Schedule)

-- | A list of jobs to be run, according to the schedule.
schedule_jobNames :: Lens.Lens' Schedule (Prelude.Maybe [Prelude.Text])
schedule_jobNames = Lens.lens (\Schedule' {jobNames} -> jobNames) (\s@Schedule' {} a -> s {jobNames = a} :: Schedule) Prelude.. Lens.mapping Lens.coerced

-- | The dates and times when the job is to run. For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
-- in the /Glue DataBrew Developer Guide/.
schedule_cronExpression :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_cronExpression = Lens.lens (\Schedule' {cronExpression} -> cronExpression) (\s@Schedule' {} a -> s {cronExpression = a} :: Schedule)

-- | The Amazon Resource Name (ARN) of the user who created the schedule.
schedule_createdBy :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_createdBy = Lens.lens (\Schedule' {createdBy} -> createdBy) (\s@Schedule' {} a -> s {createdBy = a} :: Schedule)

-- | The name of the schedule.
schedule_name :: Lens.Lens' Schedule Prelude.Text
schedule_name = Lens.lens (\Schedule' {name} -> name) (\s@Schedule' {} a -> s {name = a} :: Schedule)

instance Data.FromJSON Schedule where
  parseJSON =
    Data.withObject
      "Schedule"
      ( \x ->
          Schedule'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "JobNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CronExpression")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Schedule where
  hashWithSalt _salt Schedule' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` jobNames
      `Prelude.hashWithSalt` cronExpression
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` name

instance Prelude.NFData Schedule where
  rnf Schedule' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf jobNames
      `Prelude.seq` Prelude.rnf cronExpression
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf name
