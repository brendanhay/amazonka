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
-- Module      : Amazonka.Glue.Types.Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.NotificationProperty
import qualified Amazonka.Prelude as Prelude

-- | Defines an action to be initiated by a trigger.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | The job arguments used when this trigger fires. For this job run, they
    -- replace the default arguments set in the job definition itself.
    --
    -- You can specify arguments here that your own job-execution script
    -- consumes, as well as arguments that Glue itself consumes.
    --
    -- For information about how to specify and consume your own Job arguments,
    -- see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
    -- topic in the developer guide.
    --
    -- For information about the key-value pairs that Glue consumes to set up
    -- your job, see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
    -- topic in the developer guide.
    arguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the crawler to be used with this action.
    crawlerName :: Prelude.Maybe Prelude.Text,
    -- | The name of a job to be run.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Specifies configuration properties of a job run notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- action.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
    -- can consume resources before it is terminated and enters @TIMEOUT@
    -- status. The default is 2,880 minutes (48 hours). This overrides the
    -- timeout value set in the parent job.
    timeout :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arguments', 'action_arguments' - The job arguments used when this trigger fires. For this job run, they
-- replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that Glue itself consumes.
--
-- For information about how to specify and consume your own Job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that Glue consumes to set up
-- your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
-- topic in the developer guide.
--
-- 'crawlerName', 'action_crawlerName' - The name of the crawler to be used with this action.
--
-- 'jobName', 'action_jobName' - The name of a job to be run.
--
-- 'notificationProperty', 'action_notificationProperty' - Specifies configuration properties of a job run notification.
--
-- 'securityConfiguration', 'action_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- action.
--
-- 'timeout', 'action_timeout' - The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours). This overrides the
-- timeout value set in the parent job.
newAction ::
  Action
newAction =
  Action'
    { arguments = Prelude.Nothing,
      crawlerName = Prelude.Nothing,
      jobName = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      timeout = Prelude.Nothing
    }

-- | The job arguments used when this trigger fires. For this job run, they
-- replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that Glue itself consumes.
--
-- For information about how to specify and consume your own Job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that Glue consumes to set up
-- your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
-- topic in the developer guide.
action_arguments :: Lens.Lens' Action (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
action_arguments = Lens.lens (\Action' {arguments} -> arguments) (\s@Action' {} a -> s {arguments = a} :: Action) Prelude.. Lens.mapping Lens.coerced

-- | The name of the crawler to be used with this action.
action_crawlerName :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_crawlerName = Lens.lens (\Action' {crawlerName} -> crawlerName) (\s@Action' {} a -> s {crawlerName = a} :: Action)

-- | The name of a job to be run.
action_jobName :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_jobName = Lens.lens (\Action' {jobName} -> jobName) (\s@Action' {} a -> s {jobName = a} :: Action)

-- | Specifies configuration properties of a job run notification.
action_notificationProperty :: Lens.Lens' Action (Prelude.Maybe NotificationProperty)
action_notificationProperty = Lens.lens (\Action' {notificationProperty} -> notificationProperty) (\s@Action' {} a -> s {notificationProperty = a} :: Action)

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- action.
action_securityConfiguration :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_securityConfiguration = Lens.lens (\Action' {securityConfiguration} -> securityConfiguration) (\s@Action' {} a -> s {securityConfiguration = a} :: Action)

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours). This overrides the
-- timeout value set in the parent job.
action_timeout :: Lens.Lens' Action (Prelude.Maybe Prelude.Natural)
action_timeout = Lens.lens (\Action' {timeout} -> timeout) (\s@Action' {} a -> s {timeout = a} :: Action)

instance Data.FromJSON Action where
  parseJSON =
    Data.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Data..:? "Arguments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CrawlerName")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "NotificationProperty")
            Prelude.<*> (x Data..:? "SecurityConfiguration")
            Prelude.<*> (x Data..:? "Timeout")
      )

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt
      `Prelude.hashWithSalt` arguments
      `Prelude.hashWithSalt` crawlerName
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` notificationProperty
      `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` timeout

instance Prelude.NFData Action where
  rnf Action' {..} =
    Prelude.rnf arguments `Prelude.seq`
      Prelude.rnf crawlerName `Prelude.seq`
        Prelude.rnf jobName `Prelude.seq`
          Prelude.rnf notificationProperty `Prelude.seq`
            Prelude.rnf securityConfiguration `Prelude.seq`
              Prelude.rnf timeout

instance Data.ToJSON Action where
  toJSON Action' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arguments" Data..=) Prelude.<$> arguments,
            ("CrawlerName" Data..=) Prelude.<$> crawlerName,
            ("JobName" Data..=) Prelude.<$> jobName,
            ("NotificationProperty" Data..=)
              Prelude.<$> notificationProperty,
            ("SecurityConfiguration" Data..=)
              Prelude.<$> securityConfiguration,
            ("Timeout" Data..=) Prelude.<$> timeout
          ]
      )
