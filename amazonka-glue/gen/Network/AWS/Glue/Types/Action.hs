{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.Types.Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Action where

import Network.AWS.Glue.Types.NotificationProperty
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines an action to be initiated by a trigger.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- action.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The name of the crawler to be used with this action.
    crawlerName :: Prelude.Maybe Prelude.Text,
    -- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
    -- can consume resources before it is terminated and enters @TIMEOUT@
    -- status. The default is 2,880 minutes (48 hours). This overrides the
    -- timeout value set in the parent job.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | Specifies configuration properties of a job run notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The name of a job to be executed.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The job arguments used when this trigger fires. For this job run, they
    -- replace the default arguments set in the job definition itself.
    --
    -- You can specify arguments here that your own job-execution script
    -- consumes, as well as arguments that AWS Glue itself consumes.
    --
    -- For information about how to specify and consume your own Job arguments,
    -- see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python>
    -- topic in the developer guide.
    --
    -- For information about the key-value pairs that AWS Glue consumes to set
    -- up your job, see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue>
    -- topic in the developer guide.
    arguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfiguration', 'action_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- action.
--
-- 'crawlerName', 'action_crawlerName' - The name of the crawler to be used with this action.
--
-- 'timeout', 'action_timeout' - The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours). This overrides the
-- timeout value set in the parent job.
--
-- 'notificationProperty', 'action_notificationProperty' - Specifies configuration properties of a job run notification.
--
-- 'jobName', 'action_jobName' - The name of a job to be executed.
--
-- 'arguments', 'action_arguments' - The job arguments used when this trigger fires. For this job run, they
-- replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that AWS Glue itself consumes.
--
-- For information about how to specify and consume your own Job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that AWS Glue consumes to set
-- up your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue>
-- topic in the developer guide.
newAction ::
  Action
newAction =
  Action'
    { securityConfiguration = Prelude.Nothing,
      crawlerName = Prelude.Nothing,
      timeout = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      jobName = Prelude.Nothing,
      arguments = Prelude.Nothing
    }

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- action.
action_securityConfiguration :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_securityConfiguration = Lens.lens (\Action' {securityConfiguration} -> securityConfiguration) (\s@Action' {} a -> s {securityConfiguration = a} :: Action)

-- | The name of the crawler to be used with this action.
action_crawlerName :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_crawlerName = Lens.lens (\Action' {crawlerName} -> crawlerName) (\s@Action' {} a -> s {crawlerName = a} :: Action)

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours). This overrides the
-- timeout value set in the parent job.
action_timeout :: Lens.Lens' Action (Prelude.Maybe Prelude.Natural)
action_timeout = Lens.lens (\Action' {timeout} -> timeout) (\s@Action' {} a -> s {timeout = a} :: Action)

-- | Specifies configuration properties of a job run notification.
action_notificationProperty :: Lens.Lens' Action (Prelude.Maybe NotificationProperty)
action_notificationProperty = Lens.lens (\Action' {notificationProperty} -> notificationProperty) (\s@Action' {} a -> s {notificationProperty = a} :: Action)

-- | The name of a job to be executed.
action_jobName :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_jobName = Lens.lens (\Action' {jobName} -> jobName) (\s@Action' {} a -> s {jobName = a} :: Action)

-- | The job arguments used when this trigger fires. For this job run, they
-- replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that AWS Glue itself consumes.
--
-- For information about how to specify and consume your own Job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that AWS Glue consumes to set
-- up your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue>
-- topic in the developer guide.
action_arguments :: Lens.Lens' Action (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
action_arguments = Lens.lens (\Action' {arguments} -> arguments) (\s@Action' {} a -> s {arguments = a} :: Action) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Action where
  parseJSON =
    Prelude.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Prelude..:? "SecurityConfiguration")
            Prelude.<*> (x Prelude..:? "CrawlerName")
            Prelude.<*> (x Prelude..:? "Timeout")
            Prelude.<*> (x Prelude..:? "NotificationProperty")
            Prelude.<*> (x Prelude..:? "JobName")
            Prelude.<*> ( x Prelude..:? "Arguments"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Action

instance Prelude.NFData Action

instance Prelude.ToJSON Action where
  toJSON Action' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecurityConfiguration" Prelude..=)
              Prelude.<$> securityConfiguration,
            ("CrawlerName" Prelude..=) Prelude.<$> crawlerName,
            ("Timeout" Prelude..=) Prelude.<$> timeout,
            ("NotificationProperty" Prelude..=)
              Prelude.<$> notificationProperty,
            ("JobName" Prelude..=) Prelude.<$> jobName,
            ("Arguments" Prelude..=) Prelude.<$> arguments
          ]
      )
