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
-- Module      : Amazonka.Glue.Types.JobUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JobUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.CodeGenConfigurationNode
import Amazonka.Glue.Types.ConnectionsList
import Amazonka.Glue.Types.ExecutionClass
import Amazonka.Glue.Types.ExecutionProperty
import Amazonka.Glue.Types.JobCommand
import Amazonka.Glue.Types.NotificationProperty
import Amazonka.Glue.Types.SourceControlDetails
import Amazonka.Glue.Types.WorkerType
import qualified Amazonka.Prelude as Prelude

-- | Specifies information used to update an existing job definition. The
-- previous job definition is completely overwritten by this information.
--
-- /See:/ 'newJobUpdate' smart constructor.
data JobUpdate = JobUpdate'
  { -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The job timeout in minutes. This is the maximum time that a job run can
    -- consume resources before it is terminated and enters @TIMEOUT@ status.
    -- The default is 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | Non-overridable arguments for this job, specified as name-value pairs.
    nonOverridableArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | Glue version determines the versions of Apache Spark and Python that
    -- Glue supports. The Python version indicates the version supported for
    -- jobs of type Spark.
    --
    -- For more information about the available Glue versions and corresponding
    -- Spark and Python versions, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
    -- in the developer guide.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the configuration properties of a job notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The type of predefined worker that is allocated when a job runs. Accepts
    -- a value of Standard, G.1X, G.2X, or G.025X.
    --
    -- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
    --     of memory and a 50GB disk, and 2 executors per worker.
    --
    -- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
    --     of memory, 64 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    --
    -- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
    --     of memory, 128 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    --
    -- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
    --     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for low volume streaming jobs. This
    --     worker type is only available for Glue version 3.0 streaming jobs.
    workerType :: Prelude.Maybe WorkerType,
    -- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
    -- allowed for this job.
    executionProperty :: Prelude.Maybe ExecutionProperty,
    -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of Glue data processing units (DPUs) to allocate to this job.
    -- You can allocate a minimum of 2 DPUs; the default is 10. A DPU is a
    -- relative measure of processing power that consists of 4 vCPUs of compute
    -- capacity and 16 GB of memory. For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
    allocatedCapacity :: Prelude.Maybe Prelude.Int,
    -- | The @JobCommand@ that runs this job (required).
    command :: Prelude.Maybe JobCommand,
    -- | Description of the job being defined.
    description :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of times to retry this job if it fails.
    maxRetries :: Prelude.Maybe Prelude.Int,
    -- | The representation of a directed acyclic graph on which both the Glue
    -- Studio visual component and Glue Studio code generation is based.
    codeGenConfigurationNodes :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text CodeGenConfigurationNode)),
    -- | The default arguments for this job.
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
    defaultArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The details for a source control configuration for a job, allowing
    -- synchronization of job artifacts to or from a remote repository.
    sourceControlDetails :: Prelude.Maybe SourceControlDetails,
    -- | This field is reserved for future use.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | The connections used for this job.
    connections :: Prelude.Maybe ConnectionsList,
    -- | The name or Amazon Resource Name (ARN) of the IAM role associated with
    -- this job (required).
    role' :: Prelude.Maybe Prelude.Text,
    -- | For Glue version 1.0 or earlier jobs, using the standard worker type,
    -- the number of Glue data processing units (DPUs) that can be allocated
    -- when this job runs. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
    -- information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
    --
    -- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
    --
    -- The value that can be allocated for @MaxCapacity@ depends on whether you
    -- are running a Python shell job or an Apache Spark ETL job:
    --
    -- -   When you specify a Python shell job
    --     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
    --     or 1 DPU. The default is 0.0625 DPU.
    --
    -- -   When you specify an Apache Spark ETL job
    --     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
    --     (@JobCommand.Name@=\"gluestreaming\"), you can allocate a minimum of
    --     2 DPUs. The default is 10 DPUs. This job type cannot have a
    --     fractional DPU allocation.
    --
    -- For Glue version 2.0 jobs, you cannot instead specify a
    -- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
    -- @Number of workers@.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether the job is run with a standard or flexible execution
    -- class. The standard execution-class is ideal for time-sensitive
    -- workloads that require fast job startup and dedicated resources.
    --
    -- The flexible execution class is appropriate for time-insensitive jobs
    -- whose start and completion times may vary.
    --
    -- Only jobs with Glue version 3.0 and above and command type @glueetl@
    -- will be allowed to set @ExecutionClass@ to @FLEX@. The flexible
    -- execution class is available for Spark jobs.
    executionClass :: Prelude.Maybe ExecutionClass
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfiguration', 'jobUpdate_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job.
--
-- 'timeout', 'jobUpdate_timeout' - The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
--
-- 'nonOverridableArguments', 'jobUpdate_nonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
--
-- 'numberOfWorkers', 'jobUpdate_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- 'glueVersion', 'jobUpdate_glueVersion' - Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- jobs of type Spark.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- 'notificationProperty', 'jobUpdate_notificationProperty' - Specifies the configuration properties of a job notification.
--
-- 'workerType', 'jobUpdate_workerType' - The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, G.2X, or G.025X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
--     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for low volume streaming jobs. This
--     worker type is only available for Glue version 3.0 streaming jobs.
--
-- 'executionProperty', 'jobUpdate_executionProperty' - An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
--
-- 'allocatedCapacity', 'jobUpdate_allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) to allocate to this job.
-- You can allocate a minimum of 2 DPUs; the default is 10. A DPU is a
-- relative measure of processing power that consists of 4 vCPUs of compute
-- capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- 'command', 'jobUpdate_command' - The @JobCommand@ that runs this job (required).
--
-- 'description', 'jobUpdate_description' - Description of the job being defined.
--
-- 'maxRetries', 'jobUpdate_maxRetries' - The maximum number of times to retry this job if it fails.
--
-- 'codeGenConfigurationNodes', 'jobUpdate_codeGenConfigurationNodes' - The representation of a directed acyclic graph on which both the Glue
-- Studio visual component and Glue Studio code generation is based.
--
-- 'defaultArguments', 'jobUpdate_defaultArguments' - The default arguments for this job.
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
-- 'sourceControlDetails', 'jobUpdate_sourceControlDetails' - The details for a source control configuration for a job, allowing
-- synchronization of job artifacts to or from a remote repository.
--
-- 'logUri', 'jobUpdate_logUri' - This field is reserved for future use.
--
-- 'connections', 'jobUpdate_connections' - The connections used for this job.
--
-- 'role'', 'jobUpdate_role' - The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job (required).
--
-- 'maxCapacity', 'jobUpdate_maxCapacity' - For Glue version 1.0 or earlier jobs, using the standard worker type,
-- the number of Glue data processing units (DPUs) that can be allocated
-- when this job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
--
-- The value that can be allocated for @MaxCapacity@ depends on whether you
-- are running a Python shell job or an Apache Spark ETL job:
--
-- -   When you specify a Python shell job
--     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
--     or 1 DPU. The default is 0.0625 DPU.
--
-- -   When you specify an Apache Spark ETL job
--     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
--     (@JobCommand.Name@=\"gluestreaming\"), you can allocate a minimum of
--     2 DPUs. The default is 10 DPUs. This job type cannot have a
--     fractional DPU allocation.
--
-- For Glue version 2.0 jobs, you cannot instead specify a
-- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
-- @Number of workers@.
--
-- 'executionClass', 'jobUpdate_executionClass' - Indicates whether the job is run with a standard or flexible execution
-- class. The standard execution-class is ideal for time-sensitive
-- workloads that require fast job startup and dedicated resources.
--
-- The flexible execution class is appropriate for time-insensitive jobs
-- whose start and completion times may vary.
--
-- Only jobs with Glue version 3.0 and above and command type @glueetl@
-- will be allowed to set @ExecutionClass@ to @FLEX@. The flexible
-- execution class is available for Spark jobs.
newJobUpdate ::
  JobUpdate
newJobUpdate =
  JobUpdate'
    { securityConfiguration = Prelude.Nothing,
      timeout = Prelude.Nothing,
      nonOverridableArguments = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      workerType = Prelude.Nothing,
      executionProperty = Prelude.Nothing,
      allocatedCapacity = Prelude.Nothing,
      command = Prelude.Nothing,
      description = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      codeGenConfigurationNodes = Prelude.Nothing,
      defaultArguments = Prelude.Nothing,
      sourceControlDetails = Prelude.Nothing,
      logUri = Prelude.Nothing,
      connections = Prelude.Nothing,
      role' = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      executionClass = Prelude.Nothing
    }

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job.
jobUpdate_securityConfiguration :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Text)
jobUpdate_securityConfiguration = Lens.lens (\JobUpdate' {securityConfiguration} -> securityConfiguration) (\s@JobUpdate' {} a -> s {securityConfiguration = a} :: JobUpdate)

-- | The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
jobUpdate_timeout :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Natural)
jobUpdate_timeout = Lens.lens (\JobUpdate' {timeout} -> timeout) (\s@JobUpdate' {} a -> s {timeout = a} :: JobUpdate)

-- | Non-overridable arguments for this job, specified as name-value pairs.
jobUpdate_nonOverridableArguments :: Lens.Lens' JobUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobUpdate_nonOverridableArguments = Lens.lens (\JobUpdate' {nonOverridableArguments} -> nonOverridableArguments) (\s@JobUpdate' {} a -> s {nonOverridableArguments = a} :: JobUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
jobUpdate_numberOfWorkers :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Int)
jobUpdate_numberOfWorkers = Lens.lens (\JobUpdate' {numberOfWorkers} -> numberOfWorkers) (\s@JobUpdate' {} a -> s {numberOfWorkers = a} :: JobUpdate)

-- | Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- jobs of type Spark.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
jobUpdate_glueVersion :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Text)
jobUpdate_glueVersion = Lens.lens (\JobUpdate' {glueVersion} -> glueVersion) (\s@JobUpdate' {} a -> s {glueVersion = a} :: JobUpdate)

-- | Specifies the configuration properties of a job notification.
jobUpdate_notificationProperty :: Lens.Lens' JobUpdate (Prelude.Maybe NotificationProperty)
jobUpdate_notificationProperty = Lens.lens (\JobUpdate' {notificationProperty} -> notificationProperty) (\s@JobUpdate' {} a -> s {notificationProperty = a} :: JobUpdate)

-- | The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, G.2X, or G.025X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
--     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for low volume streaming jobs. This
--     worker type is only available for Glue version 3.0 streaming jobs.
jobUpdate_workerType :: Lens.Lens' JobUpdate (Prelude.Maybe WorkerType)
jobUpdate_workerType = Lens.lens (\JobUpdate' {workerType} -> workerType) (\s@JobUpdate' {} a -> s {workerType = a} :: JobUpdate)

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
jobUpdate_executionProperty :: Lens.Lens' JobUpdate (Prelude.Maybe ExecutionProperty)
jobUpdate_executionProperty = Lens.lens (\JobUpdate' {executionProperty} -> executionProperty) (\s@JobUpdate' {} a -> s {executionProperty = a} :: JobUpdate)

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) to allocate to this job.
-- You can allocate a minimum of 2 DPUs; the default is 10. A DPU is a
-- relative measure of processing power that consists of 4 vCPUs of compute
-- capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
jobUpdate_allocatedCapacity :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Int)
jobUpdate_allocatedCapacity = Lens.lens (\JobUpdate' {allocatedCapacity} -> allocatedCapacity) (\s@JobUpdate' {} a -> s {allocatedCapacity = a} :: JobUpdate)

-- | The @JobCommand@ that runs this job (required).
jobUpdate_command :: Lens.Lens' JobUpdate (Prelude.Maybe JobCommand)
jobUpdate_command = Lens.lens (\JobUpdate' {command} -> command) (\s@JobUpdate' {} a -> s {command = a} :: JobUpdate)

-- | Description of the job being defined.
jobUpdate_description :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Text)
jobUpdate_description = Lens.lens (\JobUpdate' {description} -> description) (\s@JobUpdate' {} a -> s {description = a} :: JobUpdate)

-- | The maximum number of times to retry this job if it fails.
jobUpdate_maxRetries :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Int)
jobUpdate_maxRetries = Lens.lens (\JobUpdate' {maxRetries} -> maxRetries) (\s@JobUpdate' {} a -> s {maxRetries = a} :: JobUpdate)

-- | The representation of a directed acyclic graph on which both the Glue
-- Studio visual component and Glue Studio code generation is based.
jobUpdate_codeGenConfigurationNodes :: Lens.Lens' JobUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text CodeGenConfigurationNode))
jobUpdate_codeGenConfigurationNodes = Lens.lens (\JobUpdate' {codeGenConfigurationNodes} -> codeGenConfigurationNodes) (\s@JobUpdate' {} a -> s {codeGenConfigurationNodes = a} :: JobUpdate) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The default arguments for this job.
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
jobUpdate_defaultArguments :: Lens.Lens' JobUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobUpdate_defaultArguments = Lens.lens (\JobUpdate' {defaultArguments} -> defaultArguments) (\s@JobUpdate' {} a -> s {defaultArguments = a} :: JobUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The details for a source control configuration for a job, allowing
-- synchronization of job artifacts to or from a remote repository.
jobUpdate_sourceControlDetails :: Lens.Lens' JobUpdate (Prelude.Maybe SourceControlDetails)
jobUpdate_sourceControlDetails = Lens.lens (\JobUpdate' {sourceControlDetails} -> sourceControlDetails) (\s@JobUpdate' {} a -> s {sourceControlDetails = a} :: JobUpdate)

-- | This field is reserved for future use.
jobUpdate_logUri :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Text)
jobUpdate_logUri = Lens.lens (\JobUpdate' {logUri} -> logUri) (\s@JobUpdate' {} a -> s {logUri = a} :: JobUpdate)

-- | The connections used for this job.
jobUpdate_connections :: Lens.Lens' JobUpdate (Prelude.Maybe ConnectionsList)
jobUpdate_connections = Lens.lens (\JobUpdate' {connections} -> connections) (\s@JobUpdate' {} a -> s {connections = a} :: JobUpdate)

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job (required).
jobUpdate_role :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Text)
jobUpdate_role = Lens.lens (\JobUpdate' {role'} -> role') (\s@JobUpdate' {} a -> s {role' = a} :: JobUpdate)

-- | For Glue version 1.0 or earlier jobs, using the standard worker type,
-- the number of Glue data processing units (DPUs) that can be allocated
-- when this job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
--
-- The value that can be allocated for @MaxCapacity@ depends on whether you
-- are running a Python shell job or an Apache Spark ETL job:
--
-- -   When you specify a Python shell job
--     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
--     or 1 DPU. The default is 0.0625 DPU.
--
-- -   When you specify an Apache Spark ETL job
--     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
--     (@JobCommand.Name@=\"gluestreaming\"), you can allocate a minimum of
--     2 DPUs. The default is 10 DPUs. This job type cannot have a
--     fractional DPU allocation.
--
-- For Glue version 2.0 jobs, you cannot instead specify a
-- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
-- @Number of workers@.
jobUpdate_maxCapacity :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Double)
jobUpdate_maxCapacity = Lens.lens (\JobUpdate' {maxCapacity} -> maxCapacity) (\s@JobUpdate' {} a -> s {maxCapacity = a} :: JobUpdate)

-- | Indicates whether the job is run with a standard or flexible execution
-- class. The standard execution-class is ideal for time-sensitive
-- workloads that require fast job startup and dedicated resources.
--
-- The flexible execution class is appropriate for time-insensitive jobs
-- whose start and completion times may vary.
--
-- Only jobs with Glue version 3.0 and above and command type @glueetl@
-- will be allowed to set @ExecutionClass@ to @FLEX@. The flexible
-- execution class is available for Spark jobs.
jobUpdate_executionClass :: Lens.Lens' JobUpdate (Prelude.Maybe ExecutionClass)
jobUpdate_executionClass = Lens.lens (\JobUpdate' {executionClass} -> executionClass) (\s@JobUpdate' {} a -> s {executionClass = a} :: JobUpdate)

instance Prelude.Hashable JobUpdate where
  hashWithSalt _salt JobUpdate' {..} =
    _salt `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` nonOverridableArguments
      `Prelude.hashWithSalt` numberOfWorkers
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` notificationProperty
      `Prelude.hashWithSalt` workerType
      `Prelude.hashWithSalt` executionProperty
      `Prelude.hashWithSalt` allocatedCapacity
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` codeGenConfigurationNodes
      `Prelude.hashWithSalt` defaultArguments
      `Prelude.hashWithSalt` sourceControlDetails
      `Prelude.hashWithSalt` logUri
      `Prelude.hashWithSalt` connections
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` executionClass

instance Prelude.NFData JobUpdate where
  rnf JobUpdate' {..} =
    Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf nonOverridableArguments
      `Prelude.seq` Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf notificationProperty
      `Prelude.seq` Prelude.rnf workerType
      `Prelude.seq` Prelude.rnf executionProperty
      `Prelude.seq` Prelude.rnf allocatedCapacity
      `Prelude.seq` Prelude.rnf command
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf codeGenConfigurationNodes
      `Prelude.seq` Prelude.rnf defaultArguments
      `Prelude.seq` Prelude.rnf sourceControlDetails
      `Prelude.seq` Prelude.rnf logUri
      `Prelude.seq` Prelude.rnf connections
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf executionClass

instance Core.ToJSON JobUpdate where
  toJSON JobUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityConfiguration" Core..=)
              Prelude.<$> securityConfiguration,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("NonOverridableArguments" Core..=)
              Prelude.<$> nonOverridableArguments,
            ("NumberOfWorkers" Core..=)
              Prelude.<$> numberOfWorkers,
            ("GlueVersion" Core..=) Prelude.<$> glueVersion,
            ("NotificationProperty" Core..=)
              Prelude.<$> notificationProperty,
            ("WorkerType" Core..=) Prelude.<$> workerType,
            ("ExecutionProperty" Core..=)
              Prelude.<$> executionProperty,
            ("AllocatedCapacity" Core..=)
              Prelude.<$> allocatedCapacity,
            ("Command" Core..=) Prelude.<$> command,
            ("Description" Core..=) Prelude.<$> description,
            ("MaxRetries" Core..=) Prelude.<$> maxRetries,
            ("CodeGenConfigurationNodes" Core..=)
              Prelude.<$> codeGenConfigurationNodes,
            ("DefaultArguments" Core..=)
              Prelude.<$> defaultArguments,
            ("SourceControlDetails" Core..=)
              Prelude.<$> sourceControlDetails,
            ("LogUri" Core..=) Prelude.<$> logUri,
            ("Connections" Core..=) Prelude.<$> connections,
            ("Role" Core..=) Prelude.<$> role',
            ("MaxCapacity" Core..=) Prelude.<$> maxCapacity,
            ("ExecutionClass" Core..=)
              Prelude.<$> executionClass
          ]
      )
