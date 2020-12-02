{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobUpdate where

import Network.AWS.Glue.Types.ConnectionsList
import Network.AWS.Glue.Types.ExecutionProperty
import Network.AWS.Glue.Types.JobCommand
import Network.AWS.Glue.Types.NotificationProperty
import Network.AWS.Glue.Types.WorkerType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies information used to update an existing job definition. The previous job definition is completely overwritten by this information.
--
--
--
-- /See:/ 'jobUpdate' smart constructor.
data JobUpdate = JobUpdate'
  { _juNumberOfWorkers :: !(Maybe Int),
    _juCommand :: !(Maybe JobCommand),
    _juNotificationProperty :: !(Maybe NotificationProperty),
    _juConnections :: !(Maybe ConnectionsList),
    _juWorkerType :: !(Maybe WorkerType),
    _juSecurityConfiguration :: !(Maybe Text),
    _juGlueVersion :: !(Maybe Text),
    _juNonOverridableArguments :: !(Maybe (Map Text (Text))),
    _juRole :: !(Maybe Text),
    _juLogURI :: !(Maybe Text),
    _juMaxRetries :: !(Maybe Int),
    _juExecutionProperty :: !(Maybe ExecutionProperty),
    _juAllocatedCapacity :: !(Maybe Int),
    _juMaxCapacity :: !(Maybe Double),
    _juTimeout :: !(Maybe Nat),
    _juDefaultArguments :: !(Maybe (Map Text (Text))),
    _juDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'juNumberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when a job runs. The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- * 'juCommand' - The @JobCommand@ that executes this job (required).
--
-- * 'juNotificationProperty' - Specifies the configuration properties of a job notification.
--
-- * 'juConnections' - The connections used for this job.
--
-- * 'juWorkerType' - The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.     * For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB of memory, 64 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.     * For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB of memory, 128 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
--
-- * 'juSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this job.
--
-- * 'juGlueVersion' - Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.  For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
--
-- * 'juNonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
--
-- * 'juRole' - The name or Amazon Resource Name (ARN) of the IAM role associated with this job (required).
--
-- * 'juLogURI' - This field is reserved for future use.
--
-- * 'juMaxRetries' - The maximum number of times to retry this job if it fails.
--
-- * 'juExecutionProperty' - An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
--
-- * 'juAllocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead. The number of AWS Glue data processing units (DPUs) to allocate to this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- * 'juMaxCapacity' - The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> . Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ . The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job or an Apache Spark ETL job:     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl") or Apache Spark streaming ETL job (@JobCommand.Name@ ="gluestreaming"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
--
-- * 'juTimeout' - The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- * 'juDefaultArguments' - The default arguments for this job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- * 'juDescription' - Description of the job being defined.
jobUpdate ::
  JobUpdate
jobUpdate =
  JobUpdate'
    { _juNumberOfWorkers = Nothing,
      _juCommand = Nothing,
      _juNotificationProperty = Nothing,
      _juConnections = Nothing,
      _juWorkerType = Nothing,
      _juSecurityConfiguration = Nothing,
      _juGlueVersion = Nothing,
      _juNonOverridableArguments = Nothing,
      _juRole = Nothing,
      _juLogURI = Nothing,
      _juMaxRetries = Nothing,
      _juExecutionProperty = Nothing,
      _juAllocatedCapacity = Nothing,
      _juMaxCapacity = Nothing,
      _juTimeout = Nothing,
      _juDefaultArguments = Nothing,
      _juDescription = Nothing
    }

-- | The number of workers of a defined @workerType@ that are allocated when a job runs. The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
juNumberOfWorkers :: Lens' JobUpdate (Maybe Int)
juNumberOfWorkers = lens _juNumberOfWorkers (\s a -> s {_juNumberOfWorkers = a})

-- | The @JobCommand@ that executes this job (required).
juCommand :: Lens' JobUpdate (Maybe JobCommand)
juCommand = lens _juCommand (\s a -> s {_juCommand = a})

-- | Specifies the configuration properties of a job notification.
juNotificationProperty :: Lens' JobUpdate (Maybe NotificationProperty)
juNotificationProperty = lens _juNotificationProperty (\s a -> s {_juNotificationProperty = a})

-- | The connections used for this job.
juConnections :: Lens' JobUpdate (Maybe ConnectionsList)
juConnections = lens _juConnections (\s a -> s {_juConnections = a})

-- | The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.     * For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB of memory, 64 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.     * For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB of memory, 128 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
juWorkerType :: Lens' JobUpdate (Maybe WorkerType)
juWorkerType = lens _juWorkerType (\s a -> s {_juWorkerType = a})

-- | The name of the @SecurityConfiguration@ structure to be used with this job.
juSecurityConfiguration :: Lens' JobUpdate (Maybe Text)
juSecurityConfiguration = lens _juSecurityConfiguration (\s a -> s {_juSecurityConfiguration = a})

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.  For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
juGlueVersion :: Lens' JobUpdate (Maybe Text)
juGlueVersion = lens _juGlueVersion (\s a -> s {_juGlueVersion = a})

-- | Non-overridable arguments for this job, specified as name-value pairs.
juNonOverridableArguments :: Lens' JobUpdate (HashMap Text (Text))
juNonOverridableArguments = lens _juNonOverridableArguments (\s a -> s {_juNonOverridableArguments = a}) . _Default . _Map

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with this job (required).
juRole :: Lens' JobUpdate (Maybe Text)
juRole = lens _juRole (\s a -> s {_juRole = a})

-- | This field is reserved for future use.
juLogURI :: Lens' JobUpdate (Maybe Text)
juLogURI = lens _juLogURI (\s a -> s {_juLogURI = a})

-- | The maximum number of times to retry this job if it fails.
juMaxRetries :: Lens' JobUpdate (Maybe Int)
juMaxRetries = lens _juMaxRetries (\s a -> s {_juMaxRetries = a})

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
juExecutionProperty :: Lens' JobUpdate (Maybe ExecutionProperty)
juExecutionProperty = lens _juExecutionProperty (\s a -> s {_juExecutionProperty = a})

-- | This field is deprecated. Use @MaxCapacity@ instead. The number of AWS Glue data processing units (DPUs) to allocate to this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
juAllocatedCapacity :: Lens' JobUpdate (Maybe Int)
juAllocatedCapacity = lens _juAllocatedCapacity (\s a -> s {_juAllocatedCapacity = a})

-- | The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> . Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ . The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job or an Apache Spark ETL job:     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl") or Apache Spark streaming ETL job (@JobCommand.Name@ ="gluestreaming"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
juMaxCapacity :: Lens' JobUpdate (Maybe Double)
juMaxCapacity = lens _juMaxCapacity (\s a -> s {_juMaxCapacity = a})

-- | The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
juTimeout :: Lens' JobUpdate (Maybe Natural)
juTimeout = lens _juTimeout (\s a -> s {_juTimeout = a}) . mapping _Nat

-- | The default arguments for this job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
juDefaultArguments :: Lens' JobUpdate (HashMap Text (Text))
juDefaultArguments = lens _juDefaultArguments (\s a -> s {_juDefaultArguments = a}) . _Default . _Map

-- | Description of the job being defined.
juDescription :: Lens' JobUpdate (Maybe Text)
juDescription = lens _juDescription (\s a -> s {_juDescription = a})

instance Hashable JobUpdate

instance NFData JobUpdate

instance ToJSON JobUpdate where
  toJSON JobUpdate' {..} =
    object
      ( catMaybes
          [ ("NumberOfWorkers" .=) <$> _juNumberOfWorkers,
            ("Command" .=) <$> _juCommand,
            ("NotificationProperty" .=) <$> _juNotificationProperty,
            ("Connections" .=) <$> _juConnections,
            ("WorkerType" .=) <$> _juWorkerType,
            ("SecurityConfiguration" .=) <$> _juSecurityConfiguration,
            ("GlueVersion" .=) <$> _juGlueVersion,
            ("NonOverridableArguments" .=) <$> _juNonOverridableArguments,
            ("Role" .=) <$> _juRole,
            ("LogUri" .=) <$> _juLogURI,
            ("MaxRetries" .=) <$> _juMaxRetries,
            ("ExecutionProperty" .=) <$> _juExecutionProperty,
            ("AllocatedCapacity" .=) <$> _juAllocatedCapacity,
            ("MaxCapacity" .=) <$> _juMaxCapacity,
            ("Timeout" .=) <$> _juTimeout,
            ("DefaultArguments" .=) <$> _juDefaultArguments,
            ("Description" .=) <$> _juDescription
          ]
      )
