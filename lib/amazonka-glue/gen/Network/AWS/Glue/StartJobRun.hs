{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartJobRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job run using a job definition.
module Network.AWS.Glue.StartJobRun
  ( -- * Creating a Request
    startJobRun,
    StartJobRun,

    -- * Request Lenses
    sjrNumberOfWorkers,
    sjrNotificationProperty,
    sjrArguments,
    sjrWorkerType,
    sjrSecurityConfiguration,
    sjrAllocatedCapacity,
    sjrMaxCapacity,
    sjrTimeout,
    sjrJobRunId,
    sjrJobName,

    -- * Destructuring the Response
    startJobRunResponse,
    StartJobRunResponse,

    -- * Response Lenses
    sjrrsJobRunId,
    sjrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startJobRun' smart constructor.
data StartJobRun = StartJobRun'
  { _sjrNumberOfWorkers ::
      !(Maybe Int),
    _sjrNotificationProperty :: !(Maybe NotificationProperty),
    _sjrArguments :: !(Maybe (Map Text (Text))),
    _sjrWorkerType :: !(Maybe WorkerType),
    _sjrSecurityConfiguration :: !(Maybe Text),
    _sjrAllocatedCapacity :: !(Maybe Int),
    _sjrMaxCapacity :: !(Maybe Double),
    _sjrTimeout :: !(Maybe Nat),
    _sjrJobRunId :: !(Maybe Text),
    _sjrJobName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartJobRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjrNumberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when a job runs. The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- * 'sjrNotificationProperty' - Specifies configuration properties of a job run notification.
--
-- * 'sjrArguments' - The job arguments specifically for this run. For this job run, they replace the default arguments set in the job definition itself. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- * 'sjrWorkerType' - The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
--
-- * 'sjrSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this job run.
--
-- * 'sjrAllocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead. The number of AWS Glue data processing units (DPUs) to allocate to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- * 'sjrMaxCapacity' - The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> . Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ . The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job, or an Apache Spark ETL job:     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
--
-- * 'sjrTimeout' - The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
--
-- * 'sjrJobRunId' - The ID of a previous @JobRun@ to retry.
--
-- * 'sjrJobName' - The name of the job definition to use.
startJobRun ::
  -- | 'sjrJobName'
  Text ->
  StartJobRun
startJobRun pJobName_ =
  StartJobRun'
    { _sjrNumberOfWorkers = Nothing,
      _sjrNotificationProperty = Nothing,
      _sjrArguments = Nothing,
      _sjrWorkerType = Nothing,
      _sjrSecurityConfiguration = Nothing,
      _sjrAllocatedCapacity = Nothing,
      _sjrMaxCapacity = Nothing,
      _sjrTimeout = Nothing,
      _sjrJobRunId = Nothing,
      _sjrJobName = pJobName_
    }

-- | The number of workers of a defined @workerType@ that are allocated when a job runs. The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
sjrNumberOfWorkers :: Lens' StartJobRun (Maybe Int)
sjrNumberOfWorkers = lens _sjrNumberOfWorkers (\s a -> s {_sjrNumberOfWorkers = a})

-- | Specifies configuration properties of a job run notification.
sjrNotificationProperty :: Lens' StartJobRun (Maybe NotificationProperty)
sjrNotificationProperty = lens _sjrNotificationProperty (\s a -> s {_sjrNotificationProperty = a})

-- | The job arguments specifically for this run. For this job run, they replace the default arguments set in the job definition itself. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
sjrArguments :: Lens' StartJobRun (HashMap Text (Text))
sjrArguments = lens _sjrArguments (\s a -> s {_sjrArguments = a}) . _Default . _Map

-- | The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
sjrWorkerType :: Lens' StartJobRun (Maybe WorkerType)
sjrWorkerType = lens _sjrWorkerType (\s a -> s {_sjrWorkerType = a})

-- | The name of the @SecurityConfiguration@ structure to be used with this job run.
sjrSecurityConfiguration :: Lens' StartJobRun (Maybe Text)
sjrSecurityConfiguration = lens _sjrSecurityConfiguration (\s a -> s {_sjrSecurityConfiguration = a})

-- | This field is deprecated. Use @MaxCapacity@ instead. The number of AWS Glue data processing units (DPUs) to allocate to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
sjrAllocatedCapacity :: Lens' StartJobRun (Maybe Int)
sjrAllocatedCapacity = lens _sjrAllocatedCapacity (\s a -> s {_sjrAllocatedCapacity = a})

-- | The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> . Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ . The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job, or an Apache Spark ETL job:     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
sjrMaxCapacity :: Lens' StartJobRun (Maybe Double)
sjrMaxCapacity = lens _sjrMaxCapacity (\s a -> s {_sjrMaxCapacity = a})

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
sjrTimeout :: Lens' StartJobRun (Maybe Natural)
sjrTimeout = lens _sjrTimeout (\s a -> s {_sjrTimeout = a}) . mapping _Nat

-- | The ID of a previous @JobRun@ to retry.
sjrJobRunId :: Lens' StartJobRun (Maybe Text)
sjrJobRunId = lens _sjrJobRunId (\s a -> s {_sjrJobRunId = a})

-- | The name of the job definition to use.
sjrJobName :: Lens' StartJobRun Text
sjrJobName = lens _sjrJobName (\s a -> s {_sjrJobName = a})

instance AWSRequest StartJobRun where
  type Rs StartJobRun = StartJobRunResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          StartJobRunResponse'
            <$> (x .?> "JobRunId") <*> (pure (fromEnum s))
      )

instance Hashable StartJobRun

instance NFData StartJobRun

instance ToHeaders StartJobRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.StartJobRun" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartJobRun where
  toJSON StartJobRun' {..} =
    object
      ( catMaybes
          [ ("NumberOfWorkers" .=) <$> _sjrNumberOfWorkers,
            ("NotificationProperty" .=) <$> _sjrNotificationProperty,
            ("Arguments" .=) <$> _sjrArguments,
            ("WorkerType" .=) <$> _sjrWorkerType,
            ("SecurityConfiguration" .=) <$> _sjrSecurityConfiguration,
            ("AllocatedCapacity" .=) <$> _sjrAllocatedCapacity,
            ("MaxCapacity" .=) <$> _sjrMaxCapacity,
            ("Timeout" .=) <$> _sjrTimeout,
            ("JobRunId" .=) <$> _sjrJobRunId,
            Just ("JobName" .= _sjrJobName)
          ]
      )

instance ToPath StartJobRun where
  toPath = const "/"

instance ToQuery StartJobRun where
  toQuery = const mempty

-- | /See:/ 'startJobRunResponse' smart constructor.
data StartJobRunResponse = StartJobRunResponse'
  { _sjrrsJobRunId ::
      !(Maybe Text),
    _sjrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartJobRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjrrsJobRunId' - The ID assigned to this job run.
--
-- * 'sjrrsResponseStatus' - -- | The response status code.
startJobRunResponse ::
  -- | 'sjrrsResponseStatus'
  Int ->
  StartJobRunResponse
startJobRunResponse pResponseStatus_ =
  StartJobRunResponse'
    { _sjrrsJobRunId = Nothing,
      _sjrrsResponseStatus = pResponseStatus_
    }

-- | The ID assigned to this job run.
sjrrsJobRunId :: Lens' StartJobRunResponse (Maybe Text)
sjrrsJobRunId = lens _sjrrsJobRunId (\s a -> s {_sjrrsJobRunId = a})

-- | -- | The response status code.
sjrrsResponseStatus :: Lens' StartJobRunResponse Int
sjrrsResponseStatus = lens _sjrrsResponseStatus (\s a -> s {_sjrrsResponseStatus = a})

instance NFData StartJobRunResponse
