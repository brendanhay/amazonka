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
-- Module      : Network.AWS.Glue.UpdateMLTransform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing machine learning transform. Call this operation to tune the algorithm parameters to achieve better results.
--
--
-- After calling this operation, you can call the @StartMLEvaluationTaskRun@ operation to assess how well your new parameters achieved your goals (such as improving the quality of your machine learning transform, or making it more cost-effective).
module Network.AWS.Glue.UpdateMLTransform
  ( -- * Creating a Request
    updateMLTransform,
    UpdateMLTransform,

    -- * Request Lenses
    umltNumberOfWorkers,
    umltWorkerType,
    umltGlueVersion,
    umltRole,
    umltName,
    umltParameters,
    umltMaxRetries,
    umltMaxCapacity,
    umltTimeout,
    umltDescription,
    umltTransformId,

    -- * Destructuring the Response
    updateMLTransformResponse,
    UpdateMLTransformResponse,

    -- * Response Lenses
    umltrsTransformId,
    umltrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateMLTransform' smart constructor.
data UpdateMLTransform = UpdateMLTransform'
  { _umltNumberOfWorkers ::
      !(Maybe Int),
    _umltWorkerType :: !(Maybe WorkerType),
    _umltGlueVersion :: !(Maybe Text),
    _umltRole :: !(Maybe Text),
    _umltName :: !(Maybe Text),
    _umltParameters :: !(Maybe TransformParameters),
    _umltMaxRetries :: !(Maybe Int),
    _umltMaxCapacity :: !(Maybe Double),
    _umltTimeout :: !(Maybe Nat),
    _umltDescription :: !(Maybe Text),
    _umltTransformId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMLTransform' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umltNumberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when this task runs.
--
-- * 'umltWorkerType' - The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
--
-- * 'umltGlueVersion' - This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- * 'umltRole' - The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
--
-- * 'umltName' - The unique name that you gave the transform when you created it.
--
-- * 'umltParameters' - The configuration parameters that are specific to the transform type (algorithm) used. Conditionally dependent on the transform type.
--
-- * 'umltMaxRetries' - The maximum number of times to retry a task for this transform after a task run fails.
--
-- * 'umltMaxCapacity' - The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .  When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
--
-- * 'umltTimeout' - The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- * 'umltDescription' - A description of the transform. The default is an empty string.
--
-- * 'umltTransformId' - A unique identifier that was generated when the transform was created.
updateMLTransform ::
  -- | 'umltTransformId'
  Text ->
  UpdateMLTransform
updateMLTransform pTransformId_ =
  UpdateMLTransform'
    { _umltNumberOfWorkers = Nothing,
      _umltWorkerType = Nothing,
      _umltGlueVersion = Nothing,
      _umltRole = Nothing,
      _umltName = Nothing,
      _umltParameters = Nothing,
      _umltMaxRetries = Nothing,
      _umltMaxCapacity = Nothing,
      _umltTimeout = Nothing,
      _umltDescription = Nothing,
      _umltTransformId = pTransformId_
    }

-- | The number of workers of a defined @workerType@ that are allocated when this task runs.
umltNumberOfWorkers :: Lens' UpdateMLTransform (Maybe Int)
umltNumberOfWorkers = lens _umltNumberOfWorkers (\s a -> s {_umltNumberOfWorkers = a})

-- | The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
umltWorkerType :: Lens' UpdateMLTransform (Maybe WorkerType)
umltWorkerType = lens _umltWorkerType (\s a -> s {_umltWorkerType = a})

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
umltGlueVersion :: Lens' UpdateMLTransform (Maybe Text)
umltGlueVersion = lens _umltGlueVersion (\s a -> s {_umltGlueVersion = a})

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
umltRole :: Lens' UpdateMLTransform (Maybe Text)
umltRole = lens _umltRole (\s a -> s {_umltRole = a})

-- | The unique name that you gave the transform when you created it.
umltName :: Lens' UpdateMLTransform (Maybe Text)
umltName = lens _umltName (\s a -> s {_umltName = a})

-- | The configuration parameters that are specific to the transform type (algorithm) used. Conditionally dependent on the transform type.
umltParameters :: Lens' UpdateMLTransform (Maybe TransformParameters)
umltParameters = lens _umltParameters (\s a -> s {_umltParameters = a})

-- | The maximum number of times to retry a task for this transform after a task run fails.
umltMaxRetries :: Lens' UpdateMLTransform (Maybe Int)
umltMaxRetries = lens _umltMaxRetries (\s a -> s {_umltMaxRetries = a})

-- | The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .  When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
umltMaxCapacity :: Lens' UpdateMLTransform (Maybe Double)
umltMaxCapacity = lens _umltMaxCapacity (\s a -> s {_umltMaxCapacity = a})

-- | The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
umltTimeout :: Lens' UpdateMLTransform (Maybe Natural)
umltTimeout = lens _umltTimeout (\s a -> s {_umltTimeout = a}) . mapping _Nat

-- | A description of the transform. The default is an empty string.
umltDescription :: Lens' UpdateMLTransform (Maybe Text)
umltDescription = lens _umltDescription (\s a -> s {_umltDescription = a})

-- | A unique identifier that was generated when the transform was created.
umltTransformId :: Lens' UpdateMLTransform Text
umltTransformId = lens _umltTransformId (\s a -> s {_umltTransformId = a})

instance AWSRequest UpdateMLTransform where
  type Rs UpdateMLTransform = UpdateMLTransformResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          UpdateMLTransformResponse'
            <$> (x .?> "TransformId") <*> (pure (fromEnum s))
      )

instance Hashable UpdateMLTransform

instance NFData UpdateMLTransform

instance ToHeaders UpdateMLTransform where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.UpdateMLTransform" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateMLTransform where
  toJSON UpdateMLTransform' {..} =
    object
      ( catMaybes
          [ ("NumberOfWorkers" .=) <$> _umltNumberOfWorkers,
            ("WorkerType" .=) <$> _umltWorkerType,
            ("GlueVersion" .=) <$> _umltGlueVersion,
            ("Role" .=) <$> _umltRole,
            ("Name" .=) <$> _umltName,
            ("Parameters" .=) <$> _umltParameters,
            ("MaxRetries" .=) <$> _umltMaxRetries,
            ("MaxCapacity" .=) <$> _umltMaxCapacity,
            ("Timeout" .=) <$> _umltTimeout,
            ("Description" .=) <$> _umltDescription,
            Just ("TransformId" .= _umltTransformId)
          ]
      )

instance ToPath UpdateMLTransform where
  toPath = const "/"

instance ToQuery UpdateMLTransform where
  toQuery = const mempty

-- | /See:/ 'updateMLTransformResponse' smart constructor.
data UpdateMLTransformResponse = UpdateMLTransformResponse'
  { _umltrsTransformId ::
      !(Maybe Text),
    _umltrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMLTransformResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umltrsTransformId' - The unique identifier for the transform that was updated.
--
-- * 'umltrsResponseStatus' - -- | The response status code.
updateMLTransformResponse ::
  -- | 'umltrsResponseStatus'
  Int ->
  UpdateMLTransformResponse
updateMLTransformResponse pResponseStatus_ =
  UpdateMLTransformResponse'
    { _umltrsTransformId = Nothing,
      _umltrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier for the transform that was updated.
umltrsTransformId :: Lens' UpdateMLTransformResponse (Maybe Text)
umltrsTransformId = lens _umltrsTransformId (\s a -> s {_umltrsTransformId = a})

-- | -- | The response status code.
umltrsResponseStatus :: Lens' UpdateMLTransformResponse Int
umltrsResponseStatus = lens _umltrsResponseStatus (\s a -> s {_umltrsResponseStatus = a})

instance NFData UpdateMLTransformResponse
