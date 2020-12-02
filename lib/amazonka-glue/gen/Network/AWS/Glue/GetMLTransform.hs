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
-- Module      : Network.AWS.Glue.GetMLTransform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an AWS Glue machine learning transform artifact and all its corresponding metadata. Machine learning transforms are a special type of transform that use machine learning to learn the details of the transformation to be performed by learning from examples provided by humans. These transformations are then saved by AWS Glue. You can retrieve their metadata by calling @GetMLTransform@ .
module Network.AWS.Glue.GetMLTransform
  ( -- * Creating a Request
    getMLTransform,
    GetMLTransform,

    -- * Request Lenses
    gmltTransformId,

    -- * Destructuring the Response
    getMLTransformResponse,
    GetMLTransformResponse,

    -- * Response Lenses
    gmltrsStatus,
    gmltrsNumberOfWorkers,
    gmltrsLastModifiedOn,
    gmltrsLabelCount,
    gmltrsWorkerType,
    gmltrsInputRecordTables,
    gmltrsGlueVersion,
    gmltrsEvaluationMetrics,
    gmltrsSchema,
    gmltrsRole,
    gmltrsName,
    gmltrsParameters,
    gmltrsMaxRetries,
    gmltrsMaxCapacity,
    gmltrsTimeout,
    gmltrsTransformEncryption,
    gmltrsDescription,
    gmltrsCreatedOn,
    gmltrsTransformId,
    gmltrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMLTransform' smart constructor.
newtype GetMLTransform = GetMLTransform' {_gmltTransformId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMLTransform' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmltTransformId' - The unique identifier of the transform, generated at the time that the transform was created.
getMLTransform ::
  -- | 'gmltTransformId'
  Text ->
  GetMLTransform
getMLTransform pTransformId_ =
  GetMLTransform' {_gmltTransformId = pTransformId_}

-- | The unique identifier of the transform, generated at the time that the transform was created.
gmltTransformId :: Lens' GetMLTransform Text
gmltTransformId = lens _gmltTransformId (\s a -> s {_gmltTransformId = a})

instance AWSRequest GetMLTransform where
  type Rs GetMLTransform = GetMLTransformResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetMLTransformResponse'
            <$> (x .?> "Status")
            <*> (x .?> "NumberOfWorkers")
            <*> (x .?> "LastModifiedOn")
            <*> (x .?> "LabelCount")
            <*> (x .?> "WorkerType")
            <*> (x .?> "InputRecordTables" .!@ mempty)
            <*> (x .?> "GlueVersion")
            <*> (x .?> "EvaluationMetrics")
            <*> (x .?> "Schema" .!@ mempty)
            <*> (x .?> "Role")
            <*> (x .?> "Name")
            <*> (x .?> "Parameters")
            <*> (x .?> "MaxRetries")
            <*> (x .?> "MaxCapacity")
            <*> (x .?> "Timeout")
            <*> (x .?> "TransformEncryption")
            <*> (x .?> "Description")
            <*> (x .?> "CreatedOn")
            <*> (x .?> "TransformId")
            <*> (pure (fromEnum s))
      )

instance Hashable GetMLTransform

instance NFData GetMLTransform

instance ToHeaders GetMLTransform where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetMLTransform" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetMLTransform where
  toJSON GetMLTransform' {..} =
    object (catMaybes [Just ("TransformId" .= _gmltTransformId)])

instance ToPath GetMLTransform where
  toPath = const "/"

instance ToQuery GetMLTransform where
  toQuery = const mempty

-- | /See:/ 'getMLTransformResponse' smart constructor.
data GetMLTransformResponse = GetMLTransformResponse'
  { _gmltrsStatus ::
      !(Maybe TransformStatusType),
    _gmltrsNumberOfWorkers :: !(Maybe Int),
    _gmltrsLastModifiedOn :: !(Maybe POSIX),
    _gmltrsLabelCount :: !(Maybe Int),
    _gmltrsWorkerType :: !(Maybe WorkerType),
    _gmltrsInputRecordTables ::
      !(Maybe [GlueTable]),
    _gmltrsGlueVersion :: !(Maybe Text),
    _gmltrsEvaluationMetrics ::
      !(Maybe EvaluationMetrics),
    _gmltrsSchema :: !(Maybe [SchemaColumn]),
    _gmltrsRole :: !(Maybe Text),
    _gmltrsName :: !(Maybe Text),
    _gmltrsParameters ::
      !(Maybe TransformParameters),
    _gmltrsMaxRetries :: !(Maybe Int),
    _gmltrsMaxCapacity :: !(Maybe Double),
    _gmltrsTimeout :: !(Maybe Nat),
    _gmltrsTransformEncryption ::
      !(Maybe TransformEncryption),
    _gmltrsDescription :: !(Maybe Text),
    _gmltrsCreatedOn :: !(Maybe POSIX),
    _gmltrsTransformId :: !(Maybe Text),
    _gmltrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMLTransformResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmltrsStatus' - The last known status of the transform (to indicate whether it can be used or not). One of "NOT_READY", "READY", or "DELETING".
--
-- * 'gmltrsNumberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when this task runs.
--
-- * 'gmltrsLastModifiedOn' - The date and time when the transform was last modified.
--
-- * 'gmltrsLabelCount' - The number of labels available for this transform.
--
-- * 'gmltrsWorkerType' - The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
--
-- * 'gmltrsInputRecordTables' - A list of AWS Glue table definitions used by the transform.
--
-- * 'gmltrsGlueVersion' - This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- * 'gmltrsEvaluationMetrics' - The latest evaluation metrics.
--
-- * 'gmltrsSchema' - The @Map<Column, Type>@ object that represents the schema that this transform accepts. Has an upper bound of 100 columns.
--
-- * 'gmltrsRole' - The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
--
-- * 'gmltrsName' - The unique name given to the transform when it was created.
--
-- * 'gmltrsParameters' - The configuration parameters that are specific to the algorithm used.
--
-- * 'gmltrsMaxRetries' - The maximum number of times to retry a task for this transform after a task run fails.
--
-- * 'gmltrsMaxCapacity' - The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .  When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
--
-- * 'gmltrsTimeout' - The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- * 'gmltrsTransformEncryption' - The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
--
-- * 'gmltrsDescription' - A description of the transform.
--
-- * 'gmltrsCreatedOn' - The date and time when the transform was created.
--
-- * 'gmltrsTransformId' - The unique identifier of the transform, generated at the time that the transform was created.
--
-- * 'gmltrsResponseStatus' - -- | The response status code.
getMLTransformResponse ::
  -- | 'gmltrsResponseStatus'
  Int ->
  GetMLTransformResponse
getMLTransformResponse pResponseStatus_ =
  GetMLTransformResponse'
    { _gmltrsStatus = Nothing,
      _gmltrsNumberOfWorkers = Nothing,
      _gmltrsLastModifiedOn = Nothing,
      _gmltrsLabelCount = Nothing,
      _gmltrsWorkerType = Nothing,
      _gmltrsInputRecordTables = Nothing,
      _gmltrsGlueVersion = Nothing,
      _gmltrsEvaluationMetrics = Nothing,
      _gmltrsSchema = Nothing,
      _gmltrsRole = Nothing,
      _gmltrsName = Nothing,
      _gmltrsParameters = Nothing,
      _gmltrsMaxRetries = Nothing,
      _gmltrsMaxCapacity = Nothing,
      _gmltrsTimeout = Nothing,
      _gmltrsTransformEncryption = Nothing,
      _gmltrsDescription = Nothing,
      _gmltrsCreatedOn = Nothing,
      _gmltrsTransformId = Nothing,
      _gmltrsResponseStatus = pResponseStatus_
    }

-- | The last known status of the transform (to indicate whether it can be used or not). One of "NOT_READY", "READY", or "DELETING".
gmltrsStatus :: Lens' GetMLTransformResponse (Maybe TransformStatusType)
gmltrsStatus = lens _gmltrsStatus (\s a -> s {_gmltrsStatus = a})

-- | The number of workers of a defined @workerType@ that are allocated when this task runs.
gmltrsNumberOfWorkers :: Lens' GetMLTransformResponse (Maybe Int)
gmltrsNumberOfWorkers = lens _gmltrsNumberOfWorkers (\s a -> s {_gmltrsNumberOfWorkers = a})

-- | The date and time when the transform was last modified.
gmltrsLastModifiedOn :: Lens' GetMLTransformResponse (Maybe UTCTime)
gmltrsLastModifiedOn = lens _gmltrsLastModifiedOn (\s a -> s {_gmltrsLastModifiedOn = a}) . mapping _Time

-- | The number of labels available for this transform.
gmltrsLabelCount :: Lens' GetMLTransformResponse (Maybe Int)
gmltrsLabelCount = lens _gmltrsLabelCount (\s a -> s {_gmltrsLabelCount = a})

-- | The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
gmltrsWorkerType :: Lens' GetMLTransformResponse (Maybe WorkerType)
gmltrsWorkerType = lens _gmltrsWorkerType (\s a -> s {_gmltrsWorkerType = a})

-- | A list of AWS Glue table definitions used by the transform.
gmltrsInputRecordTables :: Lens' GetMLTransformResponse [GlueTable]
gmltrsInputRecordTables = lens _gmltrsInputRecordTables (\s a -> s {_gmltrsInputRecordTables = a}) . _Default . _Coerce

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
gmltrsGlueVersion :: Lens' GetMLTransformResponse (Maybe Text)
gmltrsGlueVersion = lens _gmltrsGlueVersion (\s a -> s {_gmltrsGlueVersion = a})

-- | The latest evaluation metrics.
gmltrsEvaluationMetrics :: Lens' GetMLTransformResponse (Maybe EvaluationMetrics)
gmltrsEvaluationMetrics = lens _gmltrsEvaluationMetrics (\s a -> s {_gmltrsEvaluationMetrics = a})

-- | The @Map<Column, Type>@ object that represents the schema that this transform accepts. Has an upper bound of 100 columns.
gmltrsSchema :: Lens' GetMLTransformResponse [SchemaColumn]
gmltrsSchema = lens _gmltrsSchema (\s a -> s {_gmltrsSchema = a}) . _Default . _Coerce

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
gmltrsRole :: Lens' GetMLTransformResponse (Maybe Text)
gmltrsRole = lens _gmltrsRole (\s a -> s {_gmltrsRole = a})

-- | The unique name given to the transform when it was created.
gmltrsName :: Lens' GetMLTransformResponse (Maybe Text)
gmltrsName = lens _gmltrsName (\s a -> s {_gmltrsName = a})

-- | The configuration parameters that are specific to the algorithm used.
gmltrsParameters :: Lens' GetMLTransformResponse (Maybe TransformParameters)
gmltrsParameters = lens _gmltrsParameters (\s a -> s {_gmltrsParameters = a})

-- | The maximum number of times to retry a task for this transform after a task run fails.
gmltrsMaxRetries :: Lens' GetMLTransformResponse (Maybe Int)
gmltrsMaxRetries = lens _gmltrsMaxRetries (\s a -> s {_gmltrsMaxRetries = a})

-- | The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .  When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
gmltrsMaxCapacity :: Lens' GetMLTransformResponse (Maybe Double)
gmltrsMaxCapacity = lens _gmltrsMaxCapacity (\s a -> s {_gmltrsMaxCapacity = a})

-- | The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
gmltrsTimeout :: Lens' GetMLTransformResponse (Maybe Natural)
gmltrsTimeout = lens _gmltrsTimeout (\s a -> s {_gmltrsTimeout = a}) . mapping _Nat

-- | The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
gmltrsTransformEncryption :: Lens' GetMLTransformResponse (Maybe TransformEncryption)
gmltrsTransformEncryption = lens _gmltrsTransformEncryption (\s a -> s {_gmltrsTransformEncryption = a})

-- | A description of the transform.
gmltrsDescription :: Lens' GetMLTransformResponse (Maybe Text)
gmltrsDescription = lens _gmltrsDescription (\s a -> s {_gmltrsDescription = a})

-- | The date and time when the transform was created.
gmltrsCreatedOn :: Lens' GetMLTransformResponse (Maybe UTCTime)
gmltrsCreatedOn = lens _gmltrsCreatedOn (\s a -> s {_gmltrsCreatedOn = a}) . mapping _Time

-- | The unique identifier of the transform, generated at the time that the transform was created.
gmltrsTransformId :: Lens' GetMLTransformResponse (Maybe Text)
gmltrsTransformId = lens _gmltrsTransformId (\s a -> s {_gmltrsTransformId = a})

-- | -- | The response status code.
gmltrsResponseStatus :: Lens' GetMLTransformResponse Int
gmltrsResponseStatus = lens _gmltrsResponseStatus (\s a -> s {_gmltrsResponseStatus = a})

instance NFData GetMLTransformResponse
