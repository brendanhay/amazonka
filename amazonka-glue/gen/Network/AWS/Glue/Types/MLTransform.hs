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
-- Module      : Network.AWS.Glue.Types.MLTransform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MLTransform where

import Network.AWS.Glue.Types.EvaluationMetrics
import Network.AWS.Glue.Types.GlueTable
import Network.AWS.Glue.Types.SchemaColumn
import Network.AWS.Glue.Types.TransformEncryption
import Network.AWS.Glue.Types.TransformParameters
import Network.AWS.Glue.Types.TransformStatusType
import Network.AWS.Glue.Types.WorkerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure for a machine learning transform.
--
-- /See:/ 'newMLTransform' smart constructor.
data MLTransform = MLTransform'
  { -- | The current status of the machine learning transform.
    status :: Prelude.Maybe TransformStatusType,
    -- | The unique transform ID that is generated for the machine learning
    -- transform. The ID is guaranteed to be unique and does not change.
    transformId :: Prelude.Maybe Prelude.Text,
    -- | A map of key-value pairs representing the columns and data types that
    -- this transform can run against. Has an upper bound of 100 columns.
    schema :: Prelude.Maybe [SchemaColumn],
    -- | A timestamp. The time and date that this machine learning transform was
    -- created.
    createdOn :: Prelude.Maybe Prelude.POSIX,
    -- | A list of AWS Glue table definitions used by the transform.
    inputRecordTables :: Prelude.Maybe [GlueTable],
    -- | The encryption-at-rest settings of the transform that apply to accessing
    -- user data. Machine learning transforms can access user data encrypted in
    -- Amazon S3 using KMS.
    transformEncryption :: Prelude.Maybe TransformEncryption,
    -- | The timeout in minutes of the machine learning transform.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The number of AWS Glue data processing units (DPUs) that are allocated
    -- to task runs for this transform. You can allocate from 2 to 100 DPUs;
    -- the default is 10. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
    -- information, see the
    -- <http://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
    --
    -- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
    -- @WorkerType@.
    --
    -- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
    --     @MaxCapacity@ cannot be set.
    --
    -- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
    --     @WorkerType@ can be set.
    --
    -- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
    --     versa).
    --
    -- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
    --
    -- When the @WorkerType@ field is set to a value other than @Standard@, the
    -- @MaxCapacity@ field is set automatically and becomes read-only.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | A timestamp. The last point in time when this machine learning transform
    -- was modified.
    lastModifiedOn :: Prelude.Maybe Prelude.POSIX,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- a task of the transform runs.
    --
    -- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
    -- versa).
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | A user-defined name for the machine learning transform. Names are not
    -- guaranteed unique and can be changed at any time.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the IAM role with the required
    -- permissions. The required permissions include both AWS Glue service role
    -- permissions to AWS Glue resources, and Amazon S3 permissions required by
    -- the transform.
    --
    -- -   This role needs AWS Glue service role permissions to allow access to
    --     resources in AWS Glue. See
    --     <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access AWS Glue>.
    --
    -- -   This role needs permission to your Amazon Simple Storage Service
    --     (Amazon S3) sources, targets, temporary directory, scripts, and any
    --     libraries used by the task run for this transform.
    role' :: Prelude.Maybe Prelude.Text,
    -- | This value determines which version of AWS Glue this machine learning
    -- transform is compatible with. Glue 1.0 is recommended for most
    -- customers. If the value is not set, the Glue compatibility defaults to
    -- Glue 0.9. For more information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
    -- in the developer guide.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | An @EvaluationMetrics@ object. Evaluation metrics provide an estimate of
    -- the quality of your machine learning transform.
    evaluationMetrics :: Prelude.Maybe EvaluationMetrics,
    -- | The type of predefined worker that is allocated when a task of this
    -- transform runs. Accepts a value of Standard, G.1X, or G.2X.
    --
    -- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
    --     of memory and a 50GB disk, and 2 executors per worker.
    --
    -- -   For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of
    --     memory and a 64GB disk, and 1 executor per worker.
    --
    -- -   For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of
    --     memory and a 128GB disk, and 1 executor per worker.
    --
    -- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
    -- @WorkerType@.
    --
    -- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
    --     @MaxCapacity@ cannot be set.
    --
    -- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
    --     @WorkerType@ can be set.
    --
    -- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
    --     versa).
    --
    -- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
    workerType :: Prelude.Maybe WorkerType,
    -- | A user-defined, long-form description text for the machine learning
    -- transform. Descriptions are not guaranteed to be unique and can be
    -- changed at any time.
    description :: Prelude.Maybe Prelude.Text,
    -- | A count identifier for the labeling files generated by AWS Glue for this
    -- transform. As you create a better transform, you can iteratively
    -- download, label, and upload the labeling file.
    labelCount :: Prelude.Maybe Prelude.Int,
    -- | A @TransformParameters@ object. You can use parameters to tune
    -- (customize) the behavior of the machine learning transform by specifying
    -- what data it learns from and your preference on various tradeoffs (such
    -- as precious vs. recall, or accuracy vs. cost).
    parameters :: Prelude.Maybe TransformParameters,
    -- | The maximum number of times to retry after an @MLTaskRun@ of the machine
    -- learning transform fails.
    maxRetries :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MLTransform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'mLTransform_status' - The current status of the machine learning transform.
--
-- 'transformId', 'mLTransform_transformId' - The unique transform ID that is generated for the machine learning
-- transform. The ID is guaranteed to be unique and does not change.
--
-- 'schema', 'mLTransform_schema' - A map of key-value pairs representing the columns and data types that
-- this transform can run against. Has an upper bound of 100 columns.
--
-- 'createdOn', 'mLTransform_createdOn' - A timestamp. The time and date that this machine learning transform was
-- created.
--
-- 'inputRecordTables', 'mLTransform_inputRecordTables' - A list of AWS Glue table definitions used by the transform.
--
-- 'transformEncryption', 'mLTransform_transformEncryption' - The encryption-at-rest settings of the transform that apply to accessing
-- user data. Machine learning transforms can access user data encrypted in
-- Amazon S3 using KMS.
--
-- 'timeout', 'mLTransform_timeout' - The timeout in minutes of the machine learning transform.
--
-- 'maxCapacity', 'mLTransform_maxCapacity' - The number of AWS Glue data processing units (DPUs) that are allocated
-- to task runs for this transform. You can allocate from 2 to 100 DPUs;
-- the default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <http://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
-- @WorkerType@.
--
-- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
--     @MaxCapacity@ cannot be set.
--
-- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
--     @WorkerType@ can be set.
--
-- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
--     versa).
--
-- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
--
-- 'lastModifiedOn', 'mLTransform_lastModifiedOn' - A timestamp. The last point in time when this machine learning transform
-- was modified.
--
-- 'numberOfWorkers', 'mLTransform_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a task of the transform runs.
--
-- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
-- versa).
--
-- 'name', 'mLTransform_name' - A user-defined name for the machine learning transform. Names are not
-- guaranteed unique and can be changed at any time.
--
-- 'role'', 'mLTransform_role' - The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions. The required permissions include both AWS Glue service role
-- permissions to AWS Glue resources, and Amazon S3 permissions required by
-- the transform.
--
-- -   This role needs AWS Glue service role permissions to allow access to
--     resources in AWS Glue. See
--     <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access AWS Glue>.
--
-- -   This role needs permission to your Amazon Simple Storage Service
--     (Amazon S3) sources, targets, temporary directory, scripts, and any
--     libraries used by the task run for this transform.
--
-- 'glueVersion', 'mLTransform_glueVersion' - This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
-- in the developer guide.
--
-- 'evaluationMetrics', 'mLTransform_evaluationMetrics' - An @EvaluationMetrics@ object. Evaluation metrics provide an estimate of
-- the quality of your machine learning transform.
--
-- 'workerType', 'mLTransform_workerType' - The type of predefined worker that is allocated when a task of this
-- transform runs. Accepts a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of
--     memory and a 64GB disk, and 1 executor per worker.
--
-- -   For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of
--     memory and a 128GB disk, and 1 executor per worker.
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
-- @WorkerType@.
--
-- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
--     @MaxCapacity@ cannot be set.
--
-- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
--     @WorkerType@ can be set.
--
-- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
--     versa).
--
-- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
--
-- 'description', 'mLTransform_description' - A user-defined, long-form description text for the machine learning
-- transform. Descriptions are not guaranteed to be unique and can be
-- changed at any time.
--
-- 'labelCount', 'mLTransform_labelCount' - A count identifier for the labeling files generated by AWS Glue for this
-- transform. As you create a better transform, you can iteratively
-- download, label, and upload the labeling file.
--
-- 'parameters', 'mLTransform_parameters' - A @TransformParameters@ object. You can use parameters to tune
-- (customize) the behavior of the machine learning transform by specifying
-- what data it learns from and your preference on various tradeoffs (such
-- as precious vs. recall, or accuracy vs. cost).
--
-- 'maxRetries', 'mLTransform_maxRetries' - The maximum number of times to retry after an @MLTaskRun@ of the machine
-- learning transform fails.
newMLTransform ::
  MLTransform
newMLTransform =
  MLTransform'
    { status = Prelude.Nothing,
      transformId = Prelude.Nothing,
      schema = Prelude.Nothing,
      createdOn = Prelude.Nothing,
      inputRecordTables = Prelude.Nothing,
      transformEncryption = Prelude.Nothing,
      timeout = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      name = Prelude.Nothing,
      role' = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      evaluationMetrics = Prelude.Nothing,
      workerType = Prelude.Nothing,
      description = Prelude.Nothing,
      labelCount = Prelude.Nothing,
      parameters = Prelude.Nothing,
      maxRetries = Prelude.Nothing
    }

-- | The current status of the machine learning transform.
mLTransform_status :: Lens.Lens' MLTransform (Prelude.Maybe TransformStatusType)
mLTransform_status = Lens.lens (\MLTransform' {status} -> status) (\s@MLTransform' {} a -> s {status = a} :: MLTransform)

-- | The unique transform ID that is generated for the machine learning
-- transform. The ID is guaranteed to be unique and does not change.
mLTransform_transformId :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.Text)
mLTransform_transformId = Lens.lens (\MLTransform' {transformId} -> transformId) (\s@MLTransform' {} a -> s {transformId = a} :: MLTransform)

-- | A map of key-value pairs representing the columns and data types that
-- this transform can run against. Has an upper bound of 100 columns.
mLTransform_schema :: Lens.Lens' MLTransform (Prelude.Maybe [SchemaColumn])
mLTransform_schema = Lens.lens (\MLTransform' {schema} -> schema) (\s@MLTransform' {} a -> s {schema = a} :: MLTransform) Prelude.. Lens.mapping Prelude._Coerce

-- | A timestamp. The time and date that this machine learning transform was
-- created.
mLTransform_createdOn :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.UTCTime)
mLTransform_createdOn = Lens.lens (\MLTransform' {createdOn} -> createdOn) (\s@MLTransform' {} a -> s {createdOn = a} :: MLTransform) Prelude.. Lens.mapping Prelude._Time

-- | A list of AWS Glue table definitions used by the transform.
mLTransform_inputRecordTables :: Lens.Lens' MLTransform (Prelude.Maybe [GlueTable])
mLTransform_inputRecordTables = Lens.lens (\MLTransform' {inputRecordTables} -> inputRecordTables) (\s@MLTransform' {} a -> s {inputRecordTables = a} :: MLTransform) Prelude.. Lens.mapping Prelude._Coerce

-- | The encryption-at-rest settings of the transform that apply to accessing
-- user data. Machine learning transforms can access user data encrypted in
-- Amazon S3 using KMS.
mLTransform_transformEncryption :: Lens.Lens' MLTransform (Prelude.Maybe TransformEncryption)
mLTransform_transformEncryption = Lens.lens (\MLTransform' {transformEncryption} -> transformEncryption) (\s@MLTransform' {} a -> s {transformEncryption = a} :: MLTransform)

-- | The timeout in minutes of the machine learning transform.
mLTransform_timeout :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.Natural)
mLTransform_timeout = Lens.lens (\MLTransform' {timeout} -> timeout) (\s@MLTransform' {} a -> s {timeout = a} :: MLTransform)

-- | The number of AWS Glue data processing units (DPUs) that are allocated
-- to task runs for this transform. You can allocate from 2 to 100 DPUs;
-- the default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <http://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
-- @WorkerType@.
--
-- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
--     @MaxCapacity@ cannot be set.
--
-- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
--     @WorkerType@ can be set.
--
-- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
--     versa).
--
-- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
mLTransform_maxCapacity :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.Double)
mLTransform_maxCapacity = Lens.lens (\MLTransform' {maxCapacity} -> maxCapacity) (\s@MLTransform' {} a -> s {maxCapacity = a} :: MLTransform)

-- | A timestamp. The last point in time when this machine learning transform
-- was modified.
mLTransform_lastModifiedOn :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.UTCTime)
mLTransform_lastModifiedOn = Lens.lens (\MLTransform' {lastModifiedOn} -> lastModifiedOn) (\s@MLTransform' {} a -> s {lastModifiedOn = a} :: MLTransform) Prelude.. Lens.mapping Prelude._Time

-- | The number of workers of a defined @workerType@ that are allocated when
-- a task of the transform runs.
--
-- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
-- versa).
mLTransform_numberOfWorkers :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.Int)
mLTransform_numberOfWorkers = Lens.lens (\MLTransform' {numberOfWorkers} -> numberOfWorkers) (\s@MLTransform' {} a -> s {numberOfWorkers = a} :: MLTransform)

-- | A user-defined name for the machine learning transform. Names are not
-- guaranteed unique and can be changed at any time.
mLTransform_name :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.Text)
mLTransform_name = Lens.lens (\MLTransform' {name} -> name) (\s@MLTransform' {} a -> s {name = a} :: MLTransform)

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions. The required permissions include both AWS Glue service role
-- permissions to AWS Glue resources, and Amazon S3 permissions required by
-- the transform.
--
-- -   This role needs AWS Glue service role permissions to allow access to
--     resources in AWS Glue. See
--     <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access AWS Glue>.
--
-- -   This role needs permission to your Amazon Simple Storage Service
--     (Amazon S3) sources, targets, temporary directory, scripts, and any
--     libraries used by the task run for this transform.
mLTransform_role :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.Text)
mLTransform_role = Lens.lens (\MLTransform' {role'} -> role') (\s@MLTransform' {} a -> s {role' = a} :: MLTransform)

-- | This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
-- in the developer guide.
mLTransform_glueVersion :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.Text)
mLTransform_glueVersion = Lens.lens (\MLTransform' {glueVersion} -> glueVersion) (\s@MLTransform' {} a -> s {glueVersion = a} :: MLTransform)

-- | An @EvaluationMetrics@ object. Evaluation metrics provide an estimate of
-- the quality of your machine learning transform.
mLTransform_evaluationMetrics :: Lens.Lens' MLTransform (Prelude.Maybe EvaluationMetrics)
mLTransform_evaluationMetrics = Lens.lens (\MLTransform' {evaluationMetrics} -> evaluationMetrics) (\s@MLTransform' {} a -> s {evaluationMetrics = a} :: MLTransform)

-- | The type of predefined worker that is allocated when a task of this
-- transform runs. Accepts a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of
--     memory and a 64GB disk, and 1 executor per worker.
--
-- -   For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of
--     memory and a 128GB disk, and 1 executor per worker.
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
-- @WorkerType@.
--
-- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
--     @MaxCapacity@ cannot be set.
--
-- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
--     @WorkerType@ can be set.
--
-- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
--     versa).
--
-- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
mLTransform_workerType :: Lens.Lens' MLTransform (Prelude.Maybe WorkerType)
mLTransform_workerType = Lens.lens (\MLTransform' {workerType} -> workerType) (\s@MLTransform' {} a -> s {workerType = a} :: MLTransform)

-- | A user-defined, long-form description text for the machine learning
-- transform. Descriptions are not guaranteed to be unique and can be
-- changed at any time.
mLTransform_description :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.Text)
mLTransform_description = Lens.lens (\MLTransform' {description} -> description) (\s@MLTransform' {} a -> s {description = a} :: MLTransform)

-- | A count identifier for the labeling files generated by AWS Glue for this
-- transform. As you create a better transform, you can iteratively
-- download, label, and upload the labeling file.
mLTransform_labelCount :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.Int)
mLTransform_labelCount = Lens.lens (\MLTransform' {labelCount} -> labelCount) (\s@MLTransform' {} a -> s {labelCount = a} :: MLTransform)

-- | A @TransformParameters@ object. You can use parameters to tune
-- (customize) the behavior of the machine learning transform by specifying
-- what data it learns from and your preference on various tradeoffs (such
-- as precious vs. recall, or accuracy vs. cost).
mLTransform_parameters :: Lens.Lens' MLTransform (Prelude.Maybe TransformParameters)
mLTransform_parameters = Lens.lens (\MLTransform' {parameters} -> parameters) (\s@MLTransform' {} a -> s {parameters = a} :: MLTransform)

-- | The maximum number of times to retry after an @MLTaskRun@ of the machine
-- learning transform fails.
mLTransform_maxRetries :: Lens.Lens' MLTransform (Prelude.Maybe Prelude.Int)
mLTransform_maxRetries = Lens.lens (\MLTransform' {maxRetries} -> maxRetries) (\s@MLTransform' {} a -> s {maxRetries = a} :: MLTransform)

instance Prelude.FromJSON MLTransform where
  parseJSON =
    Prelude.withObject
      "MLTransform"
      ( \x ->
          MLTransform'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "TransformId")
            Prelude.<*> (x Prelude..:? "Schema" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "CreatedOn")
            Prelude.<*> ( x Prelude..:? "InputRecordTables"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "TransformEncryption")
            Prelude.<*> (x Prelude..:? "Timeout")
            Prelude.<*> (x Prelude..:? "MaxCapacity")
            Prelude.<*> (x Prelude..:? "LastModifiedOn")
            Prelude.<*> (x Prelude..:? "NumberOfWorkers")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Role")
            Prelude.<*> (x Prelude..:? "GlueVersion")
            Prelude.<*> (x Prelude..:? "EvaluationMetrics")
            Prelude.<*> (x Prelude..:? "WorkerType")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "LabelCount")
            Prelude.<*> (x Prelude..:? "Parameters")
            Prelude.<*> (x Prelude..:? "MaxRetries")
      )

instance Prelude.Hashable MLTransform

instance Prelude.NFData MLTransform
