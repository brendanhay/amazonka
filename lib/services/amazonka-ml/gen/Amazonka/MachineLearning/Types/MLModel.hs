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
-- Module      : Amazonka.MachineLearning.Types.MLModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.MLModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types.Algorithm
import Amazonka.MachineLearning.Types.EntityStatus
import Amazonka.MachineLearning.Types.MLModelType
import Amazonka.MachineLearning.Types.RealtimeEndpointInfo
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a @GetMLModel@ operation.
--
-- The content consists of the detailed metadata and the current status of
-- the @MLModel@.
--
-- /See:/ 'newMLModel' smart constructor.
data MLModel = MLModel'
  { -- | A description of the most recent details about accessing the @MLModel@.
    message :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied name or description of the @MLModel@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the training @DataSource@. The @CreateMLModel@ operation uses
    -- the @TrainingDataSourceId@.
    trainingDataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The time of the most recent edit to the @ScoreThreshold@. The time is
    -- expressed in epoch time.
    scoreThresholdLastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The time of the most recent edit to the @MLModel@. The time is expressed
    -- in epoch time.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    finishedAt :: Prelude.Maybe Data.POSIX,
    -- | The ID assigned to the @MLModel@ at creation.
    mLModelId :: Prelude.Maybe Prelude.Text,
    scoreThreshold :: Prelude.Maybe Prelude.Double,
    -- | The current endpoint of the @MLModel@.
    endpointInfo :: Prelude.Maybe RealtimeEndpointInfo,
    -- | The current status of an @MLModel@. This element can have one of the
    -- following values:
    --
    -- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
    --     to create an @MLModel@.
    --
    -- -   @INPROGRESS@ - The creation process is underway.
    --
    -- -   @FAILED@ - The request to create an @MLModel@ didn\'t run to
    --     completion. The model isn\'t usable.
    --
    -- -   @COMPLETED@ - The creation process completed successfully.
    --
    -- -   @DELETED@ - The @MLModel@ is marked as deleted. It isn\'t usable.
    status :: Prelude.Maybe EntityStatus,
    -- | Identifies the @MLModel@ category. The following are the available
    -- types:
    --
    -- -   @REGRESSION@ - Produces a numeric result. For example, \"What price
    --     should a house be listed at?\"
    --
    -- -   @BINARY@ - Produces one of two possible results. For example, \"Is
    --     this a child-friendly web site?\".
    --
    -- -   @MULTICLASS@ - Produces one of several possible results. For
    --     example, \"Is this a HIGH-, LOW-, or MEDIUM-risk trade?\".
    mLModelType :: Prelude.Maybe MLModelType,
    startedAt :: Prelude.Maybe Data.POSIX,
    computeTime :: Prelude.Maybe Prelude.Integer,
    sizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The algorithm used to train the @MLModel@. The following algorithm is
    -- supported:
    --
    -- -   @SGD@ -- Stochastic gradient descent. The goal of @SGD@ is to
    --     minimize the gradient of the loss function.
    algorithm :: Prelude.Maybe Algorithm,
    -- | The time that the @MLModel@ was created. The time is expressed in epoch
    -- time.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The location of the data file or directory in Amazon Simple Storage
    -- Service (Amazon S3).
    inputDataLocationS3 :: Prelude.Maybe Prelude.Text,
    -- | The AWS user account from which the @MLModel@ was created. The account
    -- type can be either an AWS root account or an AWS Identity and Access
    -- Management (IAM) user account.
    createdByIamUser :: Prelude.Maybe Prelude.Text,
    -- | A list of the training parameters in the @MLModel@. The list is
    -- implemented as a map of key-value pairs.
    --
    -- The following is the current set of training parameters:
    --
    -- -   @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model.
    --     Depending on the input data, the size of the model might affect its
    --     performance.
    --
    --     The value is an integer that ranges from @100000@ to @2147483648@.
    --     The default value is @33554432@.
    --
    -- -   @sgd.maxPasses@ - The number of times that the training process
    --     traverses the observations to build the @MLModel@. The value is an
    --     integer that ranges from @1@ to @10000@. The default value is @10@.
    --
    -- -   @sgd.shuffleType@ - Whether Amazon ML shuffles the training data.
    --     Shuffling the data improves a model\'s ability to find the optimal
    --     solution for a variety of data types. The valid values are @auto@
    --     and @none@. The default value is @none@.
    --
    -- -   @sgd.l1RegularizationAmount@ - The coefficient regularization L1
    --     norm, which controls overfitting the data by penalizing large
    --     coefficients. This parameter tends to drive coefficients to zero,
    --     resulting in sparse feature set. If you use this parameter, start by
    --     specifying a small value, such as @1.0E-08@.
    --
    --     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
    --     default is to not use L1 normalization. This parameter can\'t be
    --     used when @L2@ is specified. Use this parameter sparingly.
    --
    -- -   @sgd.l2RegularizationAmount@ - The coefficient regularization L2
    --     norm, which controls overfitting the data by penalizing large
    --     coefficients. This tends to drive coefficients to small, nonzero
    --     values. If you use this parameter, start by specifying a small
    --     value, such as @1.0E-08@.
    --
    --     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
    --     default is to not use L2 normalization. This parameter can\'t be
    --     used when @L1@ is specified. Use this parameter sparingly.
    trainingParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MLModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'mLModel_message' - A description of the most recent details about accessing the @MLModel@.
--
-- 'name', 'mLModel_name' - A user-supplied name or description of the @MLModel@.
--
-- 'trainingDataSourceId', 'mLModel_trainingDataSourceId' - The ID of the training @DataSource@. The @CreateMLModel@ operation uses
-- the @TrainingDataSourceId@.
--
-- 'scoreThresholdLastUpdatedAt', 'mLModel_scoreThresholdLastUpdatedAt' - The time of the most recent edit to the @ScoreThreshold@. The time is
-- expressed in epoch time.
--
-- 'lastUpdatedAt', 'mLModel_lastUpdatedAt' - The time of the most recent edit to the @MLModel@. The time is expressed
-- in epoch time.
--
-- 'finishedAt', 'mLModel_finishedAt' - Undocumented member.
--
-- 'mLModelId', 'mLModel_mLModelId' - The ID assigned to the @MLModel@ at creation.
--
-- 'scoreThreshold', 'mLModel_scoreThreshold' - Undocumented member.
--
-- 'endpointInfo', 'mLModel_endpointInfo' - The current endpoint of the @MLModel@.
--
-- 'status', 'mLModel_status' - The current status of an @MLModel@. This element can have one of the
-- following values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to create an @MLModel@.
--
-- -   @INPROGRESS@ - The creation process is underway.
--
-- -   @FAILED@ - The request to create an @MLModel@ didn\'t run to
--     completion. The model isn\'t usable.
--
-- -   @COMPLETED@ - The creation process completed successfully.
--
-- -   @DELETED@ - The @MLModel@ is marked as deleted. It isn\'t usable.
--
-- 'mLModelType', 'mLModel_mLModelType' - Identifies the @MLModel@ category. The following are the available
-- types:
--
-- -   @REGRESSION@ - Produces a numeric result. For example, \"What price
--     should a house be listed at?\"
--
-- -   @BINARY@ - Produces one of two possible results. For example, \"Is
--     this a child-friendly web site?\".
--
-- -   @MULTICLASS@ - Produces one of several possible results. For
--     example, \"Is this a HIGH-, LOW-, or MEDIUM-risk trade?\".
--
-- 'startedAt', 'mLModel_startedAt' - Undocumented member.
--
-- 'computeTime', 'mLModel_computeTime' - Undocumented member.
--
-- 'sizeInBytes', 'mLModel_sizeInBytes' - Undocumented member.
--
-- 'algorithm', 'mLModel_algorithm' - The algorithm used to train the @MLModel@. The following algorithm is
-- supported:
--
-- -   @SGD@ -- Stochastic gradient descent. The goal of @SGD@ is to
--     minimize the gradient of the loss function.
--
-- 'createdAt', 'mLModel_createdAt' - The time that the @MLModel@ was created. The time is expressed in epoch
-- time.
--
-- 'inputDataLocationS3', 'mLModel_inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
--
-- 'createdByIamUser', 'mLModel_createdByIamUser' - The AWS user account from which the @MLModel@ was created. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
--
-- 'trainingParameters', 'mLModel_trainingParameters' - A list of the training parameters in the @MLModel@. The list is
-- implemented as a map of key-value pairs.
--
-- The following is the current set of training parameters:
--
-- -   @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model.
--     Depending on the input data, the size of the model might affect its
--     performance.
--
--     The value is an integer that ranges from @100000@ to @2147483648@.
--     The default value is @33554432@.
--
-- -   @sgd.maxPasses@ - The number of times that the training process
--     traverses the observations to build the @MLModel@. The value is an
--     integer that ranges from @1@ to @10000@. The default value is @10@.
--
-- -   @sgd.shuffleType@ - Whether Amazon ML shuffles the training data.
--     Shuffling the data improves a model\'s ability to find the optimal
--     solution for a variety of data types. The valid values are @auto@
--     and @none@. The default value is @none@.
--
-- -   @sgd.l1RegularizationAmount@ - The coefficient regularization L1
--     norm, which controls overfitting the data by penalizing large
--     coefficients. This parameter tends to drive coefficients to zero,
--     resulting in sparse feature set. If you use this parameter, start by
--     specifying a small value, such as @1.0E-08@.
--
--     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
--     default is to not use L1 normalization. This parameter can\'t be
--     used when @L2@ is specified. Use this parameter sparingly.
--
-- -   @sgd.l2RegularizationAmount@ - The coefficient regularization L2
--     norm, which controls overfitting the data by penalizing large
--     coefficients. This tends to drive coefficients to small, nonzero
--     values. If you use this parameter, start by specifying a small
--     value, such as @1.0E-08@.
--
--     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
--     default is to not use L2 normalization. This parameter can\'t be
--     used when @L1@ is specified. Use this parameter sparingly.
newMLModel ::
  MLModel
newMLModel =
  MLModel'
    { message = Prelude.Nothing,
      name = Prelude.Nothing,
      trainingDataSourceId = Prelude.Nothing,
      scoreThresholdLastUpdatedAt = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      finishedAt = Prelude.Nothing,
      mLModelId = Prelude.Nothing,
      scoreThreshold = Prelude.Nothing,
      endpointInfo = Prelude.Nothing,
      status = Prelude.Nothing,
      mLModelType = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      computeTime = Prelude.Nothing,
      sizeInBytes = Prelude.Nothing,
      algorithm = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      inputDataLocationS3 = Prelude.Nothing,
      createdByIamUser = Prelude.Nothing,
      trainingParameters = Prelude.Nothing
    }

-- | A description of the most recent details about accessing the @MLModel@.
mLModel_message :: Lens.Lens' MLModel (Prelude.Maybe Prelude.Text)
mLModel_message = Lens.lens (\MLModel' {message} -> message) (\s@MLModel' {} a -> s {message = a} :: MLModel)

-- | A user-supplied name or description of the @MLModel@.
mLModel_name :: Lens.Lens' MLModel (Prelude.Maybe Prelude.Text)
mLModel_name = Lens.lens (\MLModel' {name} -> name) (\s@MLModel' {} a -> s {name = a} :: MLModel)

-- | The ID of the training @DataSource@. The @CreateMLModel@ operation uses
-- the @TrainingDataSourceId@.
mLModel_trainingDataSourceId :: Lens.Lens' MLModel (Prelude.Maybe Prelude.Text)
mLModel_trainingDataSourceId = Lens.lens (\MLModel' {trainingDataSourceId} -> trainingDataSourceId) (\s@MLModel' {} a -> s {trainingDataSourceId = a} :: MLModel)

-- | The time of the most recent edit to the @ScoreThreshold@. The time is
-- expressed in epoch time.
mLModel_scoreThresholdLastUpdatedAt :: Lens.Lens' MLModel (Prelude.Maybe Prelude.UTCTime)
mLModel_scoreThresholdLastUpdatedAt = Lens.lens (\MLModel' {scoreThresholdLastUpdatedAt} -> scoreThresholdLastUpdatedAt) (\s@MLModel' {} a -> s {scoreThresholdLastUpdatedAt = a} :: MLModel) Prelude.. Lens.mapping Data._Time

-- | The time of the most recent edit to the @MLModel@. The time is expressed
-- in epoch time.
mLModel_lastUpdatedAt :: Lens.Lens' MLModel (Prelude.Maybe Prelude.UTCTime)
mLModel_lastUpdatedAt = Lens.lens (\MLModel' {lastUpdatedAt} -> lastUpdatedAt) (\s@MLModel' {} a -> s {lastUpdatedAt = a} :: MLModel) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
mLModel_finishedAt :: Lens.Lens' MLModel (Prelude.Maybe Prelude.UTCTime)
mLModel_finishedAt = Lens.lens (\MLModel' {finishedAt} -> finishedAt) (\s@MLModel' {} a -> s {finishedAt = a} :: MLModel) Prelude.. Lens.mapping Data._Time

-- | The ID assigned to the @MLModel@ at creation.
mLModel_mLModelId :: Lens.Lens' MLModel (Prelude.Maybe Prelude.Text)
mLModel_mLModelId = Lens.lens (\MLModel' {mLModelId} -> mLModelId) (\s@MLModel' {} a -> s {mLModelId = a} :: MLModel)

-- | Undocumented member.
mLModel_scoreThreshold :: Lens.Lens' MLModel (Prelude.Maybe Prelude.Double)
mLModel_scoreThreshold = Lens.lens (\MLModel' {scoreThreshold} -> scoreThreshold) (\s@MLModel' {} a -> s {scoreThreshold = a} :: MLModel)

-- | The current endpoint of the @MLModel@.
mLModel_endpointInfo :: Lens.Lens' MLModel (Prelude.Maybe RealtimeEndpointInfo)
mLModel_endpointInfo = Lens.lens (\MLModel' {endpointInfo} -> endpointInfo) (\s@MLModel' {} a -> s {endpointInfo = a} :: MLModel)

-- | The current status of an @MLModel@. This element can have one of the
-- following values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to create an @MLModel@.
--
-- -   @INPROGRESS@ - The creation process is underway.
--
-- -   @FAILED@ - The request to create an @MLModel@ didn\'t run to
--     completion. The model isn\'t usable.
--
-- -   @COMPLETED@ - The creation process completed successfully.
--
-- -   @DELETED@ - The @MLModel@ is marked as deleted. It isn\'t usable.
mLModel_status :: Lens.Lens' MLModel (Prelude.Maybe EntityStatus)
mLModel_status = Lens.lens (\MLModel' {status} -> status) (\s@MLModel' {} a -> s {status = a} :: MLModel)

-- | Identifies the @MLModel@ category. The following are the available
-- types:
--
-- -   @REGRESSION@ - Produces a numeric result. For example, \"What price
--     should a house be listed at?\"
--
-- -   @BINARY@ - Produces one of two possible results. For example, \"Is
--     this a child-friendly web site?\".
--
-- -   @MULTICLASS@ - Produces one of several possible results. For
--     example, \"Is this a HIGH-, LOW-, or MEDIUM-risk trade?\".
mLModel_mLModelType :: Lens.Lens' MLModel (Prelude.Maybe MLModelType)
mLModel_mLModelType = Lens.lens (\MLModel' {mLModelType} -> mLModelType) (\s@MLModel' {} a -> s {mLModelType = a} :: MLModel)

-- | Undocumented member.
mLModel_startedAt :: Lens.Lens' MLModel (Prelude.Maybe Prelude.UTCTime)
mLModel_startedAt = Lens.lens (\MLModel' {startedAt} -> startedAt) (\s@MLModel' {} a -> s {startedAt = a} :: MLModel) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
mLModel_computeTime :: Lens.Lens' MLModel (Prelude.Maybe Prelude.Integer)
mLModel_computeTime = Lens.lens (\MLModel' {computeTime} -> computeTime) (\s@MLModel' {} a -> s {computeTime = a} :: MLModel)

-- | Undocumented member.
mLModel_sizeInBytes :: Lens.Lens' MLModel (Prelude.Maybe Prelude.Integer)
mLModel_sizeInBytes = Lens.lens (\MLModel' {sizeInBytes} -> sizeInBytes) (\s@MLModel' {} a -> s {sizeInBytes = a} :: MLModel)

-- | The algorithm used to train the @MLModel@. The following algorithm is
-- supported:
--
-- -   @SGD@ -- Stochastic gradient descent. The goal of @SGD@ is to
--     minimize the gradient of the loss function.
mLModel_algorithm :: Lens.Lens' MLModel (Prelude.Maybe Algorithm)
mLModel_algorithm = Lens.lens (\MLModel' {algorithm} -> algorithm) (\s@MLModel' {} a -> s {algorithm = a} :: MLModel)

-- | The time that the @MLModel@ was created. The time is expressed in epoch
-- time.
mLModel_createdAt :: Lens.Lens' MLModel (Prelude.Maybe Prelude.UTCTime)
mLModel_createdAt = Lens.lens (\MLModel' {createdAt} -> createdAt) (\s@MLModel' {} a -> s {createdAt = a} :: MLModel) Prelude.. Lens.mapping Data._Time

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
mLModel_inputDataLocationS3 :: Lens.Lens' MLModel (Prelude.Maybe Prelude.Text)
mLModel_inputDataLocationS3 = Lens.lens (\MLModel' {inputDataLocationS3} -> inputDataLocationS3) (\s@MLModel' {} a -> s {inputDataLocationS3 = a} :: MLModel)

-- | The AWS user account from which the @MLModel@ was created. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
mLModel_createdByIamUser :: Lens.Lens' MLModel (Prelude.Maybe Prelude.Text)
mLModel_createdByIamUser = Lens.lens (\MLModel' {createdByIamUser} -> createdByIamUser) (\s@MLModel' {} a -> s {createdByIamUser = a} :: MLModel)

-- | A list of the training parameters in the @MLModel@. The list is
-- implemented as a map of key-value pairs.
--
-- The following is the current set of training parameters:
--
-- -   @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model.
--     Depending on the input data, the size of the model might affect its
--     performance.
--
--     The value is an integer that ranges from @100000@ to @2147483648@.
--     The default value is @33554432@.
--
-- -   @sgd.maxPasses@ - The number of times that the training process
--     traverses the observations to build the @MLModel@. The value is an
--     integer that ranges from @1@ to @10000@. The default value is @10@.
--
-- -   @sgd.shuffleType@ - Whether Amazon ML shuffles the training data.
--     Shuffling the data improves a model\'s ability to find the optimal
--     solution for a variety of data types. The valid values are @auto@
--     and @none@. The default value is @none@.
--
-- -   @sgd.l1RegularizationAmount@ - The coefficient regularization L1
--     norm, which controls overfitting the data by penalizing large
--     coefficients. This parameter tends to drive coefficients to zero,
--     resulting in sparse feature set. If you use this parameter, start by
--     specifying a small value, such as @1.0E-08@.
--
--     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
--     default is to not use L1 normalization. This parameter can\'t be
--     used when @L2@ is specified. Use this parameter sparingly.
--
-- -   @sgd.l2RegularizationAmount@ - The coefficient regularization L2
--     norm, which controls overfitting the data by penalizing large
--     coefficients. This tends to drive coefficients to small, nonzero
--     values. If you use this parameter, start by specifying a small
--     value, such as @1.0E-08@.
--
--     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
--     default is to not use L2 normalization. This parameter can\'t be
--     used when @L1@ is specified. Use this parameter sparingly.
mLModel_trainingParameters :: Lens.Lens' MLModel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
mLModel_trainingParameters = Lens.lens (\MLModel' {trainingParameters} -> trainingParameters) (\s@MLModel' {} a -> s {trainingParameters = a} :: MLModel) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MLModel where
  parseJSON =
    Data.withObject
      "MLModel"
      ( \x ->
          MLModel'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "TrainingDataSourceId")
            Prelude.<*> (x Data..:? "ScoreThresholdLastUpdatedAt")
            Prelude.<*> (x Data..:? "LastUpdatedAt")
            Prelude.<*> (x Data..:? "FinishedAt")
            Prelude.<*> (x Data..:? "MLModelId")
            Prelude.<*> (x Data..:? "ScoreThreshold")
            Prelude.<*> (x Data..:? "EndpointInfo")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "MLModelType")
            Prelude.<*> (x Data..:? "StartedAt")
            Prelude.<*> (x Data..:? "ComputeTime")
            Prelude.<*> (x Data..:? "SizeInBytes")
            Prelude.<*> (x Data..:? "Algorithm")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "InputDataLocationS3")
            Prelude.<*> (x Data..:? "CreatedByIamUser")
            Prelude.<*> ( x Data..:? "TrainingParameters"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable MLModel where
  hashWithSalt _salt MLModel' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` trainingDataSourceId
      `Prelude.hashWithSalt` scoreThresholdLastUpdatedAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` finishedAt
      `Prelude.hashWithSalt` mLModelId
      `Prelude.hashWithSalt` scoreThreshold
      `Prelude.hashWithSalt` endpointInfo
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` mLModelType
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` computeTime
      `Prelude.hashWithSalt` sizeInBytes
      `Prelude.hashWithSalt` algorithm
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` inputDataLocationS3
      `Prelude.hashWithSalt` createdByIamUser
      `Prelude.hashWithSalt` trainingParameters

instance Prelude.NFData MLModel where
  rnf MLModel' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf trainingDataSourceId
      `Prelude.seq` Prelude.rnf scoreThresholdLastUpdatedAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf finishedAt
      `Prelude.seq` Prelude.rnf mLModelId
      `Prelude.seq` Prelude.rnf scoreThreshold
      `Prelude.seq` Prelude.rnf endpointInfo
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf mLModelType
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf computeTime
      `Prelude.seq` Prelude.rnf sizeInBytes
      `Prelude.seq` Prelude.rnf algorithm
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf inputDataLocationS3
      `Prelude.seq` Prelude.rnf createdByIamUser
      `Prelude.seq` Prelude.rnf trainingParameters
