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
-- Module      : Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The error that has occurred when attempting to retrieve a batch of
-- Records.
--
-- /See:/ 'newBatchGetRecordError' smart constructor.
data BatchGetRecordError = BatchGetRecordError'
  { -- | The name of the feature group that the record belongs to.
    featureGroupName :: Prelude.Text,
    -- | The value for the @RecordIdentifier@ in string format of a Record from a
    -- @FeatureGroup@ that is causing an error when attempting to be retrieved.
    recordIdentifierValueAsString :: Prelude.Text,
    -- | The error code of an error that has occured when attempting to retrieve
    -- a batch of Records. For more information on errors, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_feature_store_GetRecord.html#API_feature_store_GetRecord_Errors Errors>.
    errorCode :: Prelude.Text,
    -- | The error message of an error that has occured when attempting to
    -- retrieve a record in the batch.
    errorMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetRecordError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureGroupName', 'batchGetRecordError_featureGroupName' - The name of the feature group that the record belongs to.
--
-- 'recordIdentifierValueAsString', 'batchGetRecordError_recordIdentifierValueAsString' - The value for the @RecordIdentifier@ in string format of a Record from a
-- @FeatureGroup@ that is causing an error when attempting to be retrieved.
--
-- 'errorCode', 'batchGetRecordError_errorCode' - The error code of an error that has occured when attempting to retrieve
-- a batch of Records. For more information on errors, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_feature_store_GetRecord.html#API_feature_store_GetRecord_Errors Errors>.
--
-- 'errorMessage', 'batchGetRecordError_errorMessage' - The error message of an error that has occured when attempting to
-- retrieve a record in the batch.
newBatchGetRecordError ::
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'recordIdentifierValueAsString'
  Prelude.Text ->
  -- | 'errorCode'
  Prelude.Text ->
  -- | 'errorMessage'
  Prelude.Text ->
  BatchGetRecordError
newBatchGetRecordError
  pFeatureGroupName_
  pRecordIdentifierValueAsString_
  pErrorCode_
  pErrorMessage_ =
    BatchGetRecordError'
      { featureGroupName =
          pFeatureGroupName_,
        recordIdentifierValueAsString =
          pRecordIdentifierValueAsString_,
        errorCode = pErrorCode_,
        errorMessage = pErrorMessage_
      }

-- | The name of the feature group that the record belongs to.
batchGetRecordError_featureGroupName :: Lens.Lens' BatchGetRecordError Prelude.Text
batchGetRecordError_featureGroupName = Lens.lens (\BatchGetRecordError' {featureGroupName} -> featureGroupName) (\s@BatchGetRecordError' {} a -> s {featureGroupName = a} :: BatchGetRecordError)

-- | The value for the @RecordIdentifier@ in string format of a Record from a
-- @FeatureGroup@ that is causing an error when attempting to be retrieved.
batchGetRecordError_recordIdentifierValueAsString :: Lens.Lens' BatchGetRecordError Prelude.Text
batchGetRecordError_recordIdentifierValueAsString = Lens.lens (\BatchGetRecordError' {recordIdentifierValueAsString} -> recordIdentifierValueAsString) (\s@BatchGetRecordError' {} a -> s {recordIdentifierValueAsString = a} :: BatchGetRecordError)

-- | The error code of an error that has occured when attempting to retrieve
-- a batch of Records. For more information on errors, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_feature_store_GetRecord.html#API_feature_store_GetRecord_Errors Errors>.
batchGetRecordError_errorCode :: Lens.Lens' BatchGetRecordError Prelude.Text
batchGetRecordError_errorCode = Lens.lens (\BatchGetRecordError' {errorCode} -> errorCode) (\s@BatchGetRecordError' {} a -> s {errorCode = a} :: BatchGetRecordError)

-- | The error message of an error that has occured when attempting to
-- retrieve a record in the batch.
batchGetRecordError_errorMessage :: Lens.Lens' BatchGetRecordError Prelude.Text
batchGetRecordError_errorMessage = Lens.lens (\BatchGetRecordError' {errorMessage} -> errorMessage) (\s@BatchGetRecordError' {} a -> s {errorMessage = a} :: BatchGetRecordError)

instance Data.FromJSON BatchGetRecordError where
  parseJSON =
    Data.withObject
      "BatchGetRecordError"
      ( \x ->
          BatchGetRecordError'
            Prelude.<$> (x Data..: "FeatureGroupName")
            Prelude.<*> (x Data..: "RecordIdentifierValueAsString")
            Prelude.<*> (x Data..: "ErrorCode")
            Prelude.<*> (x Data..: "ErrorMessage")
      )

instance Prelude.Hashable BatchGetRecordError where
  hashWithSalt _salt BatchGetRecordError' {..} =
    _salt
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` recordIdentifierValueAsString
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData BatchGetRecordError where
  rnf BatchGetRecordError' {..} =
    Prelude.rnf featureGroupName
      `Prelude.seq` Prelude.rnf recordIdentifierValueAsString
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
