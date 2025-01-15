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
-- Module      : Amazonka.Rum.Types.BatchCreateRumMetricDefinitionsError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.BatchCreateRumMetricDefinitionsError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rum.Types.MetricDefinitionRequest

-- | A structure that defines one error caused by a
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_BatchCreateRumMetricsDefinitions.html BatchCreateRumMetricsDefinitions>
-- operation.
--
-- /See:/ 'newBatchCreateRumMetricDefinitionsError' smart constructor.
data BatchCreateRumMetricDefinitionsError = BatchCreateRumMetricDefinitionsError'
  { -- | The error code.
    errorCode :: Prelude.Text,
    -- | The error message for this metric definition.
    errorMessage :: Prelude.Text,
    -- | The metric definition that caused this error.
    metricDefinition :: MetricDefinitionRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateRumMetricDefinitionsError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchCreateRumMetricDefinitionsError_errorCode' - The error code.
--
-- 'errorMessage', 'batchCreateRumMetricDefinitionsError_errorMessage' - The error message for this metric definition.
--
-- 'metricDefinition', 'batchCreateRumMetricDefinitionsError_metricDefinition' - The metric definition that caused this error.
newBatchCreateRumMetricDefinitionsError ::
  -- | 'errorCode'
  Prelude.Text ->
  -- | 'errorMessage'
  Prelude.Text ->
  -- | 'metricDefinition'
  MetricDefinitionRequest ->
  BatchCreateRumMetricDefinitionsError
newBatchCreateRumMetricDefinitionsError
  pErrorCode_
  pErrorMessage_
  pMetricDefinition_ =
    BatchCreateRumMetricDefinitionsError'
      { errorCode =
          pErrorCode_,
        errorMessage = pErrorMessage_,
        metricDefinition = pMetricDefinition_
      }

-- | The error code.
batchCreateRumMetricDefinitionsError_errorCode :: Lens.Lens' BatchCreateRumMetricDefinitionsError Prelude.Text
batchCreateRumMetricDefinitionsError_errorCode = Lens.lens (\BatchCreateRumMetricDefinitionsError' {errorCode} -> errorCode) (\s@BatchCreateRumMetricDefinitionsError' {} a -> s {errorCode = a} :: BatchCreateRumMetricDefinitionsError)

-- | The error message for this metric definition.
batchCreateRumMetricDefinitionsError_errorMessage :: Lens.Lens' BatchCreateRumMetricDefinitionsError Prelude.Text
batchCreateRumMetricDefinitionsError_errorMessage = Lens.lens (\BatchCreateRumMetricDefinitionsError' {errorMessage} -> errorMessage) (\s@BatchCreateRumMetricDefinitionsError' {} a -> s {errorMessage = a} :: BatchCreateRumMetricDefinitionsError)

-- | The metric definition that caused this error.
batchCreateRumMetricDefinitionsError_metricDefinition :: Lens.Lens' BatchCreateRumMetricDefinitionsError MetricDefinitionRequest
batchCreateRumMetricDefinitionsError_metricDefinition = Lens.lens (\BatchCreateRumMetricDefinitionsError' {metricDefinition} -> metricDefinition) (\s@BatchCreateRumMetricDefinitionsError' {} a -> s {metricDefinition = a} :: BatchCreateRumMetricDefinitionsError)

instance
  Data.FromJSON
    BatchCreateRumMetricDefinitionsError
  where
  parseJSON =
    Data.withObject
      "BatchCreateRumMetricDefinitionsError"
      ( \x ->
          BatchCreateRumMetricDefinitionsError'
            Prelude.<$> (x Data..: "ErrorCode")
            Prelude.<*> (x Data..: "ErrorMessage")
            Prelude.<*> (x Data..: "MetricDefinition")
      )

instance
  Prelude.Hashable
    BatchCreateRumMetricDefinitionsError
  where
  hashWithSalt
    _salt
    BatchCreateRumMetricDefinitionsError' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` metricDefinition

instance
  Prelude.NFData
    BatchCreateRumMetricDefinitionsError
  where
  rnf BatchCreateRumMetricDefinitionsError' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf metricDefinition
