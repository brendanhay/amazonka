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
-- Module      : Amazonka.Rum.Types.BatchDeleteRumMetricDefinitionsError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.BatchDeleteRumMetricDefinitionsError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines one error caused by a
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_BatchDeleteRumMetricsDefinitions.html BatchCreateRumMetricsDefinitions>
-- operation.
--
-- /See:/ 'newBatchDeleteRumMetricDefinitionsError' smart constructor.
data BatchDeleteRumMetricDefinitionsError = BatchDeleteRumMetricDefinitionsError'
  { -- | The error code.
    errorCode :: Prelude.Text,
    -- | The error message for this metric definition.
    errorMessage :: Prelude.Text,
    -- | The ID of the metric definition that caused this error.
    metricDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteRumMetricDefinitionsError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchDeleteRumMetricDefinitionsError_errorCode' - The error code.
--
-- 'errorMessage', 'batchDeleteRumMetricDefinitionsError_errorMessage' - The error message for this metric definition.
--
-- 'metricDefinitionId', 'batchDeleteRumMetricDefinitionsError_metricDefinitionId' - The ID of the metric definition that caused this error.
newBatchDeleteRumMetricDefinitionsError ::
  -- | 'errorCode'
  Prelude.Text ->
  -- | 'errorMessage'
  Prelude.Text ->
  -- | 'metricDefinitionId'
  Prelude.Text ->
  BatchDeleteRumMetricDefinitionsError
newBatchDeleteRumMetricDefinitionsError
  pErrorCode_
  pErrorMessage_
  pMetricDefinitionId_ =
    BatchDeleteRumMetricDefinitionsError'
      { errorCode =
          pErrorCode_,
        errorMessage = pErrorMessage_,
        metricDefinitionId =
          pMetricDefinitionId_
      }

-- | The error code.
batchDeleteRumMetricDefinitionsError_errorCode :: Lens.Lens' BatchDeleteRumMetricDefinitionsError Prelude.Text
batchDeleteRumMetricDefinitionsError_errorCode = Lens.lens (\BatchDeleteRumMetricDefinitionsError' {errorCode} -> errorCode) (\s@BatchDeleteRumMetricDefinitionsError' {} a -> s {errorCode = a} :: BatchDeleteRumMetricDefinitionsError)

-- | The error message for this metric definition.
batchDeleteRumMetricDefinitionsError_errorMessage :: Lens.Lens' BatchDeleteRumMetricDefinitionsError Prelude.Text
batchDeleteRumMetricDefinitionsError_errorMessage = Lens.lens (\BatchDeleteRumMetricDefinitionsError' {errorMessage} -> errorMessage) (\s@BatchDeleteRumMetricDefinitionsError' {} a -> s {errorMessage = a} :: BatchDeleteRumMetricDefinitionsError)

-- | The ID of the metric definition that caused this error.
batchDeleteRumMetricDefinitionsError_metricDefinitionId :: Lens.Lens' BatchDeleteRumMetricDefinitionsError Prelude.Text
batchDeleteRumMetricDefinitionsError_metricDefinitionId = Lens.lens (\BatchDeleteRumMetricDefinitionsError' {metricDefinitionId} -> metricDefinitionId) (\s@BatchDeleteRumMetricDefinitionsError' {} a -> s {metricDefinitionId = a} :: BatchDeleteRumMetricDefinitionsError)

instance
  Data.FromJSON
    BatchDeleteRumMetricDefinitionsError
  where
  parseJSON =
    Data.withObject
      "BatchDeleteRumMetricDefinitionsError"
      ( \x ->
          BatchDeleteRumMetricDefinitionsError'
            Prelude.<$> (x Data..: "ErrorCode")
            Prelude.<*> (x Data..: "ErrorMessage")
            Prelude.<*> (x Data..: "MetricDefinitionId")
      )

instance
  Prelude.Hashable
    BatchDeleteRumMetricDefinitionsError
  where
  hashWithSalt
    _salt
    BatchDeleteRumMetricDefinitionsError' {..} =
      _salt `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` metricDefinitionId

instance
  Prelude.NFData
    BatchDeleteRumMetricDefinitionsError
  where
  rnf BatchDeleteRumMetricDefinitionsError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf metricDefinitionId
