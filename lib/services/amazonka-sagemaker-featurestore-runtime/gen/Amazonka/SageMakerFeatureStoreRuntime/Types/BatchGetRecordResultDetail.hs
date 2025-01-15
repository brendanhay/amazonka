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
-- Module      : Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordResultDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordResultDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerFeatureStoreRuntime.Types.FeatureValue

-- | The output of Records that have been retrieved in a batch.
--
-- /See:/ 'newBatchGetRecordResultDetail' smart constructor.
data BatchGetRecordResultDetail = BatchGetRecordResultDetail'
  { -- | The @FeatureGroupName@ containing Records you retrieved in a batch.
    featureGroupName :: Prelude.Text,
    -- | The value of the record identifer in string format.
    recordIdentifierValueAsString :: Prelude.Text,
    -- | The @Record@ retrieved.
    record :: Prelude.NonEmpty FeatureValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetRecordResultDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureGroupName', 'batchGetRecordResultDetail_featureGroupName' - The @FeatureGroupName@ containing Records you retrieved in a batch.
--
-- 'recordIdentifierValueAsString', 'batchGetRecordResultDetail_recordIdentifierValueAsString' - The value of the record identifer in string format.
--
-- 'record', 'batchGetRecordResultDetail_record' - The @Record@ retrieved.
newBatchGetRecordResultDetail ::
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'recordIdentifierValueAsString'
  Prelude.Text ->
  -- | 'record'
  Prelude.NonEmpty FeatureValue ->
  BatchGetRecordResultDetail
newBatchGetRecordResultDetail
  pFeatureGroupName_
  pRecordIdentifierValueAsString_
  pRecord_ =
    BatchGetRecordResultDetail'
      { featureGroupName =
          pFeatureGroupName_,
        recordIdentifierValueAsString =
          pRecordIdentifierValueAsString_,
        record = Lens.coerced Lens.# pRecord_
      }

-- | The @FeatureGroupName@ containing Records you retrieved in a batch.
batchGetRecordResultDetail_featureGroupName :: Lens.Lens' BatchGetRecordResultDetail Prelude.Text
batchGetRecordResultDetail_featureGroupName = Lens.lens (\BatchGetRecordResultDetail' {featureGroupName} -> featureGroupName) (\s@BatchGetRecordResultDetail' {} a -> s {featureGroupName = a} :: BatchGetRecordResultDetail)

-- | The value of the record identifer in string format.
batchGetRecordResultDetail_recordIdentifierValueAsString :: Lens.Lens' BatchGetRecordResultDetail Prelude.Text
batchGetRecordResultDetail_recordIdentifierValueAsString = Lens.lens (\BatchGetRecordResultDetail' {recordIdentifierValueAsString} -> recordIdentifierValueAsString) (\s@BatchGetRecordResultDetail' {} a -> s {recordIdentifierValueAsString = a} :: BatchGetRecordResultDetail)

-- | The @Record@ retrieved.
batchGetRecordResultDetail_record :: Lens.Lens' BatchGetRecordResultDetail (Prelude.NonEmpty FeatureValue)
batchGetRecordResultDetail_record = Lens.lens (\BatchGetRecordResultDetail' {record} -> record) (\s@BatchGetRecordResultDetail' {} a -> s {record = a} :: BatchGetRecordResultDetail) Prelude.. Lens.coerced

instance Data.FromJSON BatchGetRecordResultDetail where
  parseJSON =
    Data.withObject
      "BatchGetRecordResultDetail"
      ( \x ->
          BatchGetRecordResultDetail'
            Prelude.<$> (x Data..: "FeatureGroupName")
            Prelude.<*> (x Data..: "RecordIdentifierValueAsString")
            Prelude.<*> (x Data..: "Record")
      )

instance Prelude.Hashable BatchGetRecordResultDetail where
  hashWithSalt _salt BatchGetRecordResultDetail' {..} =
    _salt
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` recordIdentifierValueAsString
      `Prelude.hashWithSalt` record

instance Prelude.NFData BatchGetRecordResultDetail where
  rnf BatchGetRecordResultDetail' {..} =
    Prelude.rnf featureGroupName `Prelude.seq`
      Prelude.rnf recordIdentifierValueAsString `Prelude.seq`
        Prelude.rnf record
