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
-- Module      : Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The identifier that identifies the batch of Records you are retrieving
-- in a batch.
--
-- /See:/ 'newBatchGetRecordIdentifier' smart constructor.
data BatchGetRecordIdentifier = BatchGetRecordIdentifier'
  { -- | List of names of Features to be retrieved. If not specified, the latest
    -- value for all the Features are returned.
    featureNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A @FeatureGroupName@ containing Records you are retrieving in a batch.
    featureGroupName :: Prelude.Text,
    -- | The value for a list of record identifiers in string format.
    recordIdentifiersValueAsString :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetRecordIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureNames', 'batchGetRecordIdentifier_featureNames' - List of names of Features to be retrieved. If not specified, the latest
-- value for all the Features are returned.
--
-- 'featureGroupName', 'batchGetRecordIdentifier_featureGroupName' - A @FeatureGroupName@ containing Records you are retrieving in a batch.
--
-- 'recordIdentifiersValueAsString', 'batchGetRecordIdentifier_recordIdentifiersValueAsString' - The value for a list of record identifiers in string format.
newBatchGetRecordIdentifier ::
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'recordIdentifiersValueAsString'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetRecordIdentifier
newBatchGetRecordIdentifier
  pFeatureGroupName_
  pRecordIdentifiersValueAsString_ =
    BatchGetRecordIdentifier'
      { featureNames =
          Prelude.Nothing,
        featureGroupName = pFeatureGroupName_,
        recordIdentifiersValueAsString =
          Lens.coerced
            Lens.# pRecordIdentifiersValueAsString_
      }

-- | List of names of Features to be retrieved. If not specified, the latest
-- value for all the Features are returned.
batchGetRecordIdentifier_featureNames :: Lens.Lens' BatchGetRecordIdentifier (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetRecordIdentifier_featureNames = Lens.lens (\BatchGetRecordIdentifier' {featureNames} -> featureNames) (\s@BatchGetRecordIdentifier' {} a -> s {featureNames = a} :: BatchGetRecordIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | A @FeatureGroupName@ containing Records you are retrieving in a batch.
batchGetRecordIdentifier_featureGroupName :: Lens.Lens' BatchGetRecordIdentifier Prelude.Text
batchGetRecordIdentifier_featureGroupName = Lens.lens (\BatchGetRecordIdentifier' {featureGroupName} -> featureGroupName) (\s@BatchGetRecordIdentifier' {} a -> s {featureGroupName = a} :: BatchGetRecordIdentifier)

-- | The value for a list of record identifiers in string format.
batchGetRecordIdentifier_recordIdentifiersValueAsString :: Lens.Lens' BatchGetRecordIdentifier (Prelude.NonEmpty Prelude.Text)
batchGetRecordIdentifier_recordIdentifiersValueAsString = Lens.lens (\BatchGetRecordIdentifier' {recordIdentifiersValueAsString} -> recordIdentifiersValueAsString) (\s@BatchGetRecordIdentifier' {} a -> s {recordIdentifiersValueAsString = a} :: BatchGetRecordIdentifier) Prelude.. Lens.coerced

instance Data.FromJSON BatchGetRecordIdentifier where
  parseJSON =
    Data.withObject
      "BatchGetRecordIdentifier"
      ( \x ->
          BatchGetRecordIdentifier'
            Prelude.<$> (x Data..:? "FeatureNames")
            Prelude.<*> (x Data..: "FeatureGroupName")
            Prelude.<*> (x Data..: "RecordIdentifiersValueAsString")
      )

instance Prelude.Hashable BatchGetRecordIdentifier where
  hashWithSalt _salt BatchGetRecordIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` featureNames
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` recordIdentifiersValueAsString

instance Prelude.NFData BatchGetRecordIdentifier where
  rnf BatchGetRecordIdentifier' {..} =
    Prelude.rnf featureNames `Prelude.seq`
      Prelude.rnf featureGroupName `Prelude.seq`
        Prelude.rnf recordIdentifiersValueAsString

instance Data.ToJSON BatchGetRecordIdentifier where
  toJSON BatchGetRecordIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FeatureNames" Data..=) Prelude.<$> featureNames,
            Prelude.Just
              ("FeatureGroupName" Data..= featureGroupName),
            Prelude.Just
              ( "RecordIdentifiersValueAsString"
                  Data..= recordIdentifiersValueAsString
              )
          ]
      )
