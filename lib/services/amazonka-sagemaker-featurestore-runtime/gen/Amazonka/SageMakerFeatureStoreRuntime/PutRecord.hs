{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerFeatureStoreRuntime.PutRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used for data ingestion into the @FeatureStore@. The @PutRecord@ API
-- writes to both the @OnlineStore@ and @OfflineStore@. If the record is
-- the latest record for the @recordIdentifier@, the record is written to
-- both the @OnlineStore@ and @OfflineStore@. If the record is a historic
-- record, it is written only to the @OfflineStore@.
module Amazonka.SageMakerFeatureStoreRuntime.PutRecord
  ( -- * Creating a Request
    PutRecord (..),
    newPutRecord,

    -- * Request Lenses
    putRecord_targetStores,
    putRecord_featureGroupName,
    putRecord_record,

    -- * Destructuring the Response
    PutRecordResponse (..),
    newPutRecordResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerFeatureStoreRuntime.Types

-- | /See:/ 'newPutRecord' smart constructor.
data PutRecord = PutRecord'
  { -- | A list of stores to which you\'re adding the record. By default, Feature
    -- Store adds the record to all of the stores that you\'re using for the
    -- @FeatureGroup@.
    targetStores :: Prelude.Maybe (Prelude.NonEmpty TargetStore),
    -- | The name of the feature group that you want to insert the record into.
    featureGroupName :: Prelude.Text,
    -- | List of FeatureValues to be inserted. This will be a full over-write. If
    -- you only want to update few of the feature values, do the following:
    --
    -- -   Use @GetRecord@ to retrieve the latest record.
    --
    -- -   Update the record returned from @GetRecord@.
    --
    -- -   Use @PutRecord@ to update feature values.
    record :: Prelude.NonEmpty FeatureValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetStores', 'putRecord_targetStores' - A list of stores to which you\'re adding the record. By default, Feature
-- Store adds the record to all of the stores that you\'re using for the
-- @FeatureGroup@.
--
-- 'featureGroupName', 'putRecord_featureGroupName' - The name of the feature group that you want to insert the record into.
--
-- 'record', 'putRecord_record' - List of FeatureValues to be inserted. This will be a full over-write. If
-- you only want to update few of the feature values, do the following:
--
-- -   Use @GetRecord@ to retrieve the latest record.
--
-- -   Update the record returned from @GetRecord@.
--
-- -   Use @PutRecord@ to update feature values.
newPutRecord ::
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'record'
  Prelude.NonEmpty FeatureValue ->
  PutRecord
newPutRecord pFeatureGroupName_ pRecord_ =
  PutRecord'
    { targetStores = Prelude.Nothing,
      featureGroupName = pFeatureGroupName_,
      record = Lens.coerced Lens.# pRecord_
    }

-- | A list of stores to which you\'re adding the record. By default, Feature
-- Store adds the record to all of the stores that you\'re using for the
-- @FeatureGroup@.
putRecord_targetStores :: Lens.Lens' PutRecord (Prelude.Maybe (Prelude.NonEmpty TargetStore))
putRecord_targetStores = Lens.lens (\PutRecord' {targetStores} -> targetStores) (\s@PutRecord' {} a -> s {targetStores = a} :: PutRecord) Prelude.. Lens.mapping Lens.coerced

-- | The name of the feature group that you want to insert the record into.
putRecord_featureGroupName :: Lens.Lens' PutRecord Prelude.Text
putRecord_featureGroupName = Lens.lens (\PutRecord' {featureGroupName} -> featureGroupName) (\s@PutRecord' {} a -> s {featureGroupName = a} :: PutRecord)

-- | List of FeatureValues to be inserted. This will be a full over-write. If
-- you only want to update few of the feature values, do the following:
--
-- -   Use @GetRecord@ to retrieve the latest record.
--
-- -   Update the record returned from @GetRecord@.
--
-- -   Use @PutRecord@ to update feature values.
putRecord_record :: Lens.Lens' PutRecord (Prelude.NonEmpty FeatureValue)
putRecord_record = Lens.lens (\PutRecord' {record} -> record) (\s@PutRecord' {} a -> s {record = a} :: PutRecord) Prelude.. Lens.coerced

instance Core.AWSRequest PutRecord where
  type AWSResponse PutRecord = PutRecordResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response = Response.receiveNull PutRecordResponse'

instance Prelude.Hashable PutRecord where
  hashWithSalt _salt PutRecord' {..} =
    _salt
      `Prelude.hashWithSalt` targetStores
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` record

instance Prelude.NFData PutRecord where
  rnf PutRecord' {..} =
    Prelude.rnf targetStores `Prelude.seq`
      Prelude.rnf featureGroupName `Prelude.seq`
        Prelude.rnf record

instance Data.ToHeaders PutRecord where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRecord where
  toJSON PutRecord' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetStores" Data..=) Prelude.<$> targetStores,
            Prelude.Just ("Record" Data..= record)
          ]
      )

instance Data.ToPath PutRecord where
  toPath PutRecord' {..} =
    Prelude.mconcat
      ["/FeatureGroup/", Data.toBS featureGroupName]

instance Data.ToQuery PutRecord where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRecordResponse' smart constructor.
data PutRecordResponse = PutRecordResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutRecordResponse ::
  PutRecordResponse
newPutRecordResponse = PutRecordResponse'

instance Prelude.NFData PutRecordResponse where
  rnf _ = ()
