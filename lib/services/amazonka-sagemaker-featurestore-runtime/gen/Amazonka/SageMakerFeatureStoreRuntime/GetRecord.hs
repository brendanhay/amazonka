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
-- Module      : Amazonka.SageMakerFeatureStoreRuntime.GetRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use for @OnlineStore@ serving from a @FeatureStore@. Only the latest
-- records stored in the @OnlineStore@ can be retrieved. If no Record with
-- @RecordIdentifierValue@ is found, then an empty result is returned.
module Amazonka.SageMakerFeatureStoreRuntime.GetRecord
  ( -- * Creating a Request
    GetRecord (..),
    newGetRecord,

    -- * Request Lenses
    getRecord_featureNames,
    getRecord_featureGroupName,
    getRecord_recordIdentifierValueAsString,

    -- * Destructuring the Response
    GetRecordResponse (..),
    newGetRecordResponse,

    -- * Response Lenses
    getRecordResponse_record,
    getRecordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerFeatureStoreRuntime.Types

-- | /See:/ 'newGetRecord' smart constructor.
data GetRecord = GetRecord'
  { -- | List of names of Features to be retrieved. If not specified, the latest
    -- value for all the Features are returned.
    featureNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the feature group in which you want to put the records.
    featureGroupName :: Prelude.Text,
    -- | The value that corresponds to @RecordIdentifier@ type and uniquely
    -- identifies the record in the @FeatureGroup@.
    recordIdentifierValueAsString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureNames', 'getRecord_featureNames' - List of names of Features to be retrieved. If not specified, the latest
-- value for all the Features are returned.
--
-- 'featureGroupName', 'getRecord_featureGroupName' - The name of the feature group in which you want to put the records.
--
-- 'recordIdentifierValueAsString', 'getRecord_recordIdentifierValueAsString' - The value that corresponds to @RecordIdentifier@ type and uniquely
-- identifies the record in the @FeatureGroup@.
newGetRecord ::
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'recordIdentifierValueAsString'
  Prelude.Text ->
  GetRecord
newGetRecord
  pFeatureGroupName_
  pRecordIdentifierValueAsString_ =
    GetRecord'
      { featureNames = Prelude.Nothing,
        featureGroupName = pFeatureGroupName_,
        recordIdentifierValueAsString =
          pRecordIdentifierValueAsString_
      }

-- | List of names of Features to be retrieved. If not specified, the latest
-- value for all the Features are returned.
getRecord_featureNames :: Lens.Lens' GetRecord (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getRecord_featureNames = Lens.lens (\GetRecord' {featureNames} -> featureNames) (\s@GetRecord' {} a -> s {featureNames = a} :: GetRecord) Prelude.. Lens.mapping Lens.coerced

-- | The name of the feature group in which you want to put the records.
getRecord_featureGroupName :: Lens.Lens' GetRecord Prelude.Text
getRecord_featureGroupName = Lens.lens (\GetRecord' {featureGroupName} -> featureGroupName) (\s@GetRecord' {} a -> s {featureGroupName = a} :: GetRecord)

-- | The value that corresponds to @RecordIdentifier@ type and uniquely
-- identifies the record in the @FeatureGroup@.
getRecord_recordIdentifierValueAsString :: Lens.Lens' GetRecord Prelude.Text
getRecord_recordIdentifierValueAsString = Lens.lens (\GetRecord' {recordIdentifierValueAsString} -> recordIdentifierValueAsString) (\s@GetRecord' {} a -> s {recordIdentifierValueAsString = a} :: GetRecord)

instance Core.AWSRequest GetRecord where
  type AWSResponse GetRecord = GetRecordResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecordResponse'
            Prelude.<$> (x Core..?> "Record")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRecord where
  hashWithSalt _salt GetRecord' {..} =
    _salt `Prelude.hashWithSalt` featureNames
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` recordIdentifierValueAsString

instance Prelude.NFData GetRecord where
  rnf GetRecord' {..} =
    Prelude.rnf featureNames
      `Prelude.seq` Prelude.rnf featureGroupName
      `Prelude.seq` Prelude.rnf recordIdentifierValueAsString

instance Core.ToHeaders GetRecord where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetRecord where
  toPath GetRecord' {..} =
    Prelude.mconcat
      ["/FeatureGroup/", Core.toBS featureGroupName]

instance Core.ToQuery GetRecord where
  toQuery GetRecord' {..} =
    Prelude.mconcat
      [ "FeatureName"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> featureNames),
        "RecordIdentifierValueAsString"
          Core.=: recordIdentifierValueAsString
      ]

-- | /See:/ 'newGetRecordResponse' smart constructor.
data GetRecordResponse = GetRecordResponse'
  { -- | The record you requested. A list of @FeatureValues@.
    record :: Prelude.Maybe (Prelude.NonEmpty FeatureValue),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'record', 'getRecordResponse_record' - The record you requested. A list of @FeatureValues@.
--
-- 'httpStatus', 'getRecordResponse_httpStatus' - The response's http status code.
newGetRecordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecordResponse
newGetRecordResponse pHttpStatus_ =
  GetRecordResponse'
    { record = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The record you requested. A list of @FeatureValues@.
getRecordResponse_record :: Lens.Lens' GetRecordResponse (Prelude.Maybe (Prelude.NonEmpty FeatureValue))
getRecordResponse_record = Lens.lens (\GetRecordResponse' {record} -> record) (\s@GetRecordResponse' {} a -> s {record = a} :: GetRecordResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRecordResponse_httpStatus :: Lens.Lens' GetRecordResponse Prelude.Int
getRecordResponse_httpStatus = Lens.lens (\GetRecordResponse' {httpStatus} -> httpStatus) (\s@GetRecordResponse' {} a -> s {httpStatus = a} :: GetRecordResponse)

instance Prelude.NFData GetRecordResponse where
  rnf GetRecordResponse' {..} =
    Prelude.rnf record
      `Prelude.seq` Prelude.rnf httpStatus
