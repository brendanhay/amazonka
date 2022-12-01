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
-- Module      : Amazonka.IoT.GetCardinality
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the approximate count of unique values that match the query.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetCardinality>
-- action.
module Amazonka.IoT.GetCardinality
  ( -- * Creating a Request
    GetCardinality (..),
    newGetCardinality,

    -- * Request Lenses
    getCardinality_aggregationField,
    getCardinality_indexName,
    getCardinality_queryVersion,
    getCardinality_queryString,

    -- * Destructuring the Response
    GetCardinalityResponse (..),
    newGetCardinalityResponse,

    -- * Response Lenses
    getCardinalityResponse_cardinality,
    getCardinalityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCardinality' smart constructor.
data GetCardinality = GetCardinality'
  { -- | The field to aggregate.
    aggregationField :: Prelude.Maybe Prelude.Text,
    -- | The name of the index to search.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The query version.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The search query string.
    queryString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCardinality' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationField', 'getCardinality_aggregationField' - The field to aggregate.
--
-- 'indexName', 'getCardinality_indexName' - The name of the index to search.
--
-- 'queryVersion', 'getCardinality_queryVersion' - The query version.
--
-- 'queryString', 'getCardinality_queryString' - The search query string.
newGetCardinality ::
  -- | 'queryString'
  Prelude.Text ->
  GetCardinality
newGetCardinality pQueryString_ =
  GetCardinality'
    { aggregationField = Prelude.Nothing,
      indexName = Prelude.Nothing,
      queryVersion = Prelude.Nothing,
      queryString = pQueryString_
    }

-- | The field to aggregate.
getCardinality_aggregationField :: Lens.Lens' GetCardinality (Prelude.Maybe Prelude.Text)
getCardinality_aggregationField = Lens.lens (\GetCardinality' {aggregationField} -> aggregationField) (\s@GetCardinality' {} a -> s {aggregationField = a} :: GetCardinality)

-- | The name of the index to search.
getCardinality_indexName :: Lens.Lens' GetCardinality (Prelude.Maybe Prelude.Text)
getCardinality_indexName = Lens.lens (\GetCardinality' {indexName} -> indexName) (\s@GetCardinality' {} a -> s {indexName = a} :: GetCardinality)

-- | The query version.
getCardinality_queryVersion :: Lens.Lens' GetCardinality (Prelude.Maybe Prelude.Text)
getCardinality_queryVersion = Lens.lens (\GetCardinality' {queryVersion} -> queryVersion) (\s@GetCardinality' {} a -> s {queryVersion = a} :: GetCardinality)

-- | The search query string.
getCardinality_queryString :: Lens.Lens' GetCardinality Prelude.Text
getCardinality_queryString = Lens.lens (\GetCardinality' {queryString} -> queryString) (\s@GetCardinality' {} a -> s {queryString = a} :: GetCardinality)

instance Core.AWSRequest GetCardinality where
  type
    AWSResponse GetCardinality =
      GetCardinalityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCardinalityResponse'
            Prelude.<$> (x Core..?> "cardinality")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCardinality where
  hashWithSalt _salt GetCardinality' {..} =
    _salt `Prelude.hashWithSalt` aggregationField
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` queryVersion
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData GetCardinality where
  rnf GetCardinality' {..} =
    Prelude.rnf aggregationField
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf queryVersion
      `Prelude.seq` Prelude.rnf queryString

instance Core.ToHeaders GetCardinality where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetCardinality where
  toJSON GetCardinality' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("aggregationField" Core..=)
              Prelude.<$> aggregationField,
            ("indexName" Core..=) Prelude.<$> indexName,
            ("queryVersion" Core..=) Prelude.<$> queryVersion,
            Prelude.Just ("queryString" Core..= queryString)
          ]
      )

instance Core.ToPath GetCardinality where
  toPath = Prelude.const "/indices/cardinality"

instance Core.ToQuery GetCardinality where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCardinalityResponse' smart constructor.
data GetCardinalityResponse = GetCardinalityResponse'
  { -- | The approximate count of unique values that match the query.
    cardinality :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCardinalityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cardinality', 'getCardinalityResponse_cardinality' - The approximate count of unique values that match the query.
--
-- 'httpStatus', 'getCardinalityResponse_httpStatus' - The response's http status code.
newGetCardinalityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCardinalityResponse
newGetCardinalityResponse pHttpStatus_ =
  GetCardinalityResponse'
    { cardinality =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The approximate count of unique values that match the query.
getCardinalityResponse_cardinality :: Lens.Lens' GetCardinalityResponse (Prelude.Maybe Prelude.Int)
getCardinalityResponse_cardinality = Lens.lens (\GetCardinalityResponse' {cardinality} -> cardinality) (\s@GetCardinalityResponse' {} a -> s {cardinality = a} :: GetCardinalityResponse)

-- | The response's http status code.
getCardinalityResponse_httpStatus :: Lens.Lens' GetCardinalityResponse Prelude.Int
getCardinalityResponse_httpStatus = Lens.lens (\GetCardinalityResponse' {httpStatus} -> httpStatus) (\s@GetCardinalityResponse' {} a -> s {httpStatus = a} :: GetCardinalityResponse)

instance Prelude.NFData GetCardinalityResponse where
  rnf GetCardinalityResponse' {..} =
    Prelude.rnf cardinality
      `Prelude.seq` Prelude.rnf httpStatus
