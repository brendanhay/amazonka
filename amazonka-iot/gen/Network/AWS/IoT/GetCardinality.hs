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
-- Module      : Network.AWS.IoT.GetCardinality
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the approximate count of unique values that match the query.
module Network.AWS.IoT.GetCardinality
  ( -- * Creating a Request
    GetCardinality (..),
    newGetCardinality,

    -- * Request Lenses
    getCardinality_indexName,
    getCardinality_queryVersion,
    getCardinality_aggregationField,
    getCardinality_queryString,

    -- * Destructuring the Response
    GetCardinalityResponse (..),
    newGetCardinalityResponse,

    -- * Response Lenses
    getCardinalityResponse_cardinality,
    getCardinalityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCardinality' smart constructor.
data GetCardinality = GetCardinality'
  { -- | The name of the index to search.
    indexName :: Core.Maybe Core.Text,
    -- | The query version.
    queryVersion :: Core.Maybe Core.Text,
    -- | The field to aggregate.
    aggregationField :: Core.Maybe Core.Text,
    -- | The search query.
    queryString :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCardinality' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'getCardinality_indexName' - The name of the index to search.
--
-- 'queryVersion', 'getCardinality_queryVersion' - The query version.
--
-- 'aggregationField', 'getCardinality_aggregationField' - The field to aggregate.
--
-- 'queryString', 'getCardinality_queryString' - The search query.
newGetCardinality ::
  -- | 'queryString'
  Core.Text ->
  GetCardinality
newGetCardinality pQueryString_ =
  GetCardinality'
    { indexName = Core.Nothing,
      queryVersion = Core.Nothing,
      aggregationField = Core.Nothing,
      queryString = pQueryString_
    }

-- | The name of the index to search.
getCardinality_indexName :: Lens.Lens' GetCardinality (Core.Maybe Core.Text)
getCardinality_indexName = Lens.lens (\GetCardinality' {indexName} -> indexName) (\s@GetCardinality' {} a -> s {indexName = a} :: GetCardinality)

-- | The query version.
getCardinality_queryVersion :: Lens.Lens' GetCardinality (Core.Maybe Core.Text)
getCardinality_queryVersion = Lens.lens (\GetCardinality' {queryVersion} -> queryVersion) (\s@GetCardinality' {} a -> s {queryVersion = a} :: GetCardinality)

-- | The field to aggregate.
getCardinality_aggregationField :: Lens.Lens' GetCardinality (Core.Maybe Core.Text)
getCardinality_aggregationField = Lens.lens (\GetCardinality' {aggregationField} -> aggregationField) (\s@GetCardinality' {} a -> s {aggregationField = a} :: GetCardinality)

-- | The search query.
getCardinality_queryString :: Lens.Lens' GetCardinality Core.Text
getCardinality_queryString = Lens.lens (\GetCardinality' {queryString} -> queryString) (\s@GetCardinality' {} a -> s {queryString = a} :: GetCardinality)

instance Core.AWSRequest GetCardinality where
  type
    AWSResponse GetCardinality =
      GetCardinalityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCardinalityResponse'
            Core.<$> (x Core..?> "cardinality")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCardinality

instance Core.NFData GetCardinality

instance Core.ToHeaders GetCardinality where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetCardinality where
  toJSON GetCardinality' {..} =
    Core.object
      ( Core.catMaybes
          [ ("indexName" Core..=) Core.<$> indexName,
            ("queryVersion" Core..=) Core.<$> queryVersion,
            ("aggregationField" Core..=)
              Core.<$> aggregationField,
            Core.Just ("queryString" Core..= queryString)
          ]
      )

instance Core.ToPath GetCardinality where
  toPath = Core.const "/indices/cardinality"

instance Core.ToQuery GetCardinality where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCardinalityResponse' smart constructor.
data GetCardinalityResponse = GetCardinalityResponse'
  { -- | The approximate count of unique values that match the query.
    cardinality :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetCardinalityResponse
newGetCardinalityResponse pHttpStatus_ =
  GetCardinalityResponse'
    { cardinality = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The approximate count of unique values that match the query.
getCardinalityResponse_cardinality :: Lens.Lens' GetCardinalityResponse (Core.Maybe Core.Int)
getCardinalityResponse_cardinality = Lens.lens (\GetCardinalityResponse' {cardinality} -> cardinality) (\s@GetCardinalityResponse' {} a -> s {cardinality = a} :: GetCardinalityResponse)

-- | The response's http status code.
getCardinalityResponse_httpStatus :: Lens.Lens' GetCardinalityResponse Core.Int
getCardinalityResponse_httpStatus = Lens.lens (\GetCardinalityResponse' {httpStatus} -> httpStatus) (\s@GetCardinalityResponse' {} a -> s {httpStatus = a} :: GetCardinalityResponse)

instance Core.NFData GetCardinalityResponse
