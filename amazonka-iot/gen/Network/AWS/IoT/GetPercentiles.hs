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
-- Module      : Network.AWS.IoT.GetPercentiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Groups the aggregated values that match the query into percentile
-- groupings. The default percentile groupings are: 1,5,25,50,75,95,99,
-- although you can specify your own when you call @GetPercentiles@. This
-- function returns a value for each percentile group specified (or the
-- default percentile groupings). The percentile group \"1\" contains the
-- aggregated field value that occurs in approximately one percent of the
-- values that match the query. The percentile group \"5\" contains the
-- aggregated field value that occurs in approximately five percent of the
-- values that match the query, and so on. The result is an approximation,
-- the more values that match the query, the more accurate the percentile
-- values.
module Network.AWS.IoT.GetPercentiles
  ( -- * Creating a Request
    GetPercentiles (..),
    newGetPercentiles,

    -- * Request Lenses
    getPercentiles_indexName,
    getPercentiles_queryVersion,
    getPercentiles_percents,
    getPercentiles_aggregationField,
    getPercentiles_queryString,

    -- * Destructuring the Response
    GetPercentilesResponse (..),
    newGetPercentilesResponse,

    -- * Response Lenses
    getPercentilesResponse_percentiles,
    getPercentilesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPercentiles' smart constructor.
data GetPercentiles = GetPercentiles'
  { -- | The name of the index to search.
    indexName :: Core.Maybe Core.Text,
    -- | The query version.
    queryVersion :: Core.Maybe Core.Text,
    -- | The percentile groups returned.
    percents :: Core.Maybe [Core.Double],
    -- | The field to aggregate.
    aggregationField :: Core.Maybe Core.Text,
    -- | The query string.
    queryString :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPercentiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'getPercentiles_indexName' - The name of the index to search.
--
-- 'queryVersion', 'getPercentiles_queryVersion' - The query version.
--
-- 'percents', 'getPercentiles_percents' - The percentile groups returned.
--
-- 'aggregationField', 'getPercentiles_aggregationField' - The field to aggregate.
--
-- 'queryString', 'getPercentiles_queryString' - The query string.
newGetPercentiles ::
  -- | 'queryString'
  Core.Text ->
  GetPercentiles
newGetPercentiles pQueryString_ =
  GetPercentiles'
    { indexName = Core.Nothing,
      queryVersion = Core.Nothing,
      percents = Core.Nothing,
      aggregationField = Core.Nothing,
      queryString = pQueryString_
    }

-- | The name of the index to search.
getPercentiles_indexName :: Lens.Lens' GetPercentiles (Core.Maybe Core.Text)
getPercentiles_indexName = Lens.lens (\GetPercentiles' {indexName} -> indexName) (\s@GetPercentiles' {} a -> s {indexName = a} :: GetPercentiles)

-- | The query version.
getPercentiles_queryVersion :: Lens.Lens' GetPercentiles (Core.Maybe Core.Text)
getPercentiles_queryVersion = Lens.lens (\GetPercentiles' {queryVersion} -> queryVersion) (\s@GetPercentiles' {} a -> s {queryVersion = a} :: GetPercentiles)

-- | The percentile groups returned.
getPercentiles_percents :: Lens.Lens' GetPercentiles (Core.Maybe [Core.Double])
getPercentiles_percents = Lens.lens (\GetPercentiles' {percents} -> percents) (\s@GetPercentiles' {} a -> s {percents = a} :: GetPercentiles) Core.. Lens.mapping Lens._Coerce

-- | The field to aggregate.
getPercentiles_aggregationField :: Lens.Lens' GetPercentiles (Core.Maybe Core.Text)
getPercentiles_aggregationField = Lens.lens (\GetPercentiles' {aggregationField} -> aggregationField) (\s@GetPercentiles' {} a -> s {aggregationField = a} :: GetPercentiles)

-- | The query string.
getPercentiles_queryString :: Lens.Lens' GetPercentiles Core.Text
getPercentiles_queryString = Lens.lens (\GetPercentiles' {queryString} -> queryString) (\s@GetPercentiles' {} a -> s {queryString = a} :: GetPercentiles)

instance Core.AWSRequest GetPercentiles where
  type
    AWSResponse GetPercentiles =
      GetPercentilesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPercentilesResponse'
            Core.<$> (x Core..?> "percentiles" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPercentiles

instance Core.NFData GetPercentiles

instance Core.ToHeaders GetPercentiles where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetPercentiles where
  toJSON GetPercentiles' {..} =
    Core.object
      ( Core.catMaybes
          [ ("indexName" Core..=) Core.<$> indexName,
            ("queryVersion" Core..=) Core.<$> queryVersion,
            ("percents" Core..=) Core.<$> percents,
            ("aggregationField" Core..=)
              Core.<$> aggregationField,
            Core.Just ("queryString" Core..= queryString)
          ]
      )

instance Core.ToPath GetPercentiles where
  toPath = Core.const "/indices/percentiles"

instance Core.ToQuery GetPercentiles where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPercentilesResponse' smart constructor.
data GetPercentilesResponse = GetPercentilesResponse'
  { -- | The percentile values of the aggregated fields.
    percentiles :: Core.Maybe [PercentPair],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPercentilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentiles', 'getPercentilesResponse_percentiles' - The percentile values of the aggregated fields.
--
-- 'httpStatus', 'getPercentilesResponse_httpStatus' - The response's http status code.
newGetPercentilesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPercentilesResponse
newGetPercentilesResponse pHttpStatus_ =
  GetPercentilesResponse'
    { percentiles = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The percentile values of the aggregated fields.
getPercentilesResponse_percentiles :: Lens.Lens' GetPercentilesResponse (Core.Maybe [PercentPair])
getPercentilesResponse_percentiles = Lens.lens (\GetPercentilesResponse' {percentiles} -> percentiles) (\s@GetPercentilesResponse' {} a -> s {percentiles = a} :: GetPercentilesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getPercentilesResponse_httpStatus :: Lens.Lens' GetPercentilesResponse Core.Int
getPercentilesResponse_httpStatus = Lens.lens (\GetPercentilesResponse' {httpStatus} -> httpStatus) (\s@GetPercentilesResponse' {} a -> s {httpStatus = a} :: GetPercentilesResponse)

instance Core.NFData GetPercentilesResponse
