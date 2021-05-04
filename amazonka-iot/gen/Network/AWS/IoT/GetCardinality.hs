{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCardinality' smart constructor.
data GetCardinality = GetCardinality'
  { -- | The name of the index to search.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The query version.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The field to aggregate.
    aggregationField :: Prelude.Maybe Prelude.Text,
    -- | The search query.
    queryString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetCardinality
newGetCardinality pQueryString_ =
  GetCardinality'
    { indexName = Prelude.Nothing,
      queryVersion = Prelude.Nothing,
      aggregationField = Prelude.Nothing,
      queryString = pQueryString_
    }

-- | The name of the index to search.
getCardinality_indexName :: Lens.Lens' GetCardinality (Prelude.Maybe Prelude.Text)
getCardinality_indexName = Lens.lens (\GetCardinality' {indexName} -> indexName) (\s@GetCardinality' {} a -> s {indexName = a} :: GetCardinality)

-- | The query version.
getCardinality_queryVersion :: Lens.Lens' GetCardinality (Prelude.Maybe Prelude.Text)
getCardinality_queryVersion = Lens.lens (\GetCardinality' {queryVersion} -> queryVersion) (\s@GetCardinality' {} a -> s {queryVersion = a} :: GetCardinality)

-- | The field to aggregate.
getCardinality_aggregationField :: Lens.Lens' GetCardinality (Prelude.Maybe Prelude.Text)
getCardinality_aggregationField = Lens.lens (\GetCardinality' {aggregationField} -> aggregationField) (\s@GetCardinality' {} a -> s {aggregationField = a} :: GetCardinality)

-- | The search query.
getCardinality_queryString :: Lens.Lens' GetCardinality Prelude.Text
getCardinality_queryString = Lens.lens (\GetCardinality' {queryString} -> queryString) (\s@GetCardinality' {} a -> s {queryString = a} :: GetCardinality)

instance Prelude.AWSRequest GetCardinality where
  type Rs GetCardinality = GetCardinalityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCardinalityResponse'
            Prelude.<$> (x Prelude..?> "cardinality")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCardinality

instance Prelude.NFData GetCardinality

instance Prelude.ToHeaders GetCardinality where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON GetCardinality where
  toJSON GetCardinality' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("indexName" Prelude..=) Prelude.<$> indexName,
            ("queryVersion" Prelude..=) Prelude.<$> queryVersion,
            ("aggregationField" Prelude..=)
              Prelude.<$> aggregationField,
            Prelude.Just ("queryString" Prelude..= queryString)
          ]
      )

instance Prelude.ToPath GetCardinality where
  toPath = Prelude.const "/indices/cardinality"

instance Prelude.ToQuery GetCardinality where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCardinalityResponse' smart constructor.
data GetCardinalityResponse = GetCardinalityResponse'
  { -- | The approximate count of unique values that match the query.
    cardinality :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetCardinalityResponse
