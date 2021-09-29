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
-- Module      : Network.AWS.IoT.ListDimensions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the set of dimensions that are defined for your Amazon Web Services
-- accounts.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListDimensions>
-- action.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListDimensions
  ( -- * Creating a Request
    ListDimensions (..),
    newListDimensions,

    -- * Request Lenses
    listDimensions_nextToken,
    listDimensions_maxResults,

    -- * Destructuring the Response
    ListDimensionsResponse (..),
    newListDimensionsResponse,

    -- * Response Lenses
    listDimensionsResponse_nextToken,
    listDimensionsResponse_dimensionNames,
    listDimensionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDimensions' smart constructor.
data ListDimensions = ListDimensions'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve at one time.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDimensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDimensions_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listDimensions_maxResults' - The maximum number of results to retrieve at one time.
newListDimensions ::
  ListDimensions
newListDimensions =
  ListDimensions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results.
listDimensions_nextToken :: Lens.Lens' ListDimensions (Prelude.Maybe Prelude.Text)
listDimensions_nextToken = Lens.lens (\ListDimensions' {nextToken} -> nextToken) (\s@ListDimensions' {} a -> s {nextToken = a} :: ListDimensions)

-- | The maximum number of results to retrieve at one time.
listDimensions_maxResults :: Lens.Lens' ListDimensions (Prelude.Maybe Prelude.Natural)
listDimensions_maxResults = Lens.lens (\ListDimensions' {maxResults} -> maxResults) (\s@ListDimensions' {} a -> s {maxResults = a} :: ListDimensions)

instance Core.AWSPager ListDimensions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDimensionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDimensionsResponse_dimensionNames
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDimensions_nextToken
          Lens..~ rs
          Lens.^? listDimensionsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDimensions where
  type
    AWSResponse ListDimensions =
      ListDimensionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDimensionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "dimensionNames" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDimensions

instance Prelude.NFData ListDimensions

instance Core.ToHeaders ListDimensions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDimensions where
  toPath = Prelude.const "/dimensions"

instance Core.ToQuery ListDimensions where
  toQuery ListDimensions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDimensionsResponse' smart constructor.
data ListDimensionsResponse = ListDimensionsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the names of the defined dimensions. Use @DescribeDimension@
    -- to get details for a dimension.
    dimensionNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDimensionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDimensionsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'dimensionNames', 'listDimensionsResponse_dimensionNames' - A list of the names of the defined dimensions. Use @DescribeDimension@
-- to get details for a dimension.
--
-- 'httpStatus', 'listDimensionsResponse_httpStatus' - The response's http status code.
newListDimensionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDimensionsResponse
newListDimensionsResponse pHttpStatus_ =
  ListDimensionsResponse'
    { nextToken =
        Prelude.Nothing,
      dimensionNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listDimensionsResponse_nextToken :: Lens.Lens' ListDimensionsResponse (Prelude.Maybe Prelude.Text)
listDimensionsResponse_nextToken = Lens.lens (\ListDimensionsResponse' {nextToken} -> nextToken) (\s@ListDimensionsResponse' {} a -> s {nextToken = a} :: ListDimensionsResponse)

-- | A list of the names of the defined dimensions. Use @DescribeDimension@
-- to get details for a dimension.
listDimensionsResponse_dimensionNames :: Lens.Lens' ListDimensionsResponse (Prelude.Maybe [Prelude.Text])
listDimensionsResponse_dimensionNames = Lens.lens (\ListDimensionsResponse' {dimensionNames} -> dimensionNames) (\s@ListDimensionsResponse' {} a -> s {dimensionNames = a} :: ListDimensionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDimensionsResponse_httpStatus :: Lens.Lens' ListDimensionsResponse Prelude.Int
listDimensionsResponse_httpStatus = Lens.lens (\ListDimensionsResponse' {httpStatus} -> httpStatus) (\s@ListDimensionsResponse' {} a -> s {httpStatus = a} :: ListDimensionsResponse)

instance Prelude.NFData ListDimensionsResponse
