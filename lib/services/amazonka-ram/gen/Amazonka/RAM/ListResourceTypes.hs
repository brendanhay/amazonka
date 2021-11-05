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
-- Module      : Amazonka.RAM.ListResourceTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the shareable resource types supported by RAM.
module Amazonka.RAM.ListResourceTypes
  ( -- * Creating a Request
    ListResourceTypes (..),
    newListResourceTypes,

    -- * Request Lenses
    listResourceTypes_nextToken,
    listResourceTypes_maxResults,

    -- * Destructuring the Response
    ListResourceTypesResponse (..),
    newListResourceTypesResponse,

    -- * Response Lenses
    listResourceTypesResponse_nextToken,
    listResourceTypesResponse_resourceTypes,
    listResourceTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceTypes' smart constructor.
data ListResourceTypes = ListResourceTypes'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceTypes_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'listResourceTypes_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newListResourceTypes ::
  ListResourceTypes
newListResourceTypes =
  ListResourceTypes'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
listResourceTypes_nextToken :: Lens.Lens' ListResourceTypes (Prelude.Maybe Prelude.Text)
listResourceTypes_nextToken = Lens.lens (\ListResourceTypes' {nextToken} -> nextToken) (\s@ListResourceTypes' {} a -> s {nextToken = a} :: ListResourceTypes)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listResourceTypes_maxResults :: Lens.Lens' ListResourceTypes (Prelude.Maybe Prelude.Natural)
listResourceTypes_maxResults = Lens.lens (\ListResourceTypes' {maxResults} -> maxResults) (\s@ListResourceTypes' {} a -> s {maxResults = a} :: ListResourceTypes)

instance Core.AWSRequest ListResourceTypes where
  type
    AWSResponse ListResourceTypes =
      ListResourceTypesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceTypesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "resourceTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourceTypes

instance Prelude.NFData ListResourceTypes

instance Core.ToHeaders ListResourceTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResourceTypes where
  toJSON ListResourceTypes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListResourceTypes where
  toPath = Prelude.const "/listresourcetypes"

instance Core.ToQuery ListResourceTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceTypesResponse' smart constructor.
data ListResourceTypesResponse = ListResourceTypesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The shareable resource types supported by RAM.
    resourceTypes :: Prelude.Maybe [ServiceNameAndResourceType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceTypesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'resourceTypes', 'listResourceTypesResponse_resourceTypes' - The shareable resource types supported by RAM.
--
-- 'httpStatus', 'listResourceTypesResponse_httpStatus' - The response's http status code.
newListResourceTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceTypesResponse
newListResourceTypesResponse pHttpStatus_ =
  ListResourceTypesResponse'
    { nextToken =
        Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listResourceTypesResponse_nextToken :: Lens.Lens' ListResourceTypesResponse (Prelude.Maybe Prelude.Text)
listResourceTypesResponse_nextToken = Lens.lens (\ListResourceTypesResponse' {nextToken} -> nextToken) (\s@ListResourceTypesResponse' {} a -> s {nextToken = a} :: ListResourceTypesResponse)

-- | The shareable resource types supported by RAM.
listResourceTypesResponse_resourceTypes :: Lens.Lens' ListResourceTypesResponse (Prelude.Maybe [ServiceNameAndResourceType])
listResourceTypesResponse_resourceTypes = Lens.lens (\ListResourceTypesResponse' {resourceTypes} -> resourceTypes) (\s@ListResourceTypesResponse' {} a -> s {resourceTypes = a} :: ListResourceTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourceTypesResponse_httpStatus :: Lens.Lens' ListResourceTypesResponse Prelude.Int
listResourceTypesResponse_httpStatus = Lens.lens (\ListResourceTypesResponse' {httpStatus} -> httpStatus) (\s@ListResourceTypesResponse' {} a -> s {httpStatus = a} :: ListResourceTypesResponse)

instance Prelude.NFData ListResourceTypesResponse
