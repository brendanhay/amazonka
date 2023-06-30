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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource types that can be shared by RAM.
module Amazonka.RAM.ListResourceTypes
  ( -- * Creating a Request
    ListResourceTypes (..),
    newListResourceTypes,

    -- * Request Lenses
    listResourceTypes_maxResults,
    listResourceTypes_nextToken,
    listResourceTypes_resourceRegionScope,

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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceTypes' smart constructor.
data ListResourceTypes = ListResourceTypes'
  { -- | Specifies the total number of results that you want included on each
    -- page of the response. If you do not include this parameter, it defaults
    -- to a value that is specific to the operation. If additional items exist
    -- beyond the number you specify, the @NextToken@ response element is
    -- returned with a value (not null). Include the specified value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results. Note that the service might return fewer
    -- results than the maximum even when there are more results available. You
    -- should check @NextToken@ after every operation to ensure that you
    -- receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want the results to include only resources that have
    -- the specified scope.
    --
    -- -   @ALL@ – the results include both global and regional resources or
    --     resource types.
    --
    -- -   @GLOBAL@ – the results include only global resources or resource
    --     types.
    --
    -- -   @REGIONAL@ – the results include only regional resources or resource
    --     types.
    --
    -- The default value is @ALL@.
    resourceRegionScope :: Prelude.Maybe ResourceRegionScopeFilter
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
-- 'maxResults', 'listResourceTypes_maxResults' - Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
--
-- 'nextToken', 'listResourceTypes_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'resourceRegionScope', 'listResourceTypes_resourceRegionScope' - Specifies that you want the results to include only resources that have
-- the specified scope.
--
-- -   @ALL@ – the results include both global and regional resources or
--     resource types.
--
-- -   @GLOBAL@ – the results include only global resources or resource
--     types.
--
-- -   @REGIONAL@ – the results include only regional resources or resource
--     types.
--
-- The default value is @ALL@.
newListResourceTypes ::
  ListResourceTypes
newListResourceTypes =
  ListResourceTypes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceRegionScope = Prelude.Nothing
    }

-- | Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
listResourceTypes_maxResults :: Lens.Lens' ListResourceTypes (Prelude.Maybe Prelude.Natural)
listResourceTypes_maxResults = Lens.lens (\ListResourceTypes' {maxResults} -> maxResults) (\s@ListResourceTypes' {} a -> s {maxResults = a} :: ListResourceTypes)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listResourceTypes_nextToken :: Lens.Lens' ListResourceTypes (Prelude.Maybe Prelude.Text)
listResourceTypes_nextToken = Lens.lens (\ListResourceTypes' {nextToken} -> nextToken) (\s@ListResourceTypes' {} a -> s {nextToken = a} :: ListResourceTypes)

-- | Specifies that you want the results to include only resources that have
-- the specified scope.
--
-- -   @ALL@ – the results include both global and regional resources or
--     resource types.
--
-- -   @GLOBAL@ – the results include only global resources or resource
--     types.
--
-- -   @REGIONAL@ – the results include only regional resources or resource
--     types.
--
-- The default value is @ALL@.
listResourceTypes_resourceRegionScope :: Lens.Lens' ListResourceTypes (Prelude.Maybe ResourceRegionScopeFilter)
listResourceTypes_resourceRegionScope = Lens.lens (\ListResourceTypes' {resourceRegionScope} -> resourceRegionScope) (\s@ListResourceTypes' {} a -> s {resourceRegionScope = a} :: ListResourceTypes)

instance Core.AWSRequest ListResourceTypes where
  type
    AWSResponse ListResourceTypes =
      ListResourceTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceTypesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "resourceTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourceTypes where
  hashWithSalt _salt ListResourceTypes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceRegionScope

instance Prelude.NFData ListResourceTypes where
  rnf ListResourceTypes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceRegionScope

instance Data.ToHeaders ListResourceTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResourceTypes where
  toJSON ListResourceTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("resourceRegionScope" Data..=)
              Prelude.<$> resourceRegionScope
          ]
      )

instance Data.ToPath ListResourceTypes where
  toPath = Prelude.const "/listresourcetypes"

instance Data.ToQuery ListResourceTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceTypesResponse' smart constructor.
data ListResourceTypesResponse = ListResourceTypesResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain information about the resource types
    -- that can be shared using RAM.
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
-- 'nextToken', 'listResourceTypesResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'resourceTypes', 'listResourceTypesResponse_resourceTypes' - An array of objects that contain information about the resource types
-- that can be shared using RAM.
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

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
listResourceTypesResponse_nextToken :: Lens.Lens' ListResourceTypesResponse (Prelude.Maybe Prelude.Text)
listResourceTypesResponse_nextToken = Lens.lens (\ListResourceTypesResponse' {nextToken} -> nextToken) (\s@ListResourceTypesResponse' {} a -> s {nextToken = a} :: ListResourceTypesResponse)

-- | An array of objects that contain information about the resource types
-- that can be shared using RAM.
listResourceTypesResponse_resourceTypes :: Lens.Lens' ListResourceTypesResponse (Prelude.Maybe [ServiceNameAndResourceType])
listResourceTypesResponse_resourceTypes = Lens.lens (\ListResourceTypesResponse' {resourceTypes} -> resourceTypes) (\s@ListResourceTypesResponse' {} a -> s {resourceTypes = a} :: ListResourceTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourceTypesResponse_httpStatus :: Lens.Lens' ListResourceTypesResponse Prelude.Int
listResourceTypesResponse_httpStatus = Lens.lens (\ListResourceTypesResponse' {httpStatus} -> httpStatus) (\s@ListResourceTypesResponse' {} a -> s {httpStatus = a} :: ListResourceTypesResponse)

instance Prelude.NFData ListResourceTypesResponse where
  rnf ListResourceTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf httpStatus
