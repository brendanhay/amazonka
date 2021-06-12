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
-- Module      : Network.AWS.IoT.ListThingTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing thing types.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingTypes
  ( -- * Creating a Request
    ListThingTypes (..),
    newListThingTypes,

    -- * Request Lenses
    listThingTypes_nextToken,
    listThingTypes_maxResults,
    listThingTypes_thingTypeName,

    -- * Destructuring the Response
    ListThingTypesResponse (..),
    newListThingTypesResponse,

    -- * Response Lenses
    listThingTypesResponse_thingTypes,
    listThingTypesResponse_nextToken,
    listThingTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListThingTypes operation.
--
-- /See:/ 'newListThingTypes' smart constructor.
data ListThingTypes = ListThingTypes'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the thing type.
    thingTypeName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListThingTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingTypes_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listThingTypes_maxResults' - The maximum number of results to return in this operation.
--
-- 'thingTypeName', 'listThingTypes_thingTypeName' - The name of the thing type.
newListThingTypes ::
  ListThingTypes
newListThingTypes =
  ListThingTypes'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      thingTypeName = Core.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingTypes_nextToken :: Lens.Lens' ListThingTypes (Core.Maybe Core.Text)
listThingTypes_nextToken = Lens.lens (\ListThingTypes' {nextToken} -> nextToken) (\s@ListThingTypes' {} a -> s {nextToken = a} :: ListThingTypes)

-- | The maximum number of results to return in this operation.
listThingTypes_maxResults :: Lens.Lens' ListThingTypes (Core.Maybe Core.Natural)
listThingTypes_maxResults = Lens.lens (\ListThingTypes' {maxResults} -> maxResults) (\s@ListThingTypes' {} a -> s {maxResults = a} :: ListThingTypes)

-- | The name of the thing type.
listThingTypes_thingTypeName :: Lens.Lens' ListThingTypes (Core.Maybe Core.Text)
listThingTypes_thingTypeName = Lens.lens (\ListThingTypes' {thingTypeName} -> thingTypeName) (\s@ListThingTypes' {} a -> s {thingTypeName = a} :: ListThingTypes)

instance Core.AWSPager ListThingTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingTypesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingTypesResponse_thingTypes Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listThingTypes_nextToken
          Lens..~ rs
          Lens.^? listThingTypesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListThingTypes where
  type
    AWSResponse ListThingTypes =
      ListThingTypesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingTypesResponse'
            Core.<$> (x Core..?> "thingTypes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListThingTypes

instance Core.NFData ListThingTypes

instance Core.ToHeaders ListThingTypes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListThingTypes where
  toPath = Core.const "/thing-types"

instance Core.ToQuery ListThingTypes where
  toQuery ListThingTypes' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "thingTypeName" Core.=: thingTypeName
      ]

-- | The output for the ListThingTypes operation.
--
-- /See:/ 'newListThingTypesResponse' smart constructor.
data ListThingTypesResponse = ListThingTypesResponse'
  { -- | The thing types.
    thingTypes :: Core.Maybe [ThingTypeDefinition],
    -- | The token for the next set of results. Will not be returned if operation
    -- has returned all results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListThingTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingTypes', 'listThingTypesResponse_thingTypes' - The thing types.
--
-- 'nextToken', 'listThingTypesResponse_nextToken' - The token for the next set of results. Will not be returned if operation
-- has returned all results.
--
-- 'httpStatus', 'listThingTypesResponse_httpStatus' - The response's http status code.
newListThingTypesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListThingTypesResponse
newListThingTypesResponse pHttpStatus_ =
  ListThingTypesResponse'
    { thingTypes = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The thing types.
listThingTypesResponse_thingTypes :: Lens.Lens' ListThingTypesResponse (Core.Maybe [ThingTypeDefinition])
listThingTypesResponse_thingTypes = Lens.lens (\ListThingTypesResponse' {thingTypes} -> thingTypes) (\s@ListThingTypesResponse' {} a -> s {thingTypes = a} :: ListThingTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | The token for the next set of results. Will not be returned if operation
-- has returned all results.
listThingTypesResponse_nextToken :: Lens.Lens' ListThingTypesResponse (Core.Maybe Core.Text)
listThingTypesResponse_nextToken = Lens.lens (\ListThingTypesResponse' {nextToken} -> nextToken) (\s@ListThingTypesResponse' {} a -> s {nextToken = a} :: ListThingTypesResponse)

-- | The response's http status code.
listThingTypesResponse_httpStatus :: Lens.Lens' ListThingTypesResponse Core.Int
listThingTypesResponse_httpStatus = Lens.lens (\ListThingTypesResponse' {httpStatus} -> httpStatus) (\s@ListThingTypesResponse' {} a -> s {httpStatus = a} :: ListThingTypesResponse)

instance Core.NFData ListThingTypesResponse
