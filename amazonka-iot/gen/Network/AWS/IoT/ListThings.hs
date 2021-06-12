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
-- Module      : Network.AWS.IoT.ListThings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your things. Use the __attributeName__ and __attributeValue__
-- parameters to filter your things. For example, calling @ListThings@ with
-- attributeName=Color and attributeValue=Red retrieves all things in the
-- registry that contain an attribute __Color__ with the value __Red__.
--
-- You will not be charged for calling this API if an @Access denied@ error
-- is returned. You will also not be charged if no attributes or pagination
-- token was provided in request and no pagination token and no results
-- were returned.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThings
  ( -- * Creating a Request
    ListThings (..),
    newListThings,

    -- * Request Lenses
    listThings_attributeValue,
    listThings_nextToken,
    listThings_maxResults,
    listThings_attributeName,
    listThings_thingTypeName,

    -- * Destructuring the Response
    ListThingsResponse (..),
    newListThingsResponse,

    -- * Response Lenses
    listThingsResponse_nextToken,
    listThingsResponse_things,
    listThingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListThings operation.
--
-- /See:/ 'newListThings' smart constructor.
data ListThings = ListThings'
  { -- | The attribute value used to search for things.
    attributeValue :: Core.Maybe Core.Text,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Core.Maybe Core.Natural,
    -- | The attribute name used to search for things.
    attributeName :: Core.Maybe Core.Text,
    -- | The name of the thing type used to search for things.
    thingTypeName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListThings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'listThings_attributeValue' - The attribute value used to search for things.
--
-- 'nextToken', 'listThings_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listThings_maxResults' - The maximum number of results to return in this operation.
--
-- 'attributeName', 'listThings_attributeName' - The attribute name used to search for things.
--
-- 'thingTypeName', 'listThings_thingTypeName' - The name of the thing type used to search for things.
newListThings ::
  ListThings
newListThings =
  ListThings'
    { attributeValue = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      attributeName = Core.Nothing,
      thingTypeName = Core.Nothing
    }

-- | The attribute value used to search for things.
listThings_attributeValue :: Lens.Lens' ListThings (Core.Maybe Core.Text)
listThings_attributeValue = Lens.lens (\ListThings' {attributeValue} -> attributeValue) (\s@ListThings' {} a -> s {attributeValue = a} :: ListThings)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThings_nextToken :: Lens.Lens' ListThings (Core.Maybe Core.Text)
listThings_nextToken = Lens.lens (\ListThings' {nextToken} -> nextToken) (\s@ListThings' {} a -> s {nextToken = a} :: ListThings)

-- | The maximum number of results to return in this operation.
listThings_maxResults :: Lens.Lens' ListThings (Core.Maybe Core.Natural)
listThings_maxResults = Lens.lens (\ListThings' {maxResults} -> maxResults) (\s@ListThings' {} a -> s {maxResults = a} :: ListThings)

-- | The attribute name used to search for things.
listThings_attributeName :: Lens.Lens' ListThings (Core.Maybe Core.Text)
listThings_attributeName = Lens.lens (\ListThings' {attributeName} -> attributeName) (\s@ListThings' {} a -> s {attributeName = a} :: ListThings)

-- | The name of the thing type used to search for things.
listThings_thingTypeName :: Lens.Lens' ListThings (Core.Maybe Core.Text)
listThings_thingTypeName = Lens.lens (\ListThings' {thingTypeName} -> thingTypeName) (\s@ListThings' {} a -> s {thingTypeName = a} :: ListThings)

instance Core.AWSPager ListThings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingsResponse_things Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listThings_nextToken
          Lens..~ rs
          Lens.^? listThingsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListThings where
  type AWSResponse ListThings = ListThingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "things" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListThings

instance Core.NFData ListThings

instance Core.ToHeaders ListThings where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListThings where
  toPath = Core.const "/things"

instance Core.ToQuery ListThings where
  toQuery ListThings' {..} =
    Core.mconcat
      [ "attributeValue" Core.=: attributeValue,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "attributeName" Core.=: attributeName,
        "thingTypeName" Core.=: thingTypeName
      ]

-- | The output from the ListThings operation.
--
-- /See:/ 'newListThingsResponse' smart constructor.
data ListThingsResponse = ListThingsResponse'
  { -- | The token to use to get the next set of results. Will not be returned if
    -- operation has returned all results.
    nextToken :: Core.Maybe Core.Text,
    -- | The things.
    things :: Core.Maybe [ThingAttribute],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListThingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingsResponse_nextToken' - The token to use to get the next set of results. Will not be returned if
-- operation has returned all results.
--
-- 'things', 'listThingsResponse_things' - The things.
--
-- 'httpStatus', 'listThingsResponse_httpStatus' - The response's http status code.
newListThingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListThingsResponse
newListThingsResponse pHttpStatus_ =
  ListThingsResponse'
    { nextToken = Core.Nothing,
      things = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results. Will not be returned if
-- operation has returned all results.
listThingsResponse_nextToken :: Lens.Lens' ListThingsResponse (Core.Maybe Core.Text)
listThingsResponse_nextToken = Lens.lens (\ListThingsResponse' {nextToken} -> nextToken) (\s@ListThingsResponse' {} a -> s {nextToken = a} :: ListThingsResponse)

-- | The things.
listThingsResponse_things :: Lens.Lens' ListThingsResponse (Core.Maybe [ThingAttribute])
listThingsResponse_things = Lens.lens (\ListThingsResponse' {things} -> things) (\s@ListThingsResponse' {} a -> s {things = a} :: ListThingsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listThingsResponse_httpStatus :: Lens.Lens' ListThingsResponse Core.Int
listThingsResponse_httpStatus = Lens.lens (\ListThingsResponse' {httpStatus} -> httpStatus) (\s@ListThingsResponse' {} a -> s {httpStatus = a} :: ListThingsResponse)

instance Core.NFData ListThingsResponse
