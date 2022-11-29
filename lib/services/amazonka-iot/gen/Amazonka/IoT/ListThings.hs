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
-- Module      : Amazonka.IoT.ListThings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your things. Use the __attributeName__ and __attributeValue__
-- parameters to filter your things. For example, calling @ListThings@ with
-- attributeName=Color and attributeValue=Red retrieves all things in the
-- registry that contain an attribute __Color__ with the value __Red__. For
-- more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/thing-registry.html#list-things List Things>
-- from the /Amazon Web Services IoT Core Developer Guide/.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListThings>
-- action.
--
-- You will not be charged for calling this API if an @Access denied@ error
-- is returned. You will also not be charged if no attributes or pagination
-- token was provided in request and no pagination token and no results
-- were returned.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListThings
  ( -- * Creating a Request
    ListThings (..),
    newListThings,

    -- * Request Lenses
    listThings_nextToken,
    listThings_attributeValue,
    listThings_thingTypeName,
    listThings_maxResults,
    listThings_usePrefixAttributeValue,
    listThings_attributeName,

    -- * Destructuring the Response
    ListThingsResponse (..),
    newListThingsResponse,

    -- * Response Lenses
    listThingsResponse_nextToken,
    listThingsResponse_things,
    listThingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ListThings operation.
--
-- /See:/ 'newListThings' smart constructor.
data ListThings = ListThings'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The attribute value used to search for things.
    attributeValue :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing type used to search for things.
    thingTypeName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When @true@, the action returns the thing resources with attribute
    -- values that start with the @attributeValue@ provided.
    --
    -- When @false@, or not present, the action returns only the thing
    -- resources with attribute values that match the entire @attributeValue@
    -- provided.
    usePrefixAttributeValue :: Prelude.Maybe Prelude.Bool,
    -- | The attribute name used to search for things.
    attributeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThings_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'attributeValue', 'listThings_attributeValue' - The attribute value used to search for things.
--
-- 'thingTypeName', 'listThings_thingTypeName' - The name of the thing type used to search for things.
--
-- 'maxResults', 'listThings_maxResults' - The maximum number of results to return in this operation.
--
-- 'usePrefixAttributeValue', 'listThings_usePrefixAttributeValue' - When @true@, the action returns the thing resources with attribute
-- values that start with the @attributeValue@ provided.
--
-- When @false@, or not present, the action returns only the thing
-- resources with attribute values that match the entire @attributeValue@
-- provided.
--
-- 'attributeName', 'listThings_attributeName' - The attribute name used to search for things.
newListThings ::
  ListThings
newListThings =
  ListThings'
    { nextToken = Prelude.Nothing,
      attributeValue = Prelude.Nothing,
      thingTypeName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      usePrefixAttributeValue = Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThings_nextToken :: Lens.Lens' ListThings (Prelude.Maybe Prelude.Text)
listThings_nextToken = Lens.lens (\ListThings' {nextToken} -> nextToken) (\s@ListThings' {} a -> s {nextToken = a} :: ListThings)

-- | The attribute value used to search for things.
listThings_attributeValue :: Lens.Lens' ListThings (Prelude.Maybe Prelude.Text)
listThings_attributeValue = Lens.lens (\ListThings' {attributeValue} -> attributeValue) (\s@ListThings' {} a -> s {attributeValue = a} :: ListThings)

-- | The name of the thing type used to search for things.
listThings_thingTypeName :: Lens.Lens' ListThings (Prelude.Maybe Prelude.Text)
listThings_thingTypeName = Lens.lens (\ListThings' {thingTypeName} -> thingTypeName) (\s@ListThings' {} a -> s {thingTypeName = a} :: ListThings)

-- | The maximum number of results to return in this operation.
listThings_maxResults :: Lens.Lens' ListThings (Prelude.Maybe Prelude.Natural)
listThings_maxResults = Lens.lens (\ListThings' {maxResults} -> maxResults) (\s@ListThings' {} a -> s {maxResults = a} :: ListThings)

-- | When @true@, the action returns the thing resources with attribute
-- values that start with the @attributeValue@ provided.
--
-- When @false@, or not present, the action returns only the thing
-- resources with attribute values that match the entire @attributeValue@
-- provided.
listThings_usePrefixAttributeValue :: Lens.Lens' ListThings (Prelude.Maybe Prelude.Bool)
listThings_usePrefixAttributeValue = Lens.lens (\ListThings' {usePrefixAttributeValue} -> usePrefixAttributeValue) (\s@ListThings' {} a -> s {usePrefixAttributeValue = a} :: ListThings)

-- | The attribute name used to search for things.
listThings_attributeName :: Lens.Lens' ListThings (Prelude.Maybe Prelude.Text)
listThings_attributeName = Lens.lens (\ListThings' {attributeName} -> attributeName) (\s@ListThings' {} a -> s {attributeName = a} :: ListThings)

instance Core.AWSPager ListThings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingsResponse_things Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listThings_nextToken
          Lens..~ rs
          Lens.^? listThingsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListThings where
  type AWSResponse ListThings = ListThingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "things" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThings where
  hashWithSalt _salt ListThings' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` attributeValue
      `Prelude.hashWithSalt` thingTypeName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` usePrefixAttributeValue
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData ListThings where
  rnf ListThings' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf attributeValue
      `Prelude.seq` Prelude.rnf thingTypeName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf usePrefixAttributeValue
      `Prelude.seq` Prelude.rnf attributeName

instance Core.ToHeaders ListThings where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListThings where
  toPath = Prelude.const "/things"

instance Core.ToQuery ListThings where
  toQuery ListThings' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "attributeValue" Core.=: attributeValue,
        "thingTypeName" Core.=: thingTypeName,
        "maxResults" Core.=: maxResults,
        "usePrefixAttributeValue"
          Core.=: usePrefixAttributeValue,
        "attributeName" Core.=: attributeName
      ]

-- | The output from the ListThings operation.
--
-- /See:/ 'newListThingsResponse' smart constructor.
data ListThingsResponse = ListThingsResponse'
  { -- | The token to use to get the next set of results. Will not be returned if
    -- operation has returned all results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The things.
    things :: Prelude.Maybe [ThingAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListThingsResponse
newListThingsResponse pHttpStatus_ =
  ListThingsResponse'
    { nextToken = Prelude.Nothing,
      things = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results. Will not be returned if
-- operation has returned all results.
listThingsResponse_nextToken :: Lens.Lens' ListThingsResponse (Prelude.Maybe Prelude.Text)
listThingsResponse_nextToken = Lens.lens (\ListThingsResponse' {nextToken} -> nextToken) (\s@ListThingsResponse' {} a -> s {nextToken = a} :: ListThingsResponse)

-- | The things.
listThingsResponse_things :: Lens.Lens' ListThingsResponse (Prelude.Maybe [ThingAttribute])
listThingsResponse_things = Lens.lens (\ListThingsResponse' {things} -> things) (\s@ListThingsResponse' {} a -> s {things = a} :: ListThingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listThingsResponse_httpStatus :: Lens.Lens' ListThingsResponse Prelude.Int
listThingsResponse_httpStatus = Lens.lens (\ListThingsResponse' {httpStatus} -> httpStatus) (\s@ListThingsResponse' {} a -> s {httpStatus = a} :: ListThingsResponse)

instance Prelude.NFData ListThingsResponse where
  rnf ListThingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf things
      `Prelude.seq` Prelude.rnf httpStatus
