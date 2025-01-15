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
-- Module      : Amazonka.IoT.ListThingTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing thing types.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListThingTypes>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListThingTypes
  ( -- * Creating a Request
    ListThingTypes (..),
    newListThingTypes,

    -- * Request Lenses
    listThingTypes_maxResults,
    listThingTypes_nextToken,
    listThingTypes_thingTypeName,

    -- * Destructuring the Response
    ListThingTypesResponse (..),
    newListThingTypesResponse,

    -- * Response Lenses
    listThingTypesResponse_nextToken,
    listThingTypesResponse_thingTypes,
    listThingTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ListThingTypes operation.
--
-- /See:/ 'newListThingTypes' smart constructor.
data ListThingTypes = ListThingTypes'
  { -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing type.
    thingTypeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listThingTypes_maxResults' - The maximum number of results to return in this operation.
--
-- 'nextToken', 'listThingTypes_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'thingTypeName', 'listThingTypes_thingTypeName' - The name of the thing type.
newListThingTypes ::
  ListThingTypes
newListThingTypes =
  ListThingTypes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      thingTypeName = Prelude.Nothing
    }

-- | The maximum number of results to return in this operation.
listThingTypes_maxResults :: Lens.Lens' ListThingTypes (Prelude.Maybe Prelude.Natural)
listThingTypes_maxResults = Lens.lens (\ListThingTypes' {maxResults} -> maxResults) (\s@ListThingTypes' {} a -> s {maxResults = a} :: ListThingTypes)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingTypes_nextToken :: Lens.Lens' ListThingTypes (Prelude.Maybe Prelude.Text)
listThingTypes_nextToken = Lens.lens (\ListThingTypes' {nextToken} -> nextToken) (\s@ListThingTypes' {} a -> s {nextToken = a} :: ListThingTypes)

-- | The name of the thing type.
listThingTypes_thingTypeName :: Lens.Lens' ListThingTypes (Prelude.Maybe Prelude.Text)
listThingTypes_thingTypeName = Lens.lens (\ListThingTypes' {thingTypeName} -> thingTypeName) (\s@ListThingTypes' {} a -> s {thingTypeName = a} :: ListThingTypes)

instance Core.AWSPager ListThingTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingTypesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingTypesResponse_thingTypes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listThingTypes_nextToken
              Lens..~ rs
              Lens.^? listThingTypesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListThingTypes where
  type
    AWSResponse ListThingTypes =
      ListThingTypesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingTypesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "thingTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThingTypes where
  hashWithSalt _salt ListThingTypes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` thingTypeName

instance Prelude.NFData ListThingTypes where
  rnf ListThingTypes' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf thingTypeName

instance Data.ToHeaders ListThingTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListThingTypes where
  toPath = Prelude.const "/thing-types"

instance Data.ToQuery ListThingTypes where
  toQuery ListThingTypes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "thingTypeName" Data.=: thingTypeName
      ]

-- | The output for the ListThingTypes operation.
--
-- /See:/ 'newListThingTypesResponse' smart constructor.
data ListThingTypesResponse = ListThingTypesResponse'
  { -- | The token for the next set of results. Will not be returned if operation
    -- has returned all results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The thing types.
    thingTypes :: Prelude.Maybe [ThingTypeDefinition],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingTypesResponse_nextToken' - The token for the next set of results. Will not be returned if operation
-- has returned all results.
--
-- 'thingTypes', 'listThingTypesResponse_thingTypes' - The thing types.
--
-- 'httpStatus', 'listThingTypesResponse_httpStatus' - The response's http status code.
newListThingTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListThingTypesResponse
newListThingTypesResponse pHttpStatus_ =
  ListThingTypesResponse'
    { nextToken =
        Prelude.Nothing,
      thingTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results. Will not be returned if operation
-- has returned all results.
listThingTypesResponse_nextToken :: Lens.Lens' ListThingTypesResponse (Prelude.Maybe Prelude.Text)
listThingTypesResponse_nextToken = Lens.lens (\ListThingTypesResponse' {nextToken} -> nextToken) (\s@ListThingTypesResponse' {} a -> s {nextToken = a} :: ListThingTypesResponse)

-- | The thing types.
listThingTypesResponse_thingTypes :: Lens.Lens' ListThingTypesResponse (Prelude.Maybe [ThingTypeDefinition])
listThingTypesResponse_thingTypes = Lens.lens (\ListThingTypesResponse' {thingTypes} -> thingTypes) (\s@ListThingTypesResponse' {} a -> s {thingTypes = a} :: ListThingTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listThingTypesResponse_httpStatus :: Lens.Lens' ListThingTypesResponse Prelude.Int
listThingTypesResponse_httpStatus = Lens.lens (\ListThingTypesResponse' {httpStatus} -> httpStatus) (\s@ListThingTypesResponse' {} a -> s {httpStatus = a} :: ListThingTypesResponse)

instance Prelude.NFData ListThingTypesResponse where
  rnf ListThingTypesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf thingTypes `Prelude.seq`
        Prelude.rnf httpStatus
