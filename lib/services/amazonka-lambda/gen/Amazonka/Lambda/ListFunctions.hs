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
-- Module      : Amazonka.Lambda.ListFunctions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Lambda functions, with the version-specific
-- configuration of each. Lambda returns up to 50 functions per call.
--
-- Set @FunctionVersion@ to @ALL@ to include all published versions of each
-- function in addition to the unpublished version.
--
-- The @ListFunctions@ action returns a subset of the FunctionConfiguration
-- fields. To get the additional fields (State, StateReasonCode,
-- StateReason, LastUpdateStatus, LastUpdateStatusReason,
-- LastUpdateStatusReasonCode) for a function or version, use GetFunction.
--
-- This operation returns paginated results.
module Amazonka.Lambda.ListFunctions
  ( -- * Creating a Request
    ListFunctions (..),
    newListFunctions,

    -- * Request Lenses
    listFunctions_marker,
    listFunctions_masterRegion,
    listFunctions_functionVersion,
    listFunctions_maxItems,

    -- * Destructuring the Response
    ListFunctionsResponse (..),
    newListFunctionsResponse,

    -- * Response Lenses
    listFunctionsResponse_functions,
    listFunctionsResponse_nextMarker,
    listFunctionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFunctions' smart constructor.
data ListFunctions = ListFunctions'
  { -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | For Lambda\@Edge functions, the Amazon Web Services Region of the master
    -- function. For example, @us-east-1@ filters the list of functions to only
    -- include Lambda\@Edge functions replicated from a master function in US
    -- East (N. Virginia). If specified, you must set @FunctionVersion@ to
    -- @ALL@.
    masterRegion :: Prelude.Maybe Prelude.Text,
    -- | Set to @ALL@ to include entries for all published versions of each
    -- function.
    functionVersion :: Prelude.Maybe FunctionVersion,
    -- | The maximum number of functions to return in the response. Note that
    -- @ListFunctions@ returns a maximum of 50 items in each response, even if
    -- you set the number higher.
    maxItems :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listFunctions_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'masterRegion', 'listFunctions_masterRegion' - For Lambda\@Edge functions, the Amazon Web Services Region of the master
-- function. For example, @us-east-1@ filters the list of functions to only
-- include Lambda\@Edge functions replicated from a master function in US
-- East (N. Virginia). If specified, you must set @FunctionVersion@ to
-- @ALL@.
--
-- 'functionVersion', 'listFunctions_functionVersion' - Set to @ALL@ to include entries for all published versions of each
-- function.
--
-- 'maxItems', 'listFunctions_maxItems' - The maximum number of functions to return in the response. Note that
-- @ListFunctions@ returns a maximum of 50 items in each response, even if
-- you set the number higher.
newListFunctions ::
  ListFunctions
newListFunctions =
  ListFunctions'
    { marker = Prelude.Nothing,
      masterRegion = Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listFunctions_marker :: Lens.Lens' ListFunctions (Prelude.Maybe Prelude.Text)
listFunctions_marker = Lens.lens (\ListFunctions' {marker} -> marker) (\s@ListFunctions' {} a -> s {marker = a} :: ListFunctions)

-- | For Lambda\@Edge functions, the Amazon Web Services Region of the master
-- function. For example, @us-east-1@ filters the list of functions to only
-- include Lambda\@Edge functions replicated from a master function in US
-- East (N. Virginia). If specified, you must set @FunctionVersion@ to
-- @ALL@.
listFunctions_masterRegion :: Lens.Lens' ListFunctions (Prelude.Maybe Prelude.Text)
listFunctions_masterRegion = Lens.lens (\ListFunctions' {masterRegion} -> masterRegion) (\s@ListFunctions' {} a -> s {masterRegion = a} :: ListFunctions)

-- | Set to @ALL@ to include entries for all published versions of each
-- function.
listFunctions_functionVersion :: Lens.Lens' ListFunctions (Prelude.Maybe FunctionVersion)
listFunctions_functionVersion = Lens.lens (\ListFunctions' {functionVersion} -> functionVersion) (\s@ListFunctions' {} a -> s {functionVersion = a} :: ListFunctions)

-- | The maximum number of functions to return in the response. Note that
-- @ListFunctions@ returns a maximum of 50 items in each response, even if
-- you set the number higher.
listFunctions_maxItems :: Lens.Lens' ListFunctions (Prelude.Maybe Prelude.Natural)
listFunctions_maxItems = Lens.lens (\ListFunctions' {maxItems} -> maxItems) (\s@ListFunctions' {} a -> s {maxItems = a} :: ListFunctions)

instance Core.AWSPager ListFunctions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFunctionsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFunctionsResponse_functions Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFunctions_marker
          Lens..~ rs
          Lens.^? listFunctionsResponse_nextMarker Prelude.. Lens._Just

instance Core.AWSRequest ListFunctions where
  type
    AWSResponse ListFunctions =
      ListFunctionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionsResponse'
            Prelude.<$> (x Data..?> "Functions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFunctions where
  hashWithSalt _salt ListFunctions' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` masterRegion
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListFunctions where
  rnf ListFunctions' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf masterRegion
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf maxItems

instance Data.ToHeaders ListFunctions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListFunctions where
  toPath = Prelude.const "/2015-03-31/functions/"

instance Data.ToQuery ListFunctions where
  toQuery ListFunctions' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MasterRegion" Data.=: masterRegion,
        "FunctionVersion" Data.=: functionVersion,
        "MaxItems" Data.=: maxItems
      ]

-- | A list of Lambda functions.
--
-- /See:/ 'newListFunctionsResponse' smart constructor.
data ListFunctionsResponse = ListFunctionsResponse'
  { -- | A list of Lambda functions.
    functions :: Prelude.Maybe [FunctionConfiguration],
    -- | The pagination token that\'s included if more results are available.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functions', 'listFunctionsResponse_functions' - A list of Lambda functions.
--
-- 'nextMarker', 'listFunctionsResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listFunctionsResponse_httpStatus' - The response's http status code.
newListFunctionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFunctionsResponse
newListFunctionsResponse pHttpStatus_ =
  ListFunctionsResponse'
    { functions = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Lambda functions.
listFunctionsResponse_functions :: Lens.Lens' ListFunctionsResponse (Prelude.Maybe [FunctionConfiguration])
listFunctionsResponse_functions = Lens.lens (\ListFunctionsResponse' {functions} -> functions) (\s@ListFunctionsResponse' {} a -> s {functions = a} :: ListFunctionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s included if more results are available.
listFunctionsResponse_nextMarker :: Lens.Lens' ListFunctionsResponse (Prelude.Maybe Prelude.Text)
listFunctionsResponse_nextMarker = Lens.lens (\ListFunctionsResponse' {nextMarker} -> nextMarker) (\s@ListFunctionsResponse' {} a -> s {nextMarker = a} :: ListFunctionsResponse)

-- | The response's http status code.
listFunctionsResponse_httpStatus :: Lens.Lens' ListFunctionsResponse Prelude.Int
listFunctionsResponse_httpStatus = Lens.lens (\ListFunctionsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionsResponse' {} a -> s {httpStatus = a} :: ListFunctionsResponse)

instance Prelude.NFData ListFunctionsResponse where
  rnf ListFunctionsResponse' {..} =
    Prelude.rnf functions
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
