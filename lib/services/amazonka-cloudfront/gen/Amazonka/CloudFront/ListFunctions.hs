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
-- Module      : Amazonka.CloudFront.ListFunctions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all CloudFront functions in your Amazon Web Services
-- account.
--
-- You can optionally apply a filter to return only the functions that are
-- in the specified stage, either @DEVELOPMENT@ or @LIVE@.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Amazonka.CloudFront.ListFunctions
  ( -- * Creating a Request
    ListFunctions (..),
    newListFunctions,

    -- * Request Lenses
    listFunctions_marker,
    listFunctions_maxItems,
    listFunctions_stage,

    -- * Destructuring the Response
    ListFunctionsResponse (..),
    newListFunctionsResponse,

    -- * Response Lenses
    listFunctionsResponse_functionList,
    listFunctionsResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFunctions' smart constructor.
data ListFunctions = ListFunctions'
  { -- | Use this field when paginating results to indicate where to begin in
    -- your list of functions. The response includes functions in the list that
    -- occur after the marker. To get the next page of the list, set this
    -- field\'s value to the value of @NextMarker@ from the current page\'s
    -- response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of functions that you want in the response.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | An optional filter to return only the functions that are in the
    -- specified stage, either @DEVELOPMENT@ or @LIVE@.
    stage :: Prelude.Maybe FunctionStage
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
-- 'marker', 'listFunctions_marker' - Use this field when paginating results to indicate where to begin in
-- your list of functions. The response includes functions in the list that
-- occur after the marker. To get the next page of the list, set this
-- field\'s value to the value of @NextMarker@ from the current page\'s
-- response.
--
-- 'maxItems', 'listFunctions_maxItems' - The maximum number of functions that you want in the response.
--
-- 'stage', 'listFunctions_stage' - An optional filter to return only the functions that are in the
-- specified stage, either @DEVELOPMENT@ or @LIVE@.
newListFunctions ::
  ListFunctions
newListFunctions =
  ListFunctions'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      stage = Prelude.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in
-- your list of functions. The response includes functions in the list that
-- occur after the marker. To get the next page of the list, set this
-- field\'s value to the value of @NextMarker@ from the current page\'s
-- response.
listFunctions_marker :: Lens.Lens' ListFunctions (Prelude.Maybe Prelude.Text)
listFunctions_marker = Lens.lens (\ListFunctions' {marker} -> marker) (\s@ListFunctions' {} a -> s {marker = a} :: ListFunctions)

-- | The maximum number of functions that you want in the response.
listFunctions_maxItems :: Lens.Lens' ListFunctions (Prelude.Maybe Prelude.Text)
listFunctions_maxItems = Lens.lens (\ListFunctions' {maxItems} -> maxItems) (\s@ListFunctions' {} a -> s {maxItems = a} :: ListFunctions)

-- | An optional filter to return only the functions that are in the
-- specified stage, either @DEVELOPMENT@ or @LIVE@.
listFunctions_stage :: Lens.Lens' ListFunctions (Prelude.Maybe FunctionStage)
listFunctions_stage = Lens.lens (\ListFunctions' {stage} -> stage) (\s@ListFunctions' {} a -> s {stage = a} :: ListFunctions)

instance Core.AWSRequest ListFunctions where
  type
    AWSResponse ListFunctions =
      ListFunctionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListFunctionsResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFunctions where
  hashWithSalt _salt ListFunctions' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` stage

instance Prelude.NFData ListFunctions where
  rnf ListFunctions' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf maxItems `Prelude.seq`
        Prelude.rnf stage

instance Data.ToHeaders ListFunctions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListFunctions where
  toPath = Prelude.const "/2020-05-31/function"

instance Data.ToQuery ListFunctions where
  toQuery ListFunctions' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "Stage" Data.=: stage
      ]

-- | /See:/ 'newListFunctionsResponse' smart constructor.
data ListFunctionsResponse = ListFunctionsResponse'
  { -- | A list of CloudFront functions.
    functionList :: Prelude.Maybe FunctionList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionList', 'listFunctionsResponse_functionList' - A list of CloudFront functions.
--
-- 'httpStatus', 'listFunctionsResponse_httpStatus' - The response's http status code.
newListFunctionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFunctionsResponse
newListFunctionsResponse pHttpStatus_ =
  ListFunctionsResponse'
    { functionList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of CloudFront functions.
listFunctionsResponse_functionList :: Lens.Lens' ListFunctionsResponse (Prelude.Maybe FunctionList)
listFunctionsResponse_functionList = Lens.lens (\ListFunctionsResponse' {functionList} -> functionList) (\s@ListFunctionsResponse' {} a -> s {functionList = a} :: ListFunctionsResponse)

-- | The response's http status code.
listFunctionsResponse_httpStatus :: Lens.Lens' ListFunctionsResponse Prelude.Int
listFunctionsResponse_httpStatus = Lens.lens (\ListFunctionsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionsResponse' {} a -> s {httpStatus = a} :: ListFunctionsResponse)

instance Prelude.NFData ListFunctionsResponse where
  rnf ListFunctionsResponse' {..} =
    Prelude.rnf functionList `Prelude.seq`
      Prelude.rnf httpStatus
