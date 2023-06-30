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
-- Module      : Amazonka.Lambda.ListFunctionUrlConfigs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Lambda function URLs for the specified function.
--
-- This operation returns paginated results.
module Amazonka.Lambda.ListFunctionUrlConfigs
  ( -- * Creating a Request
    ListFunctionUrlConfigs (..),
    newListFunctionUrlConfigs,

    -- * Request Lenses
    listFunctionUrlConfigs_marker,
    listFunctionUrlConfigs_maxItems,
    listFunctionUrlConfigs_functionName,

    -- * Destructuring the Response
    ListFunctionUrlConfigsResponse (..),
    newListFunctionUrlConfigsResponse,

    -- * Response Lenses
    listFunctionUrlConfigsResponse_nextMarker,
    listFunctionUrlConfigsResponse_httpStatus,
    listFunctionUrlConfigsResponse_functionUrlConfigs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFunctionUrlConfigs' smart constructor.
data ListFunctionUrlConfigs = ListFunctionUrlConfigs'
  { -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of function URLs to return in the response. Note that
    -- @ListFunctionUrlConfigs@ returns a maximum of 50 items in each response,
    -- even if you set the number higher.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@.
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionUrlConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listFunctionUrlConfigs_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'maxItems', 'listFunctionUrlConfigs_maxItems' - The maximum number of function URLs to return in the response. Note that
-- @ListFunctionUrlConfigs@ returns a maximum of 50 items in each response,
-- even if you set the number higher.
--
-- 'functionName', 'listFunctionUrlConfigs_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newListFunctionUrlConfigs ::
  -- | 'functionName'
  Prelude.Text ->
  ListFunctionUrlConfigs
newListFunctionUrlConfigs pFunctionName_ =
  ListFunctionUrlConfigs'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listFunctionUrlConfigs_marker :: Lens.Lens' ListFunctionUrlConfigs (Prelude.Maybe Prelude.Text)
listFunctionUrlConfigs_marker = Lens.lens (\ListFunctionUrlConfigs' {marker} -> marker) (\s@ListFunctionUrlConfigs' {} a -> s {marker = a} :: ListFunctionUrlConfigs)

-- | The maximum number of function URLs to return in the response. Note that
-- @ListFunctionUrlConfigs@ returns a maximum of 50 items in each response,
-- even if you set the number higher.
listFunctionUrlConfigs_maxItems :: Lens.Lens' ListFunctionUrlConfigs (Prelude.Maybe Prelude.Natural)
listFunctionUrlConfigs_maxItems = Lens.lens (\ListFunctionUrlConfigs' {maxItems} -> maxItems) (\s@ListFunctionUrlConfigs' {} a -> s {maxItems = a} :: ListFunctionUrlConfigs)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
listFunctionUrlConfigs_functionName :: Lens.Lens' ListFunctionUrlConfigs Prelude.Text
listFunctionUrlConfigs_functionName = Lens.lens (\ListFunctionUrlConfigs' {functionName} -> functionName) (\s@ListFunctionUrlConfigs' {} a -> s {functionName = a} :: ListFunctionUrlConfigs)

instance Core.AWSPager ListFunctionUrlConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFunctionUrlConfigsResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listFunctionUrlConfigsResponse_functionUrlConfigs
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFunctionUrlConfigs_marker
          Lens..~ rs
          Lens.^? listFunctionUrlConfigsResponse_nextMarker
          Prelude.. Lens._Just

instance Core.AWSRequest ListFunctionUrlConfigs where
  type
    AWSResponse ListFunctionUrlConfigs =
      ListFunctionUrlConfigsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionUrlConfigsResponse'
            Prelude.<$> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "FunctionUrlConfigs"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListFunctionUrlConfigs where
  hashWithSalt _salt ListFunctionUrlConfigs' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData ListFunctionUrlConfigs where
  rnf ListFunctionUrlConfigs' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders ListFunctionUrlConfigs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListFunctionUrlConfigs where
  toPath ListFunctionUrlConfigs' {..} =
    Prelude.mconcat
      [ "/2021-10-31/functions/",
        Data.toBS functionName,
        "/urls"
      ]

instance Data.ToQuery ListFunctionUrlConfigs where
  toQuery ListFunctionUrlConfigs' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListFunctionUrlConfigsResponse' smart constructor.
data ListFunctionUrlConfigsResponse = ListFunctionUrlConfigsResponse'
  { -- | The pagination token that\'s included if more results are available.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of function URL configurations.
    functionUrlConfigs :: [FunctionUrlConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionUrlConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listFunctionUrlConfigsResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listFunctionUrlConfigsResponse_httpStatus' - The response's http status code.
--
-- 'functionUrlConfigs', 'listFunctionUrlConfigsResponse_functionUrlConfigs' - A list of function URL configurations.
newListFunctionUrlConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFunctionUrlConfigsResponse
newListFunctionUrlConfigsResponse pHttpStatus_ =
  ListFunctionUrlConfigsResponse'
    { nextMarker =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      functionUrlConfigs = Prelude.mempty
    }

-- | The pagination token that\'s included if more results are available.
listFunctionUrlConfigsResponse_nextMarker :: Lens.Lens' ListFunctionUrlConfigsResponse (Prelude.Maybe Prelude.Text)
listFunctionUrlConfigsResponse_nextMarker = Lens.lens (\ListFunctionUrlConfigsResponse' {nextMarker} -> nextMarker) (\s@ListFunctionUrlConfigsResponse' {} a -> s {nextMarker = a} :: ListFunctionUrlConfigsResponse)

-- | The response's http status code.
listFunctionUrlConfigsResponse_httpStatus :: Lens.Lens' ListFunctionUrlConfigsResponse Prelude.Int
listFunctionUrlConfigsResponse_httpStatus = Lens.lens (\ListFunctionUrlConfigsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionUrlConfigsResponse' {} a -> s {httpStatus = a} :: ListFunctionUrlConfigsResponse)

-- | A list of function URL configurations.
listFunctionUrlConfigsResponse_functionUrlConfigs :: Lens.Lens' ListFunctionUrlConfigsResponse [FunctionUrlConfig]
listFunctionUrlConfigsResponse_functionUrlConfigs = Lens.lens (\ListFunctionUrlConfigsResponse' {functionUrlConfigs} -> functionUrlConfigs) (\s@ListFunctionUrlConfigsResponse' {} a -> s {functionUrlConfigs = a} :: ListFunctionUrlConfigsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListFunctionUrlConfigsResponse
  where
  rnf ListFunctionUrlConfigsResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf functionUrlConfigs
