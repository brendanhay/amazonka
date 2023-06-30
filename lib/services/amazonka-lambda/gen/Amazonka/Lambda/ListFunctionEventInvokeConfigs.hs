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
-- Module      : Amazonka.Lambda.ListFunctionEventInvokeConfigs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configurations for asynchronous invocation for a
-- function.
--
-- To configure options for asynchronous invocation, use
-- PutFunctionEventInvokeConfig.
--
-- This operation returns paginated results.
module Amazonka.Lambda.ListFunctionEventInvokeConfigs
  ( -- * Creating a Request
    ListFunctionEventInvokeConfigs (..),
    newListFunctionEventInvokeConfigs,

    -- * Request Lenses
    listFunctionEventInvokeConfigs_marker,
    listFunctionEventInvokeConfigs_maxItems,
    listFunctionEventInvokeConfigs_functionName,

    -- * Destructuring the Response
    ListFunctionEventInvokeConfigsResponse (..),
    newListFunctionEventInvokeConfigsResponse,

    -- * Response Lenses
    listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs,
    listFunctionEventInvokeConfigsResponse_nextMarker,
    listFunctionEventInvokeConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFunctionEventInvokeConfigs' smart constructor.
data ListFunctionEventInvokeConfigs = ListFunctionEventInvokeConfigs'
  { -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of configurations to return.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionEventInvokeConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listFunctionEventInvokeConfigs_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'maxItems', 'listFunctionEventInvokeConfigs_maxItems' - The maximum number of configurations to return.
--
-- 'functionName', 'listFunctionEventInvokeConfigs_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newListFunctionEventInvokeConfigs ::
  -- | 'functionName'
  Prelude.Text ->
  ListFunctionEventInvokeConfigs
newListFunctionEventInvokeConfigs pFunctionName_ =
  ListFunctionEventInvokeConfigs'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listFunctionEventInvokeConfigs_marker :: Lens.Lens' ListFunctionEventInvokeConfigs (Prelude.Maybe Prelude.Text)
listFunctionEventInvokeConfigs_marker = Lens.lens (\ListFunctionEventInvokeConfigs' {marker} -> marker) (\s@ListFunctionEventInvokeConfigs' {} a -> s {marker = a} :: ListFunctionEventInvokeConfigs)

-- | The maximum number of configurations to return.
listFunctionEventInvokeConfigs_maxItems :: Lens.Lens' ListFunctionEventInvokeConfigs (Prelude.Maybe Prelude.Natural)
listFunctionEventInvokeConfigs_maxItems = Lens.lens (\ListFunctionEventInvokeConfigs' {maxItems} -> maxItems) (\s@ListFunctionEventInvokeConfigs' {} a -> s {maxItems = a} :: ListFunctionEventInvokeConfigs)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
listFunctionEventInvokeConfigs_functionName :: Lens.Lens' ListFunctionEventInvokeConfigs Prelude.Text
listFunctionEventInvokeConfigs_functionName = Lens.lens (\ListFunctionEventInvokeConfigs' {functionName} -> functionName) (\s@ListFunctionEventInvokeConfigs' {} a -> s {functionName = a} :: ListFunctionEventInvokeConfigs)

instance Core.AWSPager ListFunctionEventInvokeConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFunctionEventInvokeConfigsResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFunctionEventInvokeConfigs_marker
          Lens..~ rs
          Lens.^? listFunctionEventInvokeConfigsResponse_nextMarker
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListFunctionEventInvokeConfigs
  where
  type
    AWSResponse ListFunctionEventInvokeConfigs =
      ListFunctionEventInvokeConfigsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionEventInvokeConfigsResponse'
            Prelude.<$> ( x
                            Data..?> "FunctionEventInvokeConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFunctionEventInvokeConfigs
  where
  hashWithSalt
    _salt
    ListFunctionEventInvokeConfigs' {..} =
      _salt
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxItems
        `Prelude.hashWithSalt` functionName

instance
  Prelude.NFData
    ListFunctionEventInvokeConfigs
  where
  rnf ListFunctionEventInvokeConfigs' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf functionName

instance
  Data.ToHeaders
    ListFunctionEventInvokeConfigs
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListFunctionEventInvokeConfigs where
  toPath ListFunctionEventInvokeConfigs' {..} =
    Prelude.mconcat
      [ "/2019-09-25/functions/",
        Data.toBS functionName,
        "/event-invoke-config/list"
      ]

instance Data.ToQuery ListFunctionEventInvokeConfigs where
  toQuery ListFunctionEventInvokeConfigs' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListFunctionEventInvokeConfigsResponse' smart constructor.
data ListFunctionEventInvokeConfigsResponse = ListFunctionEventInvokeConfigsResponse'
  { -- | A list of configurations.
    functionEventInvokeConfigs :: Prelude.Maybe [FunctionEventInvokeConfig],
    -- | The pagination token that\'s included if more results are available.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionEventInvokeConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionEventInvokeConfigs', 'listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs' - A list of configurations.
--
-- 'nextMarker', 'listFunctionEventInvokeConfigsResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listFunctionEventInvokeConfigsResponse_httpStatus' - The response's http status code.
newListFunctionEventInvokeConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFunctionEventInvokeConfigsResponse
newListFunctionEventInvokeConfigsResponse
  pHttpStatus_ =
    ListFunctionEventInvokeConfigsResponse'
      { functionEventInvokeConfigs =
          Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of configurations.
listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs :: Lens.Lens' ListFunctionEventInvokeConfigsResponse (Prelude.Maybe [FunctionEventInvokeConfig])
listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs = Lens.lens (\ListFunctionEventInvokeConfigsResponse' {functionEventInvokeConfigs} -> functionEventInvokeConfigs) (\s@ListFunctionEventInvokeConfigsResponse' {} a -> s {functionEventInvokeConfigs = a} :: ListFunctionEventInvokeConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s included if more results are available.
listFunctionEventInvokeConfigsResponse_nextMarker :: Lens.Lens' ListFunctionEventInvokeConfigsResponse (Prelude.Maybe Prelude.Text)
listFunctionEventInvokeConfigsResponse_nextMarker = Lens.lens (\ListFunctionEventInvokeConfigsResponse' {nextMarker} -> nextMarker) (\s@ListFunctionEventInvokeConfigsResponse' {} a -> s {nextMarker = a} :: ListFunctionEventInvokeConfigsResponse)

-- | The response's http status code.
listFunctionEventInvokeConfigsResponse_httpStatus :: Lens.Lens' ListFunctionEventInvokeConfigsResponse Prelude.Int
listFunctionEventInvokeConfigsResponse_httpStatus = Lens.lens (\ListFunctionEventInvokeConfigsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionEventInvokeConfigsResponse' {} a -> s {httpStatus = a} :: ListFunctionEventInvokeConfigsResponse)

instance
  Prelude.NFData
    ListFunctionEventInvokeConfigsResponse
  where
  rnf ListFunctionEventInvokeConfigsResponse' {..} =
    Prelude.rnf functionEventInvokeConfigs
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
