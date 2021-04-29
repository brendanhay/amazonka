{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lambda.ListFunctionEventInvokeConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Lambda.ListFunctionEventInvokeConfigs
  ( -- * Creating a Request
    ListFunctionEventInvokeConfigs (..),
    newListFunctionEventInvokeConfigs,

    -- * Request Lenses
    listFunctionEventInvokeConfigs_maxItems,
    listFunctionEventInvokeConfigs_marker,
    listFunctionEventInvokeConfigs_functionName,

    -- * Destructuring the Response
    ListFunctionEventInvokeConfigsResponse (..),
    newListFunctionEventInvokeConfigsResponse,

    -- * Response Lenses
    listFunctionEventInvokeConfigsResponse_nextMarker,
    listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs,
    listFunctionEventInvokeConfigsResponse_httpStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFunctionEventInvokeConfigs' smart constructor.
data ListFunctionEventInvokeConfigs = ListFunctionEventInvokeConfigs'
  { -- | The maximum number of configurations to return.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionEventInvokeConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listFunctionEventInvokeConfigs_maxItems' - The maximum number of configurations to return.
--
-- 'marker', 'listFunctionEventInvokeConfigs_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
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
    { maxItems =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | The maximum number of configurations to return.
listFunctionEventInvokeConfigs_maxItems :: Lens.Lens' ListFunctionEventInvokeConfigs (Prelude.Maybe Prelude.Natural)
listFunctionEventInvokeConfigs_maxItems = Lens.lens (\ListFunctionEventInvokeConfigs' {maxItems} -> maxItems) (\s@ListFunctionEventInvokeConfigs' {} a -> s {maxItems = a} :: ListFunctionEventInvokeConfigs)

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listFunctionEventInvokeConfigs_marker :: Lens.Lens' ListFunctionEventInvokeConfigs (Prelude.Maybe Prelude.Text)
listFunctionEventInvokeConfigs_marker = Lens.lens (\ListFunctionEventInvokeConfigs' {marker} -> marker) (\s@ListFunctionEventInvokeConfigs' {} a -> s {marker = a} :: ListFunctionEventInvokeConfigs)

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

instance
  Pager.AWSPager
    ListFunctionEventInvokeConfigs
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listFunctionEventInvokeConfigsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listFunctionEventInvokeConfigs_marker
          Lens..~ rs
          Lens.^? listFunctionEventInvokeConfigsResponse_nextMarker
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListFunctionEventInvokeConfigs
  where
  type
    Rs ListFunctionEventInvokeConfigs =
      ListFunctionEventInvokeConfigsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionEventInvokeConfigsResponse'
            Prelude.<$> (x Prelude..?> "NextMarker")
            Prelude.<*> ( x Prelude..?> "FunctionEventInvokeConfigs"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFunctionEventInvokeConfigs

instance
  Prelude.NFData
    ListFunctionEventInvokeConfigs

instance
  Prelude.ToHeaders
    ListFunctionEventInvokeConfigs
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    ListFunctionEventInvokeConfigs
  where
  toPath ListFunctionEventInvokeConfigs' {..} =
    Prelude.mconcat
      [ "/2019-09-25/functions/",
        Prelude.toBS functionName,
        "/event-invoke-config/list"
      ]

instance
  Prelude.ToQuery
    ListFunctionEventInvokeConfigs
  where
  toQuery ListFunctionEventInvokeConfigs' {..} =
    Prelude.mconcat
      [ "MaxItems" Prelude.=: maxItems,
        "Marker" Prelude.=: marker
      ]

-- | /See:/ 'newListFunctionEventInvokeConfigsResponse' smart constructor.
data ListFunctionEventInvokeConfigsResponse = ListFunctionEventInvokeConfigsResponse'
  { -- | The pagination token that\'s included if more results are available.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | A list of configurations.
    functionEventInvokeConfigs :: Prelude.Maybe [FunctionEventInvokeConfig],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionEventInvokeConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listFunctionEventInvokeConfigsResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'functionEventInvokeConfigs', 'listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs' - A list of configurations.
--
-- 'httpStatus', 'listFunctionEventInvokeConfigsResponse_httpStatus' - The response's http status code.
newListFunctionEventInvokeConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFunctionEventInvokeConfigsResponse
newListFunctionEventInvokeConfigsResponse
  pHttpStatus_ =
    ListFunctionEventInvokeConfigsResponse'
      { nextMarker =
          Prelude.Nothing,
        functionEventInvokeConfigs =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token that\'s included if more results are available.
listFunctionEventInvokeConfigsResponse_nextMarker :: Lens.Lens' ListFunctionEventInvokeConfigsResponse (Prelude.Maybe Prelude.Text)
listFunctionEventInvokeConfigsResponse_nextMarker = Lens.lens (\ListFunctionEventInvokeConfigsResponse' {nextMarker} -> nextMarker) (\s@ListFunctionEventInvokeConfigsResponse' {} a -> s {nextMarker = a} :: ListFunctionEventInvokeConfigsResponse)

-- | A list of configurations.
listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs :: Lens.Lens' ListFunctionEventInvokeConfigsResponse (Prelude.Maybe [FunctionEventInvokeConfig])
listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs = Lens.lens (\ListFunctionEventInvokeConfigsResponse' {functionEventInvokeConfigs} -> functionEventInvokeConfigs) (\s@ListFunctionEventInvokeConfigsResponse' {} a -> s {functionEventInvokeConfigs = a} :: ListFunctionEventInvokeConfigsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listFunctionEventInvokeConfigsResponse_httpStatus :: Lens.Lens' ListFunctionEventInvokeConfigsResponse Prelude.Int
listFunctionEventInvokeConfigsResponse_httpStatus = Lens.lens (\ListFunctionEventInvokeConfigsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionEventInvokeConfigsResponse' {} a -> s {httpStatus = a} :: ListFunctionEventInvokeConfigsResponse)

instance
  Prelude.NFData
    ListFunctionEventInvokeConfigsResponse
