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
-- Module      : Network.AWS.Lambda.ListVersionsByFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html versions>,
-- with the version-specific configuration of each. Lambda returns up to 50
-- versions per call.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListVersionsByFunction
  ( -- * Creating a Request
    ListVersionsByFunction (..),
    newListVersionsByFunction,

    -- * Request Lenses
    listVersionsByFunction_maxItems,
    listVersionsByFunction_marker,
    listVersionsByFunction_functionName,

    -- * Destructuring the Response
    ListVersionsByFunctionResponse (..),
    newListVersionsByFunctionResponse,

    -- * Response Lenses
    listVersionsByFunctionResponse_versions,
    listVersionsByFunctionResponse_nextMarker,
    listVersionsByFunctionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListVersionsByFunction' smart constructor.
data ListVersionsByFunction = ListVersionsByFunction'
  { -- | The maximum number of versions to return.
    maxItems :: Core.Maybe Core.Natural,
    -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Core.Maybe Core.Text,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @MyFunction@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
    --
    -- -   __Partial ARN__ - @123456789012:function:MyFunction@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListVersionsByFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listVersionsByFunction_maxItems' - The maximum number of versions to return.
--
-- 'marker', 'listVersionsByFunction_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'functionName', 'listVersionsByFunction_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newListVersionsByFunction ::
  -- | 'functionName'
  Core.Text ->
  ListVersionsByFunction
newListVersionsByFunction pFunctionName_ =
  ListVersionsByFunction'
    { maxItems = Core.Nothing,
      marker = Core.Nothing,
      functionName = pFunctionName_
    }

-- | The maximum number of versions to return.
listVersionsByFunction_maxItems :: Lens.Lens' ListVersionsByFunction (Core.Maybe Core.Natural)
listVersionsByFunction_maxItems = Lens.lens (\ListVersionsByFunction' {maxItems} -> maxItems) (\s@ListVersionsByFunction' {} a -> s {maxItems = a} :: ListVersionsByFunction)

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listVersionsByFunction_marker :: Lens.Lens' ListVersionsByFunction (Core.Maybe Core.Text)
listVersionsByFunction_marker = Lens.lens (\ListVersionsByFunction' {marker} -> marker) (\s@ListVersionsByFunction' {} a -> s {marker = a} :: ListVersionsByFunction)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
listVersionsByFunction_functionName :: Lens.Lens' ListVersionsByFunction Core.Text
listVersionsByFunction_functionName = Lens.lens (\ListVersionsByFunction' {functionName} -> functionName) (\s@ListVersionsByFunction' {} a -> s {functionName = a} :: ListVersionsByFunction)

instance Core.AWSPager ListVersionsByFunction where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVersionsByFunctionResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listVersionsByFunctionResponse_versions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listVersionsByFunction_marker
          Lens..~ rs
          Lens.^? listVersionsByFunctionResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest ListVersionsByFunction where
  type
    AWSResponse ListVersionsByFunction =
      ListVersionsByFunctionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVersionsByFunctionResponse'
            Core.<$> (x Core..?> "Versions" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListVersionsByFunction

instance Core.NFData ListVersionsByFunction

instance Core.ToHeaders ListVersionsByFunction where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListVersionsByFunction where
  toPath ListVersionsByFunction' {..} =
    Core.mconcat
      [ "/2015-03-31/functions/",
        Core.toBS functionName,
        "/versions"
      ]

instance Core.ToQuery ListVersionsByFunction where
  toQuery ListVersionsByFunction' {..} =
    Core.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListVersionsByFunctionResponse' smart constructor.
data ListVersionsByFunctionResponse = ListVersionsByFunctionResponse'
  { -- | A list of Lambda function versions.
    versions :: Core.Maybe [FunctionConfiguration],
    -- | The pagination token that\'s included if more results are available.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListVersionsByFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versions', 'listVersionsByFunctionResponse_versions' - A list of Lambda function versions.
--
-- 'nextMarker', 'listVersionsByFunctionResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listVersionsByFunctionResponse_httpStatus' - The response's http status code.
newListVersionsByFunctionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListVersionsByFunctionResponse
newListVersionsByFunctionResponse pHttpStatus_ =
  ListVersionsByFunctionResponse'
    { versions =
        Core.Nothing,
      nextMarker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Lambda function versions.
listVersionsByFunctionResponse_versions :: Lens.Lens' ListVersionsByFunctionResponse (Core.Maybe [FunctionConfiguration])
listVersionsByFunctionResponse_versions = Lens.lens (\ListVersionsByFunctionResponse' {versions} -> versions) (\s@ListVersionsByFunctionResponse' {} a -> s {versions = a} :: ListVersionsByFunctionResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that\'s included if more results are available.
listVersionsByFunctionResponse_nextMarker :: Lens.Lens' ListVersionsByFunctionResponse (Core.Maybe Core.Text)
listVersionsByFunctionResponse_nextMarker = Lens.lens (\ListVersionsByFunctionResponse' {nextMarker} -> nextMarker) (\s@ListVersionsByFunctionResponse' {} a -> s {nextMarker = a} :: ListVersionsByFunctionResponse)

-- | The response's http status code.
listVersionsByFunctionResponse_httpStatus :: Lens.Lens' ListVersionsByFunctionResponse Core.Int
listVersionsByFunctionResponse_httpStatus = Lens.lens (\ListVersionsByFunctionResponse' {httpStatus} -> httpStatus) (\s@ListVersionsByFunctionResponse' {} a -> s {httpStatus = a} :: ListVersionsByFunctionResponse)

instance Core.NFData ListVersionsByFunctionResponse
