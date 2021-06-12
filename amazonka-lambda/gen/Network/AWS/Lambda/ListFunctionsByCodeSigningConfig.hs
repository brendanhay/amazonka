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
-- Module      : Network.AWS.Lambda.ListFunctionsByCodeSigningConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the functions that use the specified code signing configuration.
-- You can use this method prior to deleting a code signing configuration,
-- to verify that no functions are using it.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctionsByCodeSigningConfig
  ( -- * Creating a Request
    ListFunctionsByCodeSigningConfig (..),
    newListFunctionsByCodeSigningConfig,

    -- * Request Lenses
    listFunctionsByCodeSigningConfig_maxItems,
    listFunctionsByCodeSigningConfig_marker,
    listFunctionsByCodeSigningConfig_codeSigningConfigArn,

    -- * Destructuring the Response
    ListFunctionsByCodeSigningConfigResponse (..),
    newListFunctionsByCodeSigningConfigResponse,

    -- * Response Lenses
    listFunctionsByCodeSigningConfigResponse_functionArns,
    listFunctionsByCodeSigningConfigResponse_nextMarker,
    listFunctionsByCodeSigningConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFunctionsByCodeSigningConfig' smart constructor.
data ListFunctionsByCodeSigningConfig = ListFunctionsByCodeSigningConfig'
  { -- | Maximum number of items to return.
    maxItems :: Core.Maybe Core.Natural,
    -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Core.Maybe Core.Text,
    -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFunctionsByCodeSigningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listFunctionsByCodeSigningConfig_maxItems' - Maximum number of items to return.
--
-- 'marker', 'listFunctionsByCodeSigningConfig_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'codeSigningConfigArn', 'listFunctionsByCodeSigningConfig_codeSigningConfigArn' - The The Amazon Resource Name (ARN) of the code signing configuration.
newListFunctionsByCodeSigningConfig ::
  -- | 'codeSigningConfigArn'
  Core.Text ->
  ListFunctionsByCodeSigningConfig
newListFunctionsByCodeSigningConfig
  pCodeSigningConfigArn_ =
    ListFunctionsByCodeSigningConfig'
      { maxItems =
          Core.Nothing,
        marker = Core.Nothing,
        codeSigningConfigArn =
          pCodeSigningConfigArn_
      }

-- | Maximum number of items to return.
listFunctionsByCodeSigningConfig_maxItems :: Lens.Lens' ListFunctionsByCodeSigningConfig (Core.Maybe Core.Natural)
listFunctionsByCodeSigningConfig_maxItems = Lens.lens (\ListFunctionsByCodeSigningConfig' {maxItems} -> maxItems) (\s@ListFunctionsByCodeSigningConfig' {} a -> s {maxItems = a} :: ListFunctionsByCodeSigningConfig)

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listFunctionsByCodeSigningConfig_marker :: Lens.Lens' ListFunctionsByCodeSigningConfig (Core.Maybe Core.Text)
listFunctionsByCodeSigningConfig_marker = Lens.lens (\ListFunctionsByCodeSigningConfig' {marker} -> marker) (\s@ListFunctionsByCodeSigningConfig' {} a -> s {marker = a} :: ListFunctionsByCodeSigningConfig)

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
listFunctionsByCodeSigningConfig_codeSigningConfigArn :: Lens.Lens' ListFunctionsByCodeSigningConfig Core.Text
listFunctionsByCodeSigningConfig_codeSigningConfigArn = Lens.lens (\ListFunctionsByCodeSigningConfig' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@ListFunctionsByCodeSigningConfig' {} a -> s {codeSigningConfigArn = a} :: ListFunctionsByCodeSigningConfig)

instance
  Core.AWSPager
    ListFunctionsByCodeSigningConfig
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFunctionsByCodeSigningConfigResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listFunctionsByCodeSigningConfigResponse_functionArns
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listFunctionsByCodeSigningConfig_marker
          Lens..~ rs
          Lens.^? listFunctionsByCodeSigningConfigResponse_nextMarker
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListFunctionsByCodeSigningConfig
  where
  type
    AWSResponse ListFunctionsByCodeSigningConfig =
      ListFunctionsByCodeSigningConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionsByCodeSigningConfigResponse'
            Core.<$> (x Core..?> "FunctionArns" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListFunctionsByCodeSigningConfig

instance Core.NFData ListFunctionsByCodeSigningConfig

instance
  Core.ToHeaders
    ListFunctionsByCodeSigningConfig
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListFunctionsByCodeSigningConfig where
  toPath ListFunctionsByCodeSigningConfig' {..} =
    Core.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Core.toBS codeSigningConfigArn,
        "/functions"
      ]

instance
  Core.ToQuery
    ListFunctionsByCodeSigningConfig
  where
  toQuery ListFunctionsByCodeSigningConfig' {..} =
    Core.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListFunctionsByCodeSigningConfigResponse' smart constructor.
data ListFunctionsByCodeSigningConfigResponse = ListFunctionsByCodeSigningConfigResponse'
  { -- | The function ARNs.
    functionArns :: Core.Maybe [Core.Text],
    -- | The pagination token that\'s included if more results are available.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFunctionsByCodeSigningConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionArns', 'listFunctionsByCodeSigningConfigResponse_functionArns' - The function ARNs.
--
-- 'nextMarker', 'listFunctionsByCodeSigningConfigResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listFunctionsByCodeSigningConfigResponse_httpStatus' - The response's http status code.
newListFunctionsByCodeSigningConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListFunctionsByCodeSigningConfigResponse
newListFunctionsByCodeSigningConfigResponse
  pHttpStatus_ =
    ListFunctionsByCodeSigningConfigResponse'
      { functionArns =
          Core.Nothing,
        nextMarker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The function ARNs.
listFunctionsByCodeSigningConfigResponse_functionArns :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse (Core.Maybe [Core.Text])
listFunctionsByCodeSigningConfigResponse_functionArns = Lens.lens (\ListFunctionsByCodeSigningConfigResponse' {functionArns} -> functionArns) (\s@ListFunctionsByCodeSigningConfigResponse' {} a -> s {functionArns = a} :: ListFunctionsByCodeSigningConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that\'s included if more results are available.
listFunctionsByCodeSigningConfigResponse_nextMarker :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse (Core.Maybe Core.Text)
listFunctionsByCodeSigningConfigResponse_nextMarker = Lens.lens (\ListFunctionsByCodeSigningConfigResponse' {nextMarker} -> nextMarker) (\s@ListFunctionsByCodeSigningConfigResponse' {} a -> s {nextMarker = a} :: ListFunctionsByCodeSigningConfigResponse)

-- | The response's http status code.
listFunctionsByCodeSigningConfigResponse_httpStatus :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse Core.Int
listFunctionsByCodeSigningConfigResponse_httpStatus = Lens.lens (\ListFunctionsByCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@ListFunctionsByCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: ListFunctionsByCodeSigningConfigResponse)

instance
  Core.NFData
    ListFunctionsByCodeSigningConfigResponse
