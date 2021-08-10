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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFunctionsByCodeSigningConfig' smart constructor.
data ListFunctionsByCodeSigningConfig = ListFunctionsByCodeSigningConfig'
  { -- | Maximum number of items to return.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListFunctionsByCodeSigningConfig
newListFunctionsByCodeSigningConfig
  pCodeSigningConfigArn_ =
    ListFunctionsByCodeSigningConfig'
      { maxItems =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        codeSigningConfigArn =
          pCodeSigningConfigArn_
      }

-- | Maximum number of items to return.
listFunctionsByCodeSigningConfig_maxItems :: Lens.Lens' ListFunctionsByCodeSigningConfig (Prelude.Maybe Prelude.Natural)
listFunctionsByCodeSigningConfig_maxItems = Lens.lens (\ListFunctionsByCodeSigningConfig' {maxItems} -> maxItems) (\s@ListFunctionsByCodeSigningConfig' {} a -> s {maxItems = a} :: ListFunctionsByCodeSigningConfig)

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listFunctionsByCodeSigningConfig_marker :: Lens.Lens' ListFunctionsByCodeSigningConfig (Prelude.Maybe Prelude.Text)
listFunctionsByCodeSigningConfig_marker = Lens.lens (\ListFunctionsByCodeSigningConfig' {marker} -> marker) (\s@ListFunctionsByCodeSigningConfig' {} a -> s {marker = a} :: ListFunctionsByCodeSigningConfig)

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
listFunctionsByCodeSigningConfig_codeSigningConfigArn :: Lens.Lens' ListFunctionsByCodeSigningConfig Prelude.Text
listFunctionsByCodeSigningConfig_codeSigningConfigArn = Lens.lens (\ListFunctionsByCodeSigningConfig' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@ListFunctionsByCodeSigningConfig' {} a -> s {codeSigningConfigArn = a} :: ListFunctionsByCodeSigningConfig)

instance
  Core.AWSPager
    ListFunctionsByCodeSigningConfig
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFunctionsByCodeSigningConfigResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFunctionsByCodeSigningConfigResponse_functionArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFunctionsByCodeSigningConfig_marker
          Lens..~ rs
          Lens.^? listFunctionsByCodeSigningConfigResponse_nextMarker
            Prelude.. Lens._Just

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
            Prelude.<$> (x Core..?> "FunctionArns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFunctionsByCodeSigningConfig

instance
  Prelude.NFData
    ListFunctionsByCodeSigningConfig

instance
  Core.ToHeaders
    ListFunctionsByCodeSigningConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListFunctionsByCodeSigningConfig where
  toPath ListFunctionsByCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Core.toBS codeSigningConfigArn,
        "/functions"
      ]

instance
  Core.ToQuery
    ListFunctionsByCodeSigningConfig
  where
  toQuery ListFunctionsByCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListFunctionsByCodeSigningConfigResponse' smart constructor.
data ListFunctionsByCodeSigningConfigResponse = ListFunctionsByCodeSigningConfigResponse'
  { -- | The function ARNs.
    functionArns :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token that\'s included if more results are available.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListFunctionsByCodeSigningConfigResponse
newListFunctionsByCodeSigningConfigResponse
  pHttpStatus_ =
    ListFunctionsByCodeSigningConfigResponse'
      { functionArns =
          Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The function ARNs.
listFunctionsByCodeSigningConfigResponse_functionArns :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse (Prelude.Maybe [Prelude.Text])
listFunctionsByCodeSigningConfigResponse_functionArns = Lens.lens (\ListFunctionsByCodeSigningConfigResponse' {functionArns} -> functionArns) (\s@ListFunctionsByCodeSigningConfigResponse' {} a -> s {functionArns = a} :: ListFunctionsByCodeSigningConfigResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The pagination token that\'s included if more results are available.
listFunctionsByCodeSigningConfigResponse_nextMarker :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse (Prelude.Maybe Prelude.Text)
listFunctionsByCodeSigningConfigResponse_nextMarker = Lens.lens (\ListFunctionsByCodeSigningConfigResponse' {nextMarker} -> nextMarker) (\s@ListFunctionsByCodeSigningConfigResponse' {} a -> s {nextMarker = a} :: ListFunctionsByCodeSigningConfigResponse)

-- | The response's http status code.
listFunctionsByCodeSigningConfigResponse_httpStatus :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse Prelude.Int
listFunctionsByCodeSigningConfigResponse_httpStatus = Lens.lens (\ListFunctionsByCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@ListFunctionsByCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: ListFunctionsByCodeSigningConfigResponse)

instance
  Prelude.NFData
    ListFunctionsByCodeSigningConfigResponse
