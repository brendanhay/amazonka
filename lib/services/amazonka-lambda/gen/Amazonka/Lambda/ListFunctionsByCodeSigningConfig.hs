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
-- Module      : Amazonka.Lambda.ListFunctionsByCodeSigningConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Lambda.ListFunctionsByCodeSigningConfig
  ( -- * Creating a Request
    ListFunctionsByCodeSigningConfig (..),
    newListFunctionsByCodeSigningConfig,

    -- * Request Lenses
    listFunctionsByCodeSigningConfig_marker,
    listFunctionsByCodeSigningConfig_maxItems,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFunctionsByCodeSigningConfig' smart constructor.
data ListFunctionsByCodeSigningConfig = ListFunctionsByCodeSigningConfig'
  { -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of items to return.
    maxItems :: Prelude.Maybe Prelude.Natural,
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
-- 'marker', 'listFunctionsByCodeSigningConfig_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'maxItems', 'listFunctionsByCodeSigningConfig_maxItems' - Maximum number of items to return.
--
-- 'codeSigningConfigArn', 'listFunctionsByCodeSigningConfig_codeSigningConfigArn' - The The Amazon Resource Name (ARN) of the code signing configuration.
newListFunctionsByCodeSigningConfig ::
  -- | 'codeSigningConfigArn'
  Prelude.Text ->
  ListFunctionsByCodeSigningConfig
newListFunctionsByCodeSigningConfig
  pCodeSigningConfigArn_ =
    ListFunctionsByCodeSigningConfig'
      { marker =
          Prelude.Nothing,
        maxItems = Prelude.Nothing,
        codeSigningConfigArn =
          pCodeSigningConfigArn_
      }

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listFunctionsByCodeSigningConfig_marker :: Lens.Lens' ListFunctionsByCodeSigningConfig (Prelude.Maybe Prelude.Text)
listFunctionsByCodeSigningConfig_marker = Lens.lens (\ListFunctionsByCodeSigningConfig' {marker} -> marker) (\s@ListFunctionsByCodeSigningConfig' {} a -> s {marker = a} :: ListFunctionsByCodeSigningConfig)

-- | Maximum number of items to return.
listFunctionsByCodeSigningConfig_maxItems :: Lens.Lens' ListFunctionsByCodeSigningConfig (Prelude.Maybe Prelude.Natural)
listFunctionsByCodeSigningConfig_maxItems = Lens.lens (\ListFunctionsByCodeSigningConfig' {maxItems} -> maxItems) (\s@ListFunctionsByCodeSigningConfig' {} a -> s {maxItems = a} :: ListFunctionsByCodeSigningConfig)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionsByCodeSigningConfigResponse'
            Prelude.<$> (x Data..?> "FunctionArns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFunctionsByCodeSigningConfig
  where
  hashWithSalt
    _salt
    ListFunctionsByCodeSigningConfig' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxItems
        `Prelude.hashWithSalt` codeSigningConfigArn

instance
  Prelude.NFData
    ListFunctionsByCodeSigningConfig
  where
  rnf ListFunctionsByCodeSigningConfig' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf codeSigningConfigArn

instance
  Data.ToHeaders
    ListFunctionsByCodeSigningConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListFunctionsByCodeSigningConfig where
  toPath ListFunctionsByCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Data.toBS codeSigningConfigArn,
        "/functions"
      ]

instance
  Data.ToQuery
    ListFunctionsByCodeSigningConfig
  where
  toQuery ListFunctionsByCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
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
listFunctionsByCodeSigningConfigResponse_functionArns = Lens.lens (\ListFunctionsByCodeSigningConfigResponse' {functionArns} -> functionArns) (\s@ListFunctionsByCodeSigningConfigResponse' {} a -> s {functionArns = a} :: ListFunctionsByCodeSigningConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s included if more results are available.
listFunctionsByCodeSigningConfigResponse_nextMarker :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse (Prelude.Maybe Prelude.Text)
listFunctionsByCodeSigningConfigResponse_nextMarker = Lens.lens (\ListFunctionsByCodeSigningConfigResponse' {nextMarker} -> nextMarker) (\s@ListFunctionsByCodeSigningConfigResponse' {} a -> s {nextMarker = a} :: ListFunctionsByCodeSigningConfigResponse)

-- | The response's http status code.
listFunctionsByCodeSigningConfigResponse_httpStatus :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse Prelude.Int
listFunctionsByCodeSigningConfigResponse_httpStatus = Lens.lens (\ListFunctionsByCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@ListFunctionsByCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: ListFunctionsByCodeSigningConfigResponse)

instance
  Prelude.NFData
    ListFunctionsByCodeSigningConfigResponse
  where
  rnf ListFunctionsByCodeSigningConfigResponse' {..} =
    Prelude.rnf functionArns
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
