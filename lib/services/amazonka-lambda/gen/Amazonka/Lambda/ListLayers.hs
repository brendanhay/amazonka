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
-- Module      : Amazonka.Lambda.ListLayers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-layers.html Lambda layers>
-- and shows information about the latest version of each. Specify a
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime identifier>
-- to list only layers that indicate that they\'re compatible with that
-- runtime. Specify a compatible architecture to include only layers that
-- are compatible with that
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architecture>.
--
-- This operation returns paginated results.
module Amazonka.Lambda.ListLayers
  ( -- * Creating a Request
    ListLayers (..),
    newListLayers,

    -- * Request Lenses
    listLayers_compatibleRuntime,
    listLayers_marker,
    listLayers_maxItems,
    listLayers_compatibleArchitecture,

    -- * Destructuring the Response
    ListLayersResponse (..),
    newListLayersResponse,

    -- * Response Lenses
    listLayersResponse_nextMarker,
    listLayersResponse_layers,
    listLayersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Lambda.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLayers' smart constructor.
data ListLayers = ListLayers'
  { -- | A runtime identifier. For example, @go1.x@.
    compatibleRuntime :: Prelude.Maybe Runtime,
    -- | A pagination token returned by a previous call.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of layers to return.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The compatible
    -- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architecture>.
    compatibleArchitecture :: Prelude.Maybe Architecture
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLayers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleRuntime', 'listLayers_compatibleRuntime' - A runtime identifier. For example, @go1.x@.
--
-- 'marker', 'listLayers_marker' - A pagination token returned by a previous call.
--
-- 'maxItems', 'listLayers_maxItems' - The maximum number of layers to return.
--
-- 'compatibleArchitecture', 'listLayers_compatibleArchitecture' - The compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architecture>.
newListLayers ::
  ListLayers
newListLayers =
  ListLayers'
    { compatibleRuntime = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      compatibleArchitecture = Prelude.Nothing
    }

-- | A runtime identifier. For example, @go1.x@.
listLayers_compatibleRuntime :: Lens.Lens' ListLayers (Prelude.Maybe Runtime)
listLayers_compatibleRuntime = Lens.lens (\ListLayers' {compatibleRuntime} -> compatibleRuntime) (\s@ListLayers' {} a -> s {compatibleRuntime = a} :: ListLayers)

-- | A pagination token returned by a previous call.
listLayers_marker :: Lens.Lens' ListLayers (Prelude.Maybe Prelude.Text)
listLayers_marker = Lens.lens (\ListLayers' {marker} -> marker) (\s@ListLayers' {} a -> s {marker = a} :: ListLayers)

-- | The maximum number of layers to return.
listLayers_maxItems :: Lens.Lens' ListLayers (Prelude.Maybe Prelude.Natural)
listLayers_maxItems = Lens.lens (\ListLayers' {maxItems} -> maxItems) (\s@ListLayers' {} a -> s {maxItems = a} :: ListLayers)

-- | The compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architecture>.
listLayers_compatibleArchitecture :: Lens.Lens' ListLayers (Prelude.Maybe Architecture)
listLayers_compatibleArchitecture = Lens.lens (\ListLayers' {compatibleArchitecture} -> compatibleArchitecture) (\s@ListLayers' {} a -> s {compatibleArchitecture = a} :: ListLayers)

instance Core.AWSPager ListLayers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLayersResponse_nextMarker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLayersResponse_layers Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLayers_marker
          Lens..~ rs
          Lens.^? listLayersResponse_nextMarker Prelude.. Lens._Just

instance Core.AWSRequest ListLayers where
  type AWSResponse ListLayers = ListLayersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLayersResponse'
            Prelude.<$> (x Core..?> "NextMarker")
            Prelude.<*> (x Core..?> "Layers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLayers where
  hashWithSalt salt' ListLayers' {..} =
    salt' `Prelude.hashWithSalt` compatibleArchitecture
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` compatibleRuntime

instance Prelude.NFData ListLayers where
  rnf ListLayers' {..} =
    Prelude.rnf compatibleRuntime
      `Prelude.seq` Prelude.rnf compatibleArchitecture
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf marker

instance Core.ToHeaders ListLayers where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListLayers where
  toPath = Prelude.const "/2018-10-31/layers"

instance Core.ToQuery ListLayers where
  toQuery ListLayers' {..} =
    Prelude.mconcat
      [ "CompatibleRuntime" Core.=: compatibleRuntime,
        "Marker" Core.=: marker,
        "MaxItems" Core.=: maxItems,
        "CompatibleArchitecture"
          Core.=: compatibleArchitecture
      ]

-- | /See:/ 'newListLayersResponse' smart constructor.
data ListLayersResponse = ListLayersResponse'
  { -- | A pagination token returned when the response doesn\'t contain all
    -- layers.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | A list of function layers.
    layers :: Prelude.Maybe [LayersListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLayersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listLayersResponse_nextMarker' - A pagination token returned when the response doesn\'t contain all
-- layers.
--
-- 'layers', 'listLayersResponse_layers' - A list of function layers.
--
-- 'httpStatus', 'listLayersResponse_httpStatus' - The response's http status code.
newListLayersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLayersResponse
newListLayersResponse pHttpStatus_ =
  ListLayersResponse'
    { nextMarker = Prelude.Nothing,
      layers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token returned when the response doesn\'t contain all
-- layers.
listLayersResponse_nextMarker :: Lens.Lens' ListLayersResponse (Prelude.Maybe Prelude.Text)
listLayersResponse_nextMarker = Lens.lens (\ListLayersResponse' {nextMarker} -> nextMarker) (\s@ListLayersResponse' {} a -> s {nextMarker = a} :: ListLayersResponse)

-- | A list of function layers.
listLayersResponse_layers :: Lens.Lens' ListLayersResponse (Prelude.Maybe [LayersListItem])
listLayersResponse_layers = Lens.lens (\ListLayersResponse' {layers} -> layers) (\s@ListLayersResponse' {} a -> s {layers = a} :: ListLayersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listLayersResponse_httpStatus :: Lens.Lens' ListLayersResponse Prelude.Int
listLayersResponse_httpStatus = Lens.lens (\ListLayersResponse' {httpStatus} -> httpStatus) (\s@ListLayersResponse' {} a -> s {httpStatus = a} :: ListLayersResponse)

instance Prelude.NFData ListLayersResponse where
  rnf ListLayersResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf layers
