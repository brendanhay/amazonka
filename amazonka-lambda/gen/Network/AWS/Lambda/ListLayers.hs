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
-- Module      : Network.AWS.Lambda.ListLayers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layers>
-- and shows information about the latest version of each. Specify a
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime identifier>
-- to list only layers that indicate that they\'re compatible with that
-- runtime.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListLayers
  ( -- * Creating a Request
    ListLayers (..),
    newListLayers,

    -- * Request Lenses
    listLayers_maxItems,
    listLayers_compatibleRuntime,
    listLayers_marker,

    -- * Destructuring the Response
    ListLayersResponse (..),
    newListLayersResponse,

    -- * Response Lenses
    listLayersResponse_nextMarker,
    listLayersResponse_layers,
    listLayersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListLayers' smart constructor.
data ListLayers = ListLayers'
  { -- | The maximum number of layers to return.
    maxItems :: Core.Maybe Core.Natural,
    -- | A runtime identifier. For example, @go1.x@.
    compatibleRuntime :: Core.Maybe Runtime,
    -- | A pagination token returned by a previous call.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLayers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listLayers_maxItems' - The maximum number of layers to return.
--
-- 'compatibleRuntime', 'listLayers_compatibleRuntime' - A runtime identifier. For example, @go1.x@.
--
-- 'marker', 'listLayers_marker' - A pagination token returned by a previous call.
newListLayers ::
  ListLayers
newListLayers =
  ListLayers'
    { maxItems = Core.Nothing,
      compatibleRuntime = Core.Nothing,
      marker = Core.Nothing
    }

-- | The maximum number of layers to return.
listLayers_maxItems :: Lens.Lens' ListLayers (Core.Maybe Core.Natural)
listLayers_maxItems = Lens.lens (\ListLayers' {maxItems} -> maxItems) (\s@ListLayers' {} a -> s {maxItems = a} :: ListLayers)

-- | A runtime identifier. For example, @go1.x@.
listLayers_compatibleRuntime :: Lens.Lens' ListLayers (Core.Maybe Runtime)
listLayers_compatibleRuntime = Lens.lens (\ListLayers' {compatibleRuntime} -> compatibleRuntime) (\s@ListLayers' {} a -> s {compatibleRuntime = a} :: ListLayers)

-- | A pagination token returned by a previous call.
listLayers_marker :: Lens.Lens' ListLayers (Core.Maybe Core.Text)
listLayers_marker = Lens.lens (\ListLayers' {marker} -> marker) (\s@ListLayers' {} a -> s {marker = a} :: ListLayers)

instance Core.AWSPager ListLayers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLayersResponse_nextMarker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listLayersResponse_layers Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listLayers_marker
          Lens..~ rs
          Lens.^? listLayersResponse_nextMarker Core.. Lens._Just

instance Core.AWSRequest ListLayers where
  type AWSResponse ListLayers = ListLayersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLayersResponse'
            Core.<$> (x Core..?> "NextMarker")
            Core.<*> (x Core..?> "Layers" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListLayers

instance Core.NFData ListLayers

instance Core.ToHeaders ListLayers where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListLayers where
  toPath = Core.const "/2018-10-31/layers"

instance Core.ToQuery ListLayers where
  toQuery ListLayers' {..} =
    Core.mconcat
      [ "MaxItems" Core.=: maxItems,
        "CompatibleRuntime" Core.=: compatibleRuntime,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListLayersResponse' smart constructor.
data ListLayersResponse = ListLayersResponse'
  { -- | A pagination token returned when the response doesn\'t contain all
    -- layers.
    nextMarker :: Core.Maybe Core.Text,
    -- | A list of function layers.
    layers :: Core.Maybe [LayersListItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListLayersResponse
newListLayersResponse pHttpStatus_ =
  ListLayersResponse'
    { nextMarker = Core.Nothing,
      layers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token returned when the response doesn\'t contain all
-- layers.
listLayersResponse_nextMarker :: Lens.Lens' ListLayersResponse (Core.Maybe Core.Text)
listLayersResponse_nextMarker = Lens.lens (\ListLayersResponse' {nextMarker} -> nextMarker) (\s@ListLayersResponse' {} a -> s {nextMarker = a} :: ListLayersResponse)

-- | A list of function layers.
listLayersResponse_layers :: Lens.Lens' ListLayersResponse (Core.Maybe [LayersListItem])
listLayersResponse_layers = Lens.lens (\ListLayersResponse' {layers} -> layers) (\s@ListLayersResponse' {} a -> s {layers = a} :: ListLayersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listLayersResponse_httpStatus :: Lens.Lens' ListLayersResponse Core.Int
listLayersResponse_httpStatus = Lens.lens (\ListLayersResponse' {httpStatus} -> httpStatus) (\s@ListLayersResponse' {} a -> s {httpStatus = a} :: ListLayersResponse)

instance Core.NFData ListLayersResponse
