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

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListLayers' smart constructor.
data ListLayers = ListLayers'
  { -- | The maximum number of layers to return.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | A runtime identifier. For example, @go1.x@.
    compatibleRuntime :: Prelude.Maybe Runtime,
    -- | A pagination token returned by a previous call.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { maxItems = Prelude.Nothing,
      compatibleRuntime = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The maximum number of layers to return.
listLayers_maxItems :: Lens.Lens' ListLayers (Prelude.Maybe Prelude.Natural)
listLayers_maxItems = Lens.lens (\ListLayers' {maxItems} -> maxItems) (\s@ListLayers' {} a -> s {maxItems = a} :: ListLayers)

-- | A runtime identifier. For example, @go1.x@.
listLayers_compatibleRuntime :: Lens.Lens' ListLayers (Prelude.Maybe Runtime)
listLayers_compatibleRuntime = Lens.lens (\ListLayers' {compatibleRuntime} -> compatibleRuntime) (\s@ListLayers' {} a -> s {compatibleRuntime = a} :: ListLayers)

-- | A pagination token returned by a previous call.
listLayers_marker :: Lens.Lens' ListLayers (Prelude.Maybe Prelude.Text)
listLayers_marker = Lens.lens (\ListLayers' {marker} -> marker) (\s@ListLayers' {} a -> s {marker = a} :: ListLayers)

instance Pager.AWSPager ListLayers where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listLayersResponse_nextMarker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listLayersResponse_layers Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listLayers_marker
          Lens..~ rs
          Lens.^? listLayersResponse_nextMarker Prelude.. Lens._Just

instance Prelude.AWSRequest ListLayers where
  type Rs ListLayers = ListLayersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLayersResponse'
            Prelude.<$> (x Prelude..?> "NextMarker")
            Prelude.<*> (x Prelude..?> "Layers" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLayers

instance Prelude.NFData ListLayers

instance Prelude.ToHeaders ListLayers where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListLayers where
  toPath = Prelude.const "/2018-10-31/layers"

instance Prelude.ToQuery ListLayers where
  toQuery ListLayers' {..} =
    Prelude.mconcat
      [ "MaxItems" Prelude.=: maxItems,
        "CompatibleRuntime" Prelude.=: compatibleRuntime,
        "Marker" Prelude.=: marker
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listLayersResponse_layers = Lens.lens (\ListLayersResponse' {layers} -> layers) (\s@ListLayersResponse' {} a -> s {layers = a} :: ListLayersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listLayersResponse_httpStatus :: Lens.Lens' ListLayersResponse Prelude.Int
listLayersResponse_httpStatus = Lens.lens (\ListLayersResponse' {httpStatus} -> httpStatus) (\s@ListLayersResponse' {} a -> s {httpStatus = a} :: ListLayersResponse)

instance Prelude.NFData ListLayersResponse
