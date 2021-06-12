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
-- Module      : Network.AWS.MediaConvert.ListPresets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your presets. This will return
-- the presets themselves, not just a list of them. To retrieve the next
-- twenty presets, use the nextToken string returned with the array.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListPresets
  ( -- * Creating a Request
    ListPresets (..),
    newListPresets,

    -- * Request Lenses
    listPresets_nextToken,
    listPresets_listBy,
    listPresets_maxResults,
    listPresets_category,
    listPresets_order,

    -- * Destructuring the Response
    ListPresetsResponse (..),
    newListPresetsResponse,

    -- * Response Lenses
    listPresetsResponse_nextToken,
    listPresetsResponse_presets,
    listPresetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPresets' smart constructor.
data ListPresets = ListPresets'
  { -- | Use this string, provided with the response to a previous request, to
    -- request the next batch of presets.
    nextToken :: Core.Maybe Core.Text,
    -- | Optional. When you request a list of presets, you can choose to list
    -- them alphabetically by NAME or chronologically by CREATION_DATE. If you
    -- don\'t specify, the service will list them by name.
    listBy :: Core.Maybe PresetListBy,
    -- | Optional. Number of presets, up to twenty, that will be returned at one
    -- time
    maxResults :: Core.Maybe Core.Natural,
    -- | Optionally, specify a preset category to limit responses to only presets
    -- from that category.
    category :: Core.Maybe Core.Text,
    -- | Optional. When you request lists of resources, you can specify whether
    -- they are sorted in ASCENDING or DESCENDING order. Default varies by
    -- resource.
    order :: Core.Maybe Order
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPresets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPresets_nextToken' - Use this string, provided with the response to a previous request, to
-- request the next batch of presets.
--
-- 'listBy', 'listPresets_listBy' - Optional. When you request a list of presets, you can choose to list
-- them alphabetically by NAME or chronologically by CREATION_DATE. If you
-- don\'t specify, the service will list them by name.
--
-- 'maxResults', 'listPresets_maxResults' - Optional. Number of presets, up to twenty, that will be returned at one
-- time
--
-- 'category', 'listPresets_category' - Optionally, specify a preset category to limit responses to only presets
-- from that category.
--
-- 'order', 'listPresets_order' - Optional. When you request lists of resources, you can specify whether
-- they are sorted in ASCENDING or DESCENDING order. Default varies by
-- resource.
newListPresets ::
  ListPresets
newListPresets =
  ListPresets'
    { nextToken = Core.Nothing,
      listBy = Core.Nothing,
      maxResults = Core.Nothing,
      category = Core.Nothing,
      order = Core.Nothing
    }

-- | Use this string, provided with the response to a previous request, to
-- request the next batch of presets.
listPresets_nextToken :: Lens.Lens' ListPresets (Core.Maybe Core.Text)
listPresets_nextToken = Lens.lens (\ListPresets' {nextToken} -> nextToken) (\s@ListPresets' {} a -> s {nextToken = a} :: ListPresets)

-- | Optional. When you request a list of presets, you can choose to list
-- them alphabetically by NAME or chronologically by CREATION_DATE. If you
-- don\'t specify, the service will list them by name.
listPresets_listBy :: Lens.Lens' ListPresets (Core.Maybe PresetListBy)
listPresets_listBy = Lens.lens (\ListPresets' {listBy} -> listBy) (\s@ListPresets' {} a -> s {listBy = a} :: ListPresets)

-- | Optional. Number of presets, up to twenty, that will be returned at one
-- time
listPresets_maxResults :: Lens.Lens' ListPresets (Core.Maybe Core.Natural)
listPresets_maxResults = Lens.lens (\ListPresets' {maxResults} -> maxResults) (\s@ListPresets' {} a -> s {maxResults = a} :: ListPresets)

-- | Optionally, specify a preset category to limit responses to only presets
-- from that category.
listPresets_category :: Lens.Lens' ListPresets (Core.Maybe Core.Text)
listPresets_category = Lens.lens (\ListPresets' {category} -> category) (\s@ListPresets' {} a -> s {category = a} :: ListPresets)

-- | Optional. When you request lists of resources, you can specify whether
-- they are sorted in ASCENDING or DESCENDING order. Default varies by
-- resource.
listPresets_order :: Lens.Lens' ListPresets (Core.Maybe Order)
listPresets_order = Lens.lens (\ListPresets' {order} -> order) (\s@ListPresets' {} a -> s {order = a} :: ListPresets)

instance Core.AWSPager ListPresets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPresetsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPresetsResponse_presets Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPresets_nextToken
          Lens..~ rs
          Lens.^? listPresetsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListPresets where
  type AWSResponse ListPresets = ListPresetsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPresetsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "presets" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPresets

instance Core.NFData ListPresets

instance Core.ToHeaders ListPresets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListPresets where
  toPath = Core.const "/2017-08-29/presets"

instance Core.ToQuery ListPresets where
  toQuery ListPresets' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "listBy" Core.=: listBy,
        "maxResults" Core.=: maxResults,
        "category" Core.=: category,
        "order" Core.=: order
      ]

-- | /See:/ 'newListPresetsResponse' smart constructor.
data ListPresetsResponse = ListPresetsResponse'
  { -- | Use this string to request the next batch of presets.
    nextToken :: Core.Maybe Core.Text,
    -- | List of presets
    presets :: Core.Maybe [Preset],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPresetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPresetsResponse_nextToken' - Use this string to request the next batch of presets.
--
-- 'presets', 'listPresetsResponse_presets' - List of presets
--
-- 'httpStatus', 'listPresetsResponse_httpStatus' - The response's http status code.
newListPresetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPresetsResponse
newListPresetsResponse pHttpStatus_ =
  ListPresetsResponse'
    { nextToken = Core.Nothing,
      presets = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Use this string to request the next batch of presets.
listPresetsResponse_nextToken :: Lens.Lens' ListPresetsResponse (Core.Maybe Core.Text)
listPresetsResponse_nextToken = Lens.lens (\ListPresetsResponse' {nextToken} -> nextToken) (\s@ListPresetsResponse' {} a -> s {nextToken = a} :: ListPresetsResponse)

-- | List of presets
listPresetsResponse_presets :: Lens.Lens' ListPresetsResponse (Core.Maybe [Preset])
listPresetsResponse_presets = Lens.lens (\ListPresetsResponse' {presets} -> presets) (\s@ListPresetsResponse' {} a -> s {presets = a} :: ListPresetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPresetsResponse_httpStatus :: Lens.Lens' ListPresetsResponse Core.Int
listPresetsResponse_httpStatus = Lens.lens (\ListPresetsResponse' {httpStatus} -> httpStatus) (\s@ListPresetsResponse' {} a -> s {httpStatus = a} :: ListPresetsResponse)

instance Core.NFData ListPresetsResponse
