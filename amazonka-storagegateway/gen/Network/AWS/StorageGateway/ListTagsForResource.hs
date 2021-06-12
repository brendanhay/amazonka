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
-- Module      : Network.AWS.StorageGateway.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that have been added to the specified resource. This
-- operation is supported in storage gateways of all types.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_limit,
    listTagsForResource_marker,
    listTagsForResource_resourceARN,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_marker,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | ListTagsForResourceInput
--
-- /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | Specifies that the list of tags returned be limited to the specified
    -- number of items.
    limit :: Core.Maybe Core.Natural,
    -- | An opaque string that indicates the position at which to begin returning
    -- the list of tags.
    marker :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the resource for which you want to
    -- list tags.
    resourceARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listTagsForResource_limit' - Specifies that the list of tags returned be limited to the specified
-- number of items.
--
-- 'marker', 'listTagsForResource_marker' - An opaque string that indicates the position at which to begin returning
-- the list of tags.
--
-- 'resourceARN', 'listTagsForResource_resourceARN' - The Amazon Resource Name (ARN) of the resource for which you want to
-- list tags.
newListTagsForResource ::
  -- | 'resourceARN'
  Core.Text ->
  ListTagsForResource
newListTagsForResource pResourceARN_ =
  ListTagsForResource'
    { limit = Core.Nothing,
      marker = Core.Nothing,
      resourceARN = pResourceARN_
    }

-- | Specifies that the list of tags returned be limited to the specified
-- number of items.
listTagsForResource_limit :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Natural)
listTagsForResource_limit = Lens.lens (\ListTagsForResource' {limit} -> limit) (\s@ListTagsForResource' {} a -> s {limit = a} :: ListTagsForResource)

-- | An opaque string that indicates the position at which to begin returning
-- the list of tags.
listTagsForResource_marker :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Text)
listTagsForResource_marker = Lens.lens (\ListTagsForResource' {marker} -> marker) (\s@ListTagsForResource' {} a -> s {marker = a} :: ListTagsForResource)

-- | The Amazon Resource Name (ARN) of the resource for which you want to
-- list tags.
listTagsForResource_resourceARN :: Lens.Lens' ListTagsForResource Core.Text
listTagsForResource_resourceARN = Lens.lens (\ListTagsForResource' {resourceARN} -> resourceARN) (\s@ListTagsForResource' {} a -> s {resourceARN = a} :: ListTagsForResource)

instance Core.AWSPager ListTagsForResource where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagsForResourceResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTagsForResourceResponse_tags Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTagsForResource_marker
          Lens..~ rs
          Lens.^? listTagsForResourceResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Core.<$> (x Core..?> "ResourceARN")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTagsForResource

instance Core.NFData ListTagsForResource

instance Core.ToHeaders ListTagsForResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListTagsForResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker,
            Core.Just ("ResourceARN" Core..= resourceARN)
          ]
      )

instance Core.ToPath ListTagsForResource where
  toPath = Core.const "/"

instance Core.ToQuery ListTagsForResource where
  toQuery = Core.const Core.mempty

-- | ListTagsForResourceOutput
--
-- /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | The Amazon Resource Name (ARN) of the resource for which you want to
    -- list tags.
    resourceARN :: Core.Maybe Core.Text,
    -- | An array that contains the tags for the specified resource.
    tags :: Core.Maybe [Tag],
    -- | An opaque string that indicates the position at which to stop returning
    -- the list of tags.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'listTagsForResourceResponse_resourceARN' - The Amazon Resource Name (ARN) of the resource for which you want to
-- list tags.
--
-- 'tags', 'listTagsForResourceResponse_tags' - An array that contains the tags for the specified resource.
--
-- 'marker', 'listTagsForResourceResponse_marker' - An opaque string that indicates the position at which to stop returning
-- the list of tags.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { resourceARN =
        Core.Nothing,
      tags = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource for which you want to
-- list tags.
listTagsForResourceResponse_resourceARN :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Core.Text)
listTagsForResourceResponse_resourceARN = Lens.lens (\ListTagsForResourceResponse' {resourceARN} -> resourceARN) (\s@ListTagsForResourceResponse' {} a -> s {resourceARN = a} :: ListTagsForResourceResponse)

-- | An array that contains the tags for the specified resource.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Tag])
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | An opaque string that indicates the position at which to stop returning
-- the list of tags.
listTagsForResourceResponse_marker :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Core.Text)
listTagsForResourceResponse_marker = Lens.lens (\ListTagsForResourceResponse' {marker} -> marker) (\s@ListTagsForResourceResponse' {} a -> s {marker = a} :: ListTagsForResourceResponse)

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Core.NFData ListTagsForResourceResponse
