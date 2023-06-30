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
-- Module      : Amazonka.StorageGateway.ListTagsForResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that have been added to the specified resource. This
-- operation is supported in storage gateways of all types.
--
-- This operation returns paginated results.
module Amazonka.StorageGateway.ListTagsForResource
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
    listTagsForResourceResponse_marker,
    listTagsForResourceResponse_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | ListTagsForResourceInput
--
-- /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | Specifies that the list of tags returned be limited to the specified
    -- number of items.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | An opaque string that indicates the position at which to begin returning
    -- the list of tags.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource for which you want to
    -- list tags.
    resourceARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListTagsForResource
newListTagsForResource pResourceARN_ =
  ListTagsForResource'
    { limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      resourceARN = pResourceARN_
    }

-- | Specifies that the list of tags returned be limited to the specified
-- number of items.
listTagsForResource_limit :: Lens.Lens' ListTagsForResource (Prelude.Maybe Prelude.Natural)
listTagsForResource_limit = Lens.lens (\ListTagsForResource' {limit} -> limit) (\s@ListTagsForResource' {} a -> s {limit = a} :: ListTagsForResource)

-- | An opaque string that indicates the position at which to begin returning
-- the list of tags.
listTagsForResource_marker :: Lens.Lens' ListTagsForResource (Prelude.Maybe Prelude.Text)
listTagsForResource_marker = Lens.lens (\ListTagsForResource' {marker} -> marker) (\s@ListTagsForResource' {} a -> s {marker = a} :: ListTagsForResource)

-- | The Amazon Resource Name (ARN) of the resource for which you want to
-- list tags.
listTagsForResource_resourceARN :: Lens.Lens' ListTagsForResource Prelude.Text
listTagsForResource_resourceARN = Lens.lens (\ListTagsForResource' {resourceARN} -> resourceARN) (\s@ListTagsForResource' {} a -> s {resourceARN = a} :: ListTagsForResource)

instance Core.AWSPager ListTagsForResource where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagsForResourceResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTagsForResourceResponse_tags
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTagsForResource_marker
          Lens..~ rs
          Lens.^? listTagsForResourceResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> (x Data..?> "ResourceARN")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForResource where
  hashWithSalt _salt ListTagsForResource' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` resourceARN

instance Prelude.NFData ListTagsForResource where
  rnf ListTagsForResource' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf resourceARN

instance Data.ToHeaders ListTagsForResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.ListTagsForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("Marker" Data..=) Prelude.<$> marker,
            Prelude.Just ("ResourceARN" Data..= resourceARN)
          ]
      )

instance Data.ToPath ListTagsForResource where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTagsForResource where
  toQuery = Prelude.const Prelude.mempty

-- | ListTagsForResourceOutput
--
-- /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | An opaque string that indicates the position at which to stop returning
    -- the list of tags.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource for which you want to
    -- list tags.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | An array that contains the tags for the specified resource.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listTagsForResourceResponse_marker' - An opaque string that indicates the position at which to stop returning
-- the list of tags.
--
-- 'resourceARN', 'listTagsForResourceResponse_resourceARN' - The Amazon Resource Name (ARN) of the resource for which you want to
-- list tags.
--
-- 'tags', 'listTagsForResourceResponse_tags' - An array that contains the tags for the specified resource.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { marker =
        Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque string that indicates the position at which to stop returning
-- the list of tags.
listTagsForResourceResponse_marker :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe Prelude.Text)
listTagsForResourceResponse_marker = Lens.lens (\ListTagsForResourceResponse' {marker} -> marker) (\s@ListTagsForResourceResponse' {} a -> s {marker = a} :: ListTagsForResourceResponse)

-- | The Amazon Resource Name (ARN) of the resource for which you want to
-- list tags.
listTagsForResourceResponse_resourceARN :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe Prelude.Text)
listTagsForResourceResponse_resourceARN = Lens.lens (\ListTagsForResourceResponse' {resourceARN} -> resourceARN) (\s@ListTagsForResourceResponse' {} a -> s {resourceARN = a} :: ListTagsForResourceResponse)

-- | An array that contains the tags for the specified resource.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe [Tag])
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Prelude.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Prelude.NFData ListTagsForResourceResponse where
  rnf ListTagsForResourceResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
