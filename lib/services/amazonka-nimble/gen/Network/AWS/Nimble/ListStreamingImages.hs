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
-- Module      : Network.AWS.Nimble.ListStreamingImages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the streaming image resources available to this studio.
--
-- This list will contain both images provided by Amazon Web Services, as
-- well as streaming images that you have created in your studio.
--
-- This operation returns paginated results.
module Network.AWS.Nimble.ListStreamingImages
  ( -- * Creating a Request
    ListStreamingImages (..),
    newListStreamingImages,

    -- * Request Lenses
    listStreamingImages_owner,
    listStreamingImages_nextToken,
    listStreamingImages_studioId,

    -- * Destructuring the Response
    ListStreamingImagesResponse (..),
    newListStreamingImagesResponse,

    -- * Response Lenses
    listStreamingImagesResponse_streamingImages,
    listStreamingImagesResponse_nextToken,
    listStreamingImagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Nimble.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStreamingImages' smart constructor.
data ListStreamingImages = ListStreamingImages'
  { -- | The owner.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamingImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'owner', 'listStreamingImages_owner' - The owner.
--
-- 'nextToken', 'listStreamingImages_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'studioId', 'listStreamingImages_studioId' - The studio ID.
newListStreamingImages ::
  -- | 'studioId'
  Prelude.Text ->
  ListStreamingImages
newListStreamingImages pStudioId_ =
  ListStreamingImages'
    { owner = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The owner.
listStreamingImages_owner :: Lens.Lens' ListStreamingImages (Prelude.Maybe Prelude.Text)
listStreamingImages_owner = Lens.lens (\ListStreamingImages' {owner} -> owner) (\s@ListStreamingImages' {} a -> s {owner = a} :: ListStreamingImages)

-- | The token for the next set of results, or null if there are no more
-- results.
listStreamingImages_nextToken :: Lens.Lens' ListStreamingImages (Prelude.Maybe Prelude.Text)
listStreamingImages_nextToken = Lens.lens (\ListStreamingImages' {nextToken} -> nextToken) (\s@ListStreamingImages' {} a -> s {nextToken = a} :: ListStreamingImages)

-- | The studio ID.
listStreamingImages_studioId :: Lens.Lens' ListStreamingImages Prelude.Text
listStreamingImages_studioId = Lens.lens (\ListStreamingImages' {studioId} -> studioId) (\s@ListStreamingImages' {} a -> s {studioId = a} :: ListStreamingImages)

instance Core.AWSPager ListStreamingImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStreamingImagesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStreamingImagesResponse_streamingImages
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStreamingImages_nextToken
          Lens..~ rs
          Lens.^? listStreamingImagesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListStreamingImages where
  type
    AWSResponse ListStreamingImages =
      ListStreamingImagesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamingImagesResponse'
            Prelude.<$> ( x Core..?> "streamingImages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStreamingImages

instance Prelude.NFData ListStreamingImages

instance Core.ToHeaders ListStreamingImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListStreamingImages where
  toPath ListStreamingImages' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/streaming-images"
      ]

instance Core.ToQuery ListStreamingImages where
  toQuery ListStreamingImages' {..} =
    Prelude.mconcat
      [ "owner" Core.=: owner,
        "nextToken" Core.=: nextToken
      ]

-- | /See:/ 'newListStreamingImagesResponse' smart constructor.
data ListStreamingImagesResponse = ListStreamingImagesResponse'
  { -- | A collection of streaming images.
    streamingImages :: Prelude.Maybe [StreamingImage],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamingImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingImages', 'listStreamingImagesResponse_streamingImages' - A collection of streaming images.
--
-- 'nextToken', 'listStreamingImagesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listStreamingImagesResponse_httpStatus' - The response's http status code.
newListStreamingImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStreamingImagesResponse
newListStreamingImagesResponse pHttpStatus_ =
  ListStreamingImagesResponse'
    { streamingImages =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of streaming images.
listStreamingImagesResponse_streamingImages :: Lens.Lens' ListStreamingImagesResponse (Prelude.Maybe [StreamingImage])
listStreamingImagesResponse_streamingImages = Lens.lens (\ListStreamingImagesResponse' {streamingImages} -> streamingImages) (\s@ListStreamingImagesResponse' {} a -> s {streamingImages = a} :: ListStreamingImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listStreamingImagesResponse_nextToken :: Lens.Lens' ListStreamingImagesResponse (Prelude.Maybe Prelude.Text)
listStreamingImagesResponse_nextToken = Lens.lens (\ListStreamingImagesResponse' {nextToken} -> nextToken) (\s@ListStreamingImagesResponse' {} a -> s {nextToken = a} :: ListStreamingImagesResponse)

-- | The response's http status code.
listStreamingImagesResponse_httpStatus :: Lens.Lens' ListStreamingImagesResponse Prelude.Int
listStreamingImagesResponse_httpStatus = Lens.lens (\ListStreamingImagesResponse' {httpStatus} -> httpStatus) (\s@ListStreamingImagesResponse' {} a -> s {httpStatus = a} :: ListStreamingImagesResponse)

instance Prelude.NFData ListStreamingImagesResponse
