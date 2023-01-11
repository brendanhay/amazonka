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
-- Module      : Amazonka.Nimble.ListStreamingImages
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Nimble.ListStreamingImages
  ( -- * Creating a Request
    ListStreamingImages (..),
    newListStreamingImages,

    -- * Request Lenses
    listStreamingImages_nextToken,
    listStreamingImages_owner,
    listStreamingImages_studioId,

    -- * Destructuring the Response
    ListStreamingImagesResponse (..),
    newListStreamingImagesResponse,

    -- * Response Lenses
    listStreamingImagesResponse_nextToken,
    listStreamingImagesResponse_streamingImages,
    listStreamingImagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStreamingImages' smart constructor.
data ListStreamingImages = ListStreamingImages'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter this request to streaming images with the given owner
    owner :: Prelude.Maybe Prelude.Text,
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
-- 'nextToken', 'listStreamingImages_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'owner', 'listStreamingImages_owner' - Filter this request to streaming images with the given owner
--
-- 'studioId', 'listStreamingImages_studioId' - The studio ID.
newListStreamingImages ::
  -- | 'studioId'
  Prelude.Text ->
  ListStreamingImages
newListStreamingImages pStudioId_ =
  ListStreamingImages'
    { nextToken = Prelude.Nothing,
      owner = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listStreamingImages_nextToken :: Lens.Lens' ListStreamingImages (Prelude.Maybe Prelude.Text)
listStreamingImages_nextToken = Lens.lens (\ListStreamingImages' {nextToken} -> nextToken) (\s@ListStreamingImages' {} a -> s {nextToken = a} :: ListStreamingImages)

-- | Filter this request to streaming images with the given owner
listStreamingImages_owner :: Lens.Lens' ListStreamingImages (Prelude.Maybe Prelude.Text)
listStreamingImages_owner = Lens.lens (\ListStreamingImages' {owner} -> owner) (\s@ListStreamingImages' {} a -> s {owner = a} :: ListStreamingImages)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamingImagesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "streamingImages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStreamingImages where
  hashWithSalt _salt ListStreamingImages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData ListStreamingImages where
  rnf ListStreamingImages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders ListStreamingImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListStreamingImages where
  toPath ListStreamingImages' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-images"
      ]

instance Data.ToQuery ListStreamingImages where
  toQuery ListStreamingImages' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "owner" Data.=: owner
      ]

-- | /See:/ 'newListStreamingImagesResponse' smart constructor.
data ListStreamingImagesResponse = ListStreamingImagesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of streaming images.
    streamingImages :: Prelude.Maybe [StreamingImage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamingImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStreamingImagesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'streamingImages', 'listStreamingImagesResponse_streamingImages' - A collection of streaming images.
--
-- 'httpStatus', 'listStreamingImagesResponse_httpStatus' - The response's http status code.
newListStreamingImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStreamingImagesResponse
newListStreamingImagesResponse pHttpStatus_ =
  ListStreamingImagesResponse'
    { nextToken =
        Prelude.Nothing,
      streamingImages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listStreamingImagesResponse_nextToken :: Lens.Lens' ListStreamingImagesResponse (Prelude.Maybe Prelude.Text)
listStreamingImagesResponse_nextToken = Lens.lens (\ListStreamingImagesResponse' {nextToken} -> nextToken) (\s@ListStreamingImagesResponse' {} a -> s {nextToken = a} :: ListStreamingImagesResponse)

-- | A collection of streaming images.
listStreamingImagesResponse_streamingImages :: Lens.Lens' ListStreamingImagesResponse (Prelude.Maybe [StreamingImage])
listStreamingImagesResponse_streamingImages = Lens.lens (\ListStreamingImagesResponse' {streamingImages} -> streamingImages) (\s@ListStreamingImagesResponse' {} a -> s {streamingImages = a} :: ListStreamingImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStreamingImagesResponse_httpStatus :: Lens.Lens' ListStreamingImagesResponse Prelude.Int
listStreamingImagesResponse_httpStatus = Lens.lens (\ListStreamingImagesResponse' {httpStatus} -> httpStatus) (\s@ListStreamingImagesResponse' {} a -> s {httpStatus = a} :: ListStreamingImagesResponse)

instance Prelude.NFData ListStreamingImagesResponse where
  rnf ListStreamingImagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf streamingImages
      `Prelude.seq` Prelude.rnf httpStatus
