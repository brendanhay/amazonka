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
-- Module      : Amazonka.ImageBuilder.ListImagePipelineImages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of images created by the specified pipeline.
module Amazonka.ImageBuilder.ListImagePipelineImages
  ( -- * Creating a Request
    ListImagePipelineImages (..),
    newListImagePipelineImages,

    -- * Request Lenses
    listImagePipelineImages_nextToken,
    listImagePipelineImages_filters,
    listImagePipelineImages_maxResults,
    listImagePipelineImages_imagePipelineArn,

    -- * Destructuring the Response
    ListImagePipelineImagesResponse (..),
    newListImagePipelineImagesResponse,

    -- * Response Lenses
    listImagePipelineImagesResponse_nextToken,
    listImagePipelineImagesResponse_requestId,
    listImagePipelineImagesResponse_imageSummaryList,
    listImagePipelineImagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImagePipelineImages' smart constructor.
data ListImagePipelineImages = ListImagePipelineImages'
  { -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use the following filters to streamline results:
    --
    -- -   @name@
    --
    -- -   @version@
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the image pipeline whose images you
    -- want to view.
    imagePipelineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImagePipelineImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImagePipelineImages_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'filters', 'listImagePipelineImages_filters' - Use the following filters to streamline results:
--
-- -   @name@
--
-- -   @version@
--
-- 'maxResults', 'listImagePipelineImages_maxResults' - The maximum items to return in a request.
--
-- 'imagePipelineArn', 'listImagePipelineImages_imagePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline whose images you
-- want to view.
newListImagePipelineImages ::
  -- | 'imagePipelineArn'
  Prelude.Text ->
  ListImagePipelineImages
newListImagePipelineImages pImagePipelineArn_ =
  ListImagePipelineImages'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      imagePipelineArn = pImagePipelineArn_
    }

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listImagePipelineImages_nextToken :: Lens.Lens' ListImagePipelineImages (Prelude.Maybe Prelude.Text)
listImagePipelineImages_nextToken = Lens.lens (\ListImagePipelineImages' {nextToken} -> nextToken) (\s@ListImagePipelineImages' {} a -> s {nextToken = a} :: ListImagePipelineImages)

-- | Use the following filters to streamline results:
--
-- -   @name@
--
-- -   @version@
listImagePipelineImages_filters :: Lens.Lens' ListImagePipelineImages (Prelude.Maybe (Prelude.NonEmpty Filter))
listImagePipelineImages_filters = Lens.lens (\ListImagePipelineImages' {filters} -> filters) (\s@ListImagePipelineImages' {} a -> s {filters = a} :: ListImagePipelineImages) Prelude.. Lens.mapping Lens.coerced

-- | The maximum items to return in a request.
listImagePipelineImages_maxResults :: Lens.Lens' ListImagePipelineImages (Prelude.Maybe Prelude.Natural)
listImagePipelineImages_maxResults = Lens.lens (\ListImagePipelineImages' {maxResults} -> maxResults) (\s@ListImagePipelineImages' {} a -> s {maxResults = a} :: ListImagePipelineImages)

-- | The Amazon Resource Name (ARN) of the image pipeline whose images you
-- want to view.
listImagePipelineImages_imagePipelineArn :: Lens.Lens' ListImagePipelineImages Prelude.Text
listImagePipelineImages_imagePipelineArn = Lens.lens (\ListImagePipelineImages' {imagePipelineArn} -> imagePipelineArn) (\s@ListImagePipelineImages' {} a -> s {imagePipelineArn = a} :: ListImagePipelineImages)

instance Core.AWSRequest ListImagePipelineImages where
  type
    AWSResponse ListImagePipelineImages =
      ListImagePipelineImagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImagePipelineImagesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "requestId")
            Prelude.<*> ( x Core..?> "imageSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImagePipelineImages where
  hashWithSalt _salt ListImagePipelineImages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` imagePipelineArn

instance Prelude.NFData ListImagePipelineImages where
  rnf ListImagePipelineImages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf imagePipelineArn

instance Core.ToHeaders ListImagePipelineImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListImagePipelineImages where
  toJSON ListImagePipelineImages' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("imagePipelineArn" Core..= imagePipelineArn)
          ]
      )

instance Core.ToPath ListImagePipelineImages where
  toPath = Prelude.const "/ListImagePipelineImages"

instance Core.ToQuery ListImagePipelineImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImagePipelineImagesResponse' smart constructor.
data ListImagePipelineImagesResponse = ListImagePipelineImagesResponse'
  { -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The list of images built by this pipeline.
    imageSummaryList :: Prelude.Maybe [ImageSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImagePipelineImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImagePipelineImagesResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
--
-- 'requestId', 'listImagePipelineImagesResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'imageSummaryList', 'listImagePipelineImagesResponse_imageSummaryList' - The list of images built by this pipeline.
--
-- 'httpStatus', 'listImagePipelineImagesResponse_httpStatus' - The response's http status code.
newListImagePipelineImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImagePipelineImagesResponse
newListImagePipelineImagesResponse pHttpStatus_ =
  ListImagePipelineImagesResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      imageSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listImagePipelineImagesResponse_nextToken :: Lens.Lens' ListImagePipelineImagesResponse (Prelude.Maybe Prelude.Text)
listImagePipelineImagesResponse_nextToken = Lens.lens (\ListImagePipelineImagesResponse' {nextToken} -> nextToken) (\s@ListImagePipelineImagesResponse' {} a -> s {nextToken = a} :: ListImagePipelineImagesResponse)

-- | The request ID that uniquely identifies this request.
listImagePipelineImagesResponse_requestId :: Lens.Lens' ListImagePipelineImagesResponse (Prelude.Maybe Prelude.Text)
listImagePipelineImagesResponse_requestId = Lens.lens (\ListImagePipelineImagesResponse' {requestId} -> requestId) (\s@ListImagePipelineImagesResponse' {} a -> s {requestId = a} :: ListImagePipelineImagesResponse)

-- | The list of images built by this pipeline.
listImagePipelineImagesResponse_imageSummaryList :: Lens.Lens' ListImagePipelineImagesResponse (Prelude.Maybe [ImageSummary])
listImagePipelineImagesResponse_imageSummaryList = Lens.lens (\ListImagePipelineImagesResponse' {imageSummaryList} -> imageSummaryList) (\s@ListImagePipelineImagesResponse' {} a -> s {imageSummaryList = a} :: ListImagePipelineImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImagePipelineImagesResponse_httpStatus :: Lens.Lens' ListImagePipelineImagesResponse Prelude.Int
listImagePipelineImagesResponse_httpStatus = Lens.lens (\ListImagePipelineImagesResponse' {httpStatus} -> httpStatus) (\s@ListImagePipelineImagesResponse' {} a -> s {httpStatus = a} :: ListImagePipelineImagesResponse)

instance
  Prelude.NFData
    ListImagePipelineImagesResponse
  where
  rnf ListImagePipelineImagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf imageSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
