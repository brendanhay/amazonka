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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of images created by the specified pipeline.
module Amazonka.ImageBuilder.ListImagePipelineImages
  ( -- * Creating a Request
    ListImagePipelineImages (..),
    newListImagePipelineImages,

    -- * Request Lenses
    listImagePipelineImages_filters,
    listImagePipelineImages_maxResults,
    listImagePipelineImages_nextToken,
    listImagePipelineImages_imagePipelineArn,

    -- * Destructuring the Response
    ListImagePipelineImagesResponse (..),
    newListImagePipelineImagesResponse,

    -- * Response Lenses
    listImagePipelineImagesResponse_imageSummaryList,
    listImagePipelineImagesResponse_nextToken,
    listImagePipelineImagesResponse_requestId,
    listImagePipelineImagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImagePipelineImages' smart constructor.
data ListImagePipelineImages = ListImagePipelineImages'
  { -- | Use the following filters to streamline results:
    --
    -- -   @name@
    --
    -- -   @version@
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'filters', 'listImagePipelineImages_filters' - Use the following filters to streamline results:
--
-- -   @name@
--
-- -   @version@
--
-- 'maxResults', 'listImagePipelineImages_maxResults' - The maximum items to return in a request.
--
-- 'nextToken', 'listImagePipelineImages_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'imagePipelineArn', 'listImagePipelineImages_imagePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline whose images you
-- want to view.
newListImagePipelineImages ::
  -- | 'imagePipelineArn'
  Prelude.Text ->
  ListImagePipelineImages
newListImagePipelineImages pImagePipelineArn_ =
  ListImagePipelineImages'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      imagePipelineArn = pImagePipelineArn_
    }

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

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listImagePipelineImages_nextToken :: Lens.Lens' ListImagePipelineImages (Prelude.Maybe Prelude.Text)
listImagePipelineImages_nextToken = Lens.lens (\ListImagePipelineImages' {nextToken} -> nextToken) (\s@ListImagePipelineImages' {} a -> s {nextToken = a} :: ListImagePipelineImages)

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
            Prelude.<$> ( x
                            Data..?> "imageSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImagePipelineImages where
  hashWithSalt _salt ListImagePipelineImages' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` imagePipelineArn

instance Prelude.NFData ListImagePipelineImages where
  rnf ListImagePipelineImages' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf imagePipelineArn

instance Data.ToHeaders ListImagePipelineImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListImagePipelineImages where
  toJSON ListImagePipelineImages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("imagePipelineArn" Data..= imagePipelineArn)
          ]
      )

instance Data.ToPath ListImagePipelineImages where
  toPath = Prelude.const "/ListImagePipelineImages"

instance Data.ToQuery ListImagePipelineImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImagePipelineImagesResponse' smart constructor.
data ListImagePipelineImagesResponse = ListImagePipelineImagesResponse'
  { -- | The list of images built by this pipeline.
    imageSummaryList :: Prelude.Maybe [ImageSummary],
    -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
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
-- 'imageSummaryList', 'listImagePipelineImagesResponse_imageSummaryList' - The list of images built by this pipeline.
--
-- 'nextToken', 'listImagePipelineImagesResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
--
-- 'requestId', 'listImagePipelineImagesResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'listImagePipelineImagesResponse_httpStatus' - The response's http status code.
newListImagePipelineImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImagePipelineImagesResponse
newListImagePipelineImagesResponse pHttpStatus_ =
  ListImagePipelineImagesResponse'
    { imageSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of images built by this pipeline.
listImagePipelineImagesResponse_imageSummaryList :: Lens.Lens' ListImagePipelineImagesResponse (Prelude.Maybe [ImageSummary])
listImagePipelineImagesResponse_imageSummaryList = Lens.lens (\ListImagePipelineImagesResponse' {imageSummaryList} -> imageSummaryList) (\s@ListImagePipelineImagesResponse' {} a -> s {imageSummaryList = a} :: ListImagePipelineImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listImagePipelineImagesResponse_nextToken :: Lens.Lens' ListImagePipelineImagesResponse (Prelude.Maybe Prelude.Text)
listImagePipelineImagesResponse_nextToken = Lens.lens (\ListImagePipelineImagesResponse' {nextToken} -> nextToken) (\s@ListImagePipelineImagesResponse' {} a -> s {nextToken = a} :: ListImagePipelineImagesResponse)

-- | The request ID that uniquely identifies this request.
listImagePipelineImagesResponse_requestId :: Lens.Lens' ListImagePipelineImagesResponse (Prelude.Maybe Prelude.Text)
listImagePipelineImagesResponse_requestId = Lens.lens (\ListImagePipelineImagesResponse' {requestId} -> requestId) (\s@ListImagePipelineImagesResponse' {} a -> s {requestId = a} :: ListImagePipelineImagesResponse)

-- | The response's http status code.
listImagePipelineImagesResponse_httpStatus :: Lens.Lens' ListImagePipelineImagesResponse Prelude.Int
listImagePipelineImagesResponse_httpStatus = Lens.lens (\ListImagePipelineImagesResponse' {httpStatus} -> httpStatus) (\s@ListImagePipelineImagesResponse' {} a -> s {httpStatus = a} :: ListImagePipelineImagesResponse)

instance
  Prelude.NFData
    ListImagePipelineImagesResponse
  where
  rnf ListImagePipelineImagesResponse' {..} =
    Prelude.rnf imageSummaryList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf requestId `Prelude.seq`
          Prelude.rnf httpStatus
