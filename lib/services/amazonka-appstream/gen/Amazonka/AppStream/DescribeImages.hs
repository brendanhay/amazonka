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
-- Module      : Amazonka.AppStream.DescribeImages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified images, if the
-- image names or image ARNs are provided. Otherwise, all images in the
-- account are described.
--
-- This operation returns paginated results.
module Amazonka.AppStream.DescribeImages
  ( -- * Creating a Request
    DescribeImages (..),
    newDescribeImages,

    -- * Request Lenses
    describeImages_nextToken,
    describeImages_type,
    describeImages_arns,
    describeImages_names,
    describeImages_maxResults,

    -- * Destructuring the Response
    DescribeImagesResponse (..),
    newDescribeImagesResponse,

    -- * Response Lenses
    describeImagesResponse_nextToken,
    describeImagesResponse_images,
    describeImagesResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of image (public, private, or shared) to describe.
    type' :: Prelude.Maybe VisibilityType,
    -- | The ARNs of the public, private, and shared images to describe.
    arns :: Prelude.Maybe [Prelude.Text],
    -- | The names of the public or private images to describe.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImages_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'type'', 'describeImages_type' - The type of image (public, private, or shared) to describe.
--
-- 'arns', 'describeImages_arns' - The ARNs of the public, private, and shared images to describe.
--
-- 'names', 'describeImages_names' - The names of the public or private images to describe.
--
-- 'maxResults', 'describeImages_maxResults' - The maximum size of each page of results.
newDescribeImages ::
  DescribeImages
newDescribeImages =
  DescribeImages'
    { nextToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      arns = Prelude.Nothing,
      names = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeImages_nextToken :: Lens.Lens' DescribeImages (Prelude.Maybe Prelude.Text)
describeImages_nextToken = Lens.lens (\DescribeImages' {nextToken} -> nextToken) (\s@DescribeImages' {} a -> s {nextToken = a} :: DescribeImages)

-- | The type of image (public, private, or shared) to describe.
describeImages_type :: Lens.Lens' DescribeImages (Prelude.Maybe VisibilityType)
describeImages_type = Lens.lens (\DescribeImages' {type'} -> type') (\s@DescribeImages' {} a -> s {type' = a} :: DescribeImages)

-- | The ARNs of the public, private, and shared images to describe.
describeImages_arns :: Lens.Lens' DescribeImages (Prelude.Maybe [Prelude.Text])
describeImages_arns = Lens.lens (\DescribeImages' {arns} -> arns) (\s@DescribeImages' {} a -> s {arns = a} :: DescribeImages) Prelude.. Lens.mapping Lens.coerced

-- | The names of the public or private images to describe.
describeImages_names :: Lens.Lens' DescribeImages (Prelude.Maybe [Prelude.Text])
describeImages_names = Lens.lens (\DescribeImages' {names} -> names) (\s@DescribeImages' {} a -> s {names = a} :: DescribeImages) Prelude.. Lens.mapping Lens.coerced

-- | The maximum size of each page of results.
describeImages_maxResults :: Lens.Lens' DescribeImages (Prelude.Maybe Prelude.Natural)
describeImages_maxResults = Lens.lens (\DescribeImages' {maxResults} -> maxResults) (\s@DescribeImages' {} a -> s {maxResults = a} :: DescribeImages)

instance Core.AWSPager DescribeImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImagesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImagesResponse_images Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeImages_nextToken
          Lens..~ rs
          Lens.^? describeImagesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeImages where
  type
    AWSResponse DescribeImages =
      DescribeImagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImagesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Images" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImages where
  hashWithSalt _salt DescribeImages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arns
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeImages where
  rnf DescribeImages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf arns
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DescribeImages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeImages where
  toJSON DescribeImages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Type" Data..=) Prelude.<$> type',
            ("Arns" Data..=) Prelude.<$> arns,
            ("Names" Data..=) Prelude.<$> names,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribeImages where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the images.
    images :: Prelude.Maybe [Image],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImagesResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'images', 'describeImagesResponse_images' - Information about the images.
--
-- 'httpStatus', 'describeImagesResponse_httpStatus' - The response's http status code.
newDescribeImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImagesResponse
newDescribeImagesResponse pHttpStatus_ =
  DescribeImagesResponse'
    { nextToken =
        Prelude.Nothing,
      images = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeImagesResponse_nextToken :: Lens.Lens' DescribeImagesResponse (Prelude.Maybe Prelude.Text)
describeImagesResponse_nextToken = Lens.lens (\DescribeImagesResponse' {nextToken} -> nextToken) (\s@DescribeImagesResponse' {} a -> s {nextToken = a} :: DescribeImagesResponse)

-- | Information about the images.
describeImagesResponse_images :: Lens.Lens' DescribeImagesResponse (Prelude.Maybe [Image])
describeImagesResponse_images = Lens.lens (\DescribeImagesResponse' {images} -> images) (\s@DescribeImagesResponse' {} a -> s {images = a} :: DescribeImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeImagesResponse_httpStatus :: Lens.Lens' DescribeImagesResponse Prelude.Int
describeImagesResponse_httpStatus = Lens.lens (\DescribeImagesResponse' {httpStatus} -> httpStatus) (\s@DescribeImagesResponse' {} a -> s {httpStatus = a} :: DescribeImagesResponse)

instance Prelude.NFData DescribeImagesResponse where
  rnf DescribeImagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf images
      `Prelude.seq` Prelude.rnf httpStatus
