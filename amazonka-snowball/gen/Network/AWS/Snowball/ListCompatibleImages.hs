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
-- Module      : Network.AWS.Snowball.ListCompatibleImages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action returns a list of the different Amazon EC2 Amazon Machine
-- Images (AMIs) that are owned by your AWS account that would be supported
-- for use on a Snow device. Currently, supported AMIs are based on the
-- CentOS 7 (x86_64) - with Updates HVM, Ubuntu Server 14.04 LTS (HVM), and
-- Ubuntu 16.04 LTS - Xenial (HVM) images, available on the AWS
-- Marketplace.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListCompatibleImages
  ( -- * Creating a Request
    ListCompatibleImages (..),
    newListCompatibleImages,

    -- * Request Lenses
    listCompatibleImages_nextToken,
    listCompatibleImages_maxResults,

    -- * Destructuring the Response
    ListCompatibleImagesResponse (..),
    newListCompatibleImagesResponse,

    -- * Response Lenses
    listCompatibleImagesResponse_nextToken,
    listCompatibleImagesResponse_compatibleImages,
    listCompatibleImagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newListCompatibleImages' smart constructor.
data ListCompatibleImages = ListCompatibleImages'
  { -- | HTTP requests are stateless. To identify what object comes \"next\" in
    -- the list of compatible images, you can specify a value for @NextToken@
    -- as the starting point for your list of returned images.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results for the list of compatible images.
    -- Currently, a Snowball Edge device can store 10 AMIs.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCompatibleImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCompatibleImages_nextToken' - HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of compatible images, you can specify a value for @NextToken@
-- as the starting point for your list of returned images.
--
-- 'maxResults', 'listCompatibleImages_maxResults' - The maximum number of results for the list of compatible images.
-- Currently, a Snowball Edge device can store 10 AMIs.
newListCompatibleImages ::
  ListCompatibleImages
newListCompatibleImages =
  ListCompatibleImages'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of compatible images, you can specify a value for @NextToken@
-- as the starting point for your list of returned images.
listCompatibleImages_nextToken :: Lens.Lens' ListCompatibleImages (Core.Maybe Core.Text)
listCompatibleImages_nextToken = Lens.lens (\ListCompatibleImages' {nextToken} -> nextToken) (\s@ListCompatibleImages' {} a -> s {nextToken = a} :: ListCompatibleImages)

-- | The maximum number of results for the list of compatible images.
-- Currently, a Snowball Edge device can store 10 AMIs.
listCompatibleImages_maxResults :: Lens.Lens' ListCompatibleImages (Core.Maybe Core.Natural)
listCompatibleImages_maxResults = Lens.lens (\ListCompatibleImages' {maxResults} -> maxResults) (\s@ListCompatibleImages' {} a -> s {maxResults = a} :: ListCompatibleImages)

instance Core.AWSPager ListCompatibleImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCompatibleImagesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listCompatibleImagesResponse_compatibleImages
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCompatibleImages_nextToken
          Lens..~ rs
          Lens.^? listCompatibleImagesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListCompatibleImages where
  type
    AWSResponse ListCompatibleImages =
      ListCompatibleImagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCompatibleImagesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "CompatibleImages" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCompatibleImages

instance Core.NFData ListCompatibleImages

instance Core.ToHeaders ListCompatibleImages where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.ListCompatibleImages" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCompatibleImages where
  toJSON ListCompatibleImages' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListCompatibleImages where
  toPath = Core.const "/"

instance Core.ToQuery ListCompatibleImages where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCompatibleImagesResponse' smart constructor.
data ListCompatibleImagesResponse = ListCompatibleImagesResponse'
  { -- | Because HTTP requests are stateless, this is the starting point for your
    -- next list of returned images.
    nextToken :: Core.Maybe Core.Text,
    -- | A JSON-formatted object that describes a compatible AMI, including the
    -- ID and name for a Snow device AMI.
    compatibleImages :: Core.Maybe [CompatibleImage],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCompatibleImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCompatibleImagesResponse_nextToken' - Because HTTP requests are stateless, this is the starting point for your
-- next list of returned images.
--
-- 'compatibleImages', 'listCompatibleImagesResponse_compatibleImages' - A JSON-formatted object that describes a compatible AMI, including the
-- ID and name for a Snow device AMI.
--
-- 'httpStatus', 'listCompatibleImagesResponse_httpStatus' - The response's http status code.
newListCompatibleImagesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCompatibleImagesResponse
newListCompatibleImagesResponse pHttpStatus_ =
  ListCompatibleImagesResponse'
    { nextToken =
        Core.Nothing,
      compatibleImages = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Because HTTP requests are stateless, this is the starting point for your
-- next list of returned images.
listCompatibleImagesResponse_nextToken :: Lens.Lens' ListCompatibleImagesResponse (Core.Maybe Core.Text)
listCompatibleImagesResponse_nextToken = Lens.lens (\ListCompatibleImagesResponse' {nextToken} -> nextToken) (\s@ListCompatibleImagesResponse' {} a -> s {nextToken = a} :: ListCompatibleImagesResponse)

-- | A JSON-formatted object that describes a compatible AMI, including the
-- ID and name for a Snow device AMI.
listCompatibleImagesResponse_compatibleImages :: Lens.Lens' ListCompatibleImagesResponse (Core.Maybe [CompatibleImage])
listCompatibleImagesResponse_compatibleImages = Lens.lens (\ListCompatibleImagesResponse' {compatibleImages} -> compatibleImages) (\s@ListCompatibleImagesResponse' {} a -> s {compatibleImages = a} :: ListCompatibleImagesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCompatibleImagesResponse_httpStatus :: Lens.Lens' ListCompatibleImagesResponse Core.Int
listCompatibleImagesResponse_httpStatus = Lens.lens (\ListCompatibleImagesResponse' {httpStatus} -> httpStatus) (\s@ListCompatibleImagesResponse' {} a -> s {httpStatus = a} :: ListCompatibleImagesResponse)

instance Core.NFData ListCompatibleImagesResponse
