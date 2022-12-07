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
-- Module      : Amazonka.Snowball.ListCompatibleImages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action returns a list of the different Amazon EC2 Amazon Machine
-- Images (AMIs) that are owned by your Amazon Web Services accountthat
-- would be supported for use on a Snow device. Currently, supported AMIs
-- are based on the CentOS 7 (x86_64) - with Updates HVM, Ubuntu Server
-- 14.04 LTS (HVM), and Ubuntu 16.04 LTS - Xenial (HVM) images, available
-- on the Amazon Web Services Marketplace.
--
-- This operation returns paginated results.
module Amazonka.Snowball.ListCompatibleImages
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newListCompatibleImages' smart constructor.
data ListCompatibleImages = ListCompatibleImages'
  { -- | HTTP requests are stateless. To identify what object comes \"next\" in
    -- the list of compatible images, you can specify a value for @NextToken@
    -- as the starting point for your list of returned images.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results for the list of compatible images.
    -- Currently, a Snowball Edge device can store 10 AMIs.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of compatible images, you can specify a value for @NextToken@
-- as the starting point for your list of returned images.
listCompatibleImages_nextToken :: Lens.Lens' ListCompatibleImages (Prelude.Maybe Prelude.Text)
listCompatibleImages_nextToken = Lens.lens (\ListCompatibleImages' {nextToken} -> nextToken) (\s@ListCompatibleImages' {} a -> s {nextToken = a} :: ListCompatibleImages)

-- | The maximum number of results for the list of compatible images.
-- Currently, a Snowball Edge device can store 10 AMIs.
listCompatibleImages_maxResults :: Lens.Lens' ListCompatibleImages (Prelude.Maybe Prelude.Natural)
listCompatibleImages_maxResults = Lens.lens (\ListCompatibleImages' {maxResults} -> maxResults) (\s@ListCompatibleImages' {} a -> s {maxResults = a} :: ListCompatibleImages)

instance Core.AWSPager ListCompatibleImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCompatibleImagesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCompatibleImagesResponse_compatibleImages
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCompatibleImages_nextToken
          Lens..~ rs
          Lens.^? listCompatibleImagesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCompatibleImages where
  type
    AWSResponse ListCompatibleImages =
      ListCompatibleImagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCompatibleImagesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "CompatibleImages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCompatibleImages where
  hashWithSalt _salt ListCompatibleImages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCompatibleImages where
  rnf ListCompatibleImages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListCompatibleImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.ListCompatibleImages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCompatibleImages where
  toJSON ListCompatibleImages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListCompatibleImages where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCompatibleImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCompatibleImagesResponse' smart constructor.
data ListCompatibleImagesResponse = ListCompatibleImagesResponse'
  { -- | Because HTTP requests are stateless, this is the starting point for your
    -- next list of returned images.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A JSON-formatted object that describes a compatible AMI, including the
    -- ID and name for a Snow device AMI.
    compatibleImages :: Prelude.Maybe [CompatibleImage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListCompatibleImagesResponse
newListCompatibleImagesResponse pHttpStatus_ =
  ListCompatibleImagesResponse'
    { nextToken =
        Prelude.Nothing,
      compatibleImages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Because HTTP requests are stateless, this is the starting point for your
-- next list of returned images.
listCompatibleImagesResponse_nextToken :: Lens.Lens' ListCompatibleImagesResponse (Prelude.Maybe Prelude.Text)
listCompatibleImagesResponse_nextToken = Lens.lens (\ListCompatibleImagesResponse' {nextToken} -> nextToken) (\s@ListCompatibleImagesResponse' {} a -> s {nextToken = a} :: ListCompatibleImagesResponse)

-- | A JSON-formatted object that describes a compatible AMI, including the
-- ID and name for a Snow device AMI.
listCompatibleImagesResponse_compatibleImages :: Lens.Lens' ListCompatibleImagesResponse (Prelude.Maybe [CompatibleImage])
listCompatibleImagesResponse_compatibleImages = Lens.lens (\ListCompatibleImagesResponse' {compatibleImages} -> compatibleImages) (\s@ListCompatibleImagesResponse' {} a -> s {compatibleImages = a} :: ListCompatibleImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCompatibleImagesResponse_httpStatus :: Lens.Lens' ListCompatibleImagesResponse Prelude.Int
listCompatibleImagesResponse_httpStatus = Lens.lens (\ListCompatibleImagesResponse' {httpStatus} -> httpStatus) (\s@ListCompatibleImagesResponse' {} a -> s {httpStatus = a} :: ListCompatibleImagesResponse)

instance Prelude.NFData ListCompatibleImagesResponse where
  rnf ListCompatibleImagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf compatibleImages
      `Prelude.seq` Prelude.rnf httpStatus
