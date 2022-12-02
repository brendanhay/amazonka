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
-- Module      : Amazonka.ImageBuilder.ListImages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of images that you have access to.
module Amazonka.ImageBuilder.ListImages
  ( -- * Creating a Request
    ListImages (..),
    newListImages,

    -- * Request Lenses
    listImages_nextToken,
    listImages_byName,
    listImages_filters,
    listImages_owner,
    listImages_maxResults,
    listImages_includeDeprecated,

    -- * Destructuring the Response
    ListImagesResponse (..),
    newListImagesResponse,

    -- * Response Lenses
    listImagesResponse_nextToken,
    listImagesResponse_imageVersionList,
    listImagesResponse_requestId,
    listImagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImages' smart constructor.
data ListImages = ListImages'
  { -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Requests a list of images with a specific recipe name.
    byName :: Prelude.Maybe Prelude.Bool,
    -- | Use the following filters to streamline results:
    --
    -- -   @name@
    --
    -- -   @osVersion@
    --
    -- -   @platform@
    --
    -- -   @type@
    --
    -- -   @version@
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | The owner defines which images you want to list. By default, this
    -- request will only show images owned by your account. You can use this
    -- field to specify if you want to view images owned by yourself, by
    -- Amazon, or those images that have been shared with you by other
    -- customers.
    owner :: Prelude.Maybe Ownership,
    -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Includes deprecated images in the response list.
    includeDeprecated :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImages_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'byName', 'listImages_byName' - Requests a list of images with a specific recipe name.
--
-- 'filters', 'listImages_filters' - Use the following filters to streamline results:
--
-- -   @name@
--
-- -   @osVersion@
--
-- -   @platform@
--
-- -   @type@
--
-- -   @version@
--
-- 'owner', 'listImages_owner' - The owner defines which images you want to list. By default, this
-- request will only show images owned by your account. You can use this
-- field to specify if you want to view images owned by yourself, by
-- Amazon, or those images that have been shared with you by other
-- customers.
--
-- 'maxResults', 'listImages_maxResults' - The maximum items to return in a request.
--
-- 'includeDeprecated', 'listImages_includeDeprecated' - Includes deprecated images in the response list.
newListImages ::
  ListImages
newListImages =
  ListImages'
    { nextToken = Prelude.Nothing,
      byName = Prelude.Nothing,
      filters = Prelude.Nothing,
      owner = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      includeDeprecated = Prelude.Nothing
    }

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listImages_nextToken :: Lens.Lens' ListImages (Prelude.Maybe Prelude.Text)
listImages_nextToken = Lens.lens (\ListImages' {nextToken} -> nextToken) (\s@ListImages' {} a -> s {nextToken = a} :: ListImages)

-- | Requests a list of images with a specific recipe name.
listImages_byName :: Lens.Lens' ListImages (Prelude.Maybe Prelude.Bool)
listImages_byName = Lens.lens (\ListImages' {byName} -> byName) (\s@ListImages' {} a -> s {byName = a} :: ListImages)

-- | Use the following filters to streamline results:
--
-- -   @name@
--
-- -   @osVersion@
--
-- -   @platform@
--
-- -   @type@
--
-- -   @version@
listImages_filters :: Lens.Lens' ListImages (Prelude.Maybe (Prelude.NonEmpty Filter))
listImages_filters = Lens.lens (\ListImages' {filters} -> filters) (\s@ListImages' {} a -> s {filters = a} :: ListImages) Prelude.. Lens.mapping Lens.coerced

-- | The owner defines which images you want to list. By default, this
-- request will only show images owned by your account. You can use this
-- field to specify if you want to view images owned by yourself, by
-- Amazon, or those images that have been shared with you by other
-- customers.
listImages_owner :: Lens.Lens' ListImages (Prelude.Maybe Ownership)
listImages_owner = Lens.lens (\ListImages' {owner} -> owner) (\s@ListImages' {} a -> s {owner = a} :: ListImages)

-- | The maximum items to return in a request.
listImages_maxResults :: Lens.Lens' ListImages (Prelude.Maybe Prelude.Natural)
listImages_maxResults = Lens.lens (\ListImages' {maxResults} -> maxResults) (\s@ListImages' {} a -> s {maxResults = a} :: ListImages)

-- | Includes deprecated images in the response list.
listImages_includeDeprecated :: Lens.Lens' ListImages (Prelude.Maybe Prelude.Bool)
listImages_includeDeprecated = Lens.lens (\ListImages' {includeDeprecated} -> includeDeprecated) (\s@ListImages' {} a -> s {includeDeprecated = a} :: ListImages)

instance Core.AWSRequest ListImages where
  type AWSResponse ListImages = ListImagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImagesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "imageVersionList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImages where
  hashWithSalt _salt ListImages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` byName
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` includeDeprecated

instance Prelude.NFData ListImages where
  rnf ListImages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf byName
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf includeDeprecated

instance Data.ToHeaders ListImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListImages where
  toJSON ListImages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("byName" Data..=) Prelude.<$> byName,
            ("filters" Data..=) Prelude.<$> filters,
            ("owner" Data..=) Prelude.<$> owner,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("includeDeprecated" Data..=)
              Prelude.<$> includeDeprecated
          ]
      )

instance Data.ToPath ListImages where
  toPath = Prelude.const "/ListImages"

instance Data.ToQuery ListImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImagesResponse' smart constructor.
data ListImagesResponse = ListImagesResponse'
  { -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of image semantic versions.
    --
    -- The semantic version has four nodes:
    -- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
    -- first three, and can filter on all of them.
    --
    -- __Filtering:__ With semantic versioning, you have the flexibility to use
    -- wildcards (x) to specify the most recent versions or nodes when
    -- selecting the base image or components for your recipe. When you use a
    -- wildcard in any node, all nodes to the right of the first wildcard must
    -- also be wildcards.
    imageVersionList :: Prelude.Maybe [ImageVersion],
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImagesResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
--
-- 'imageVersionList', 'listImagesResponse_imageVersionList' - The list of image semantic versions.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Filtering:__ With semantic versioning, you have the flexibility to use
-- wildcards (x) to specify the most recent versions or nodes when
-- selecting the base image or components for your recipe. When you use a
-- wildcard in any node, all nodes to the right of the first wildcard must
-- also be wildcards.
--
-- 'requestId', 'listImagesResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'listImagesResponse_httpStatus' - The response's http status code.
newListImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImagesResponse
newListImagesResponse pHttpStatus_ =
  ListImagesResponse'
    { nextToken = Prelude.Nothing,
      imageVersionList = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listImagesResponse_nextToken :: Lens.Lens' ListImagesResponse (Prelude.Maybe Prelude.Text)
listImagesResponse_nextToken = Lens.lens (\ListImagesResponse' {nextToken} -> nextToken) (\s@ListImagesResponse' {} a -> s {nextToken = a} :: ListImagesResponse)

-- | The list of image semantic versions.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Filtering:__ With semantic versioning, you have the flexibility to use
-- wildcards (x) to specify the most recent versions or nodes when
-- selecting the base image or components for your recipe. When you use a
-- wildcard in any node, all nodes to the right of the first wildcard must
-- also be wildcards.
listImagesResponse_imageVersionList :: Lens.Lens' ListImagesResponse (Prelude.Maybe [ImageVersion])
listImagesResponse_imageVersionList = Lens.lens (\ListImagesResponse' {imageVersionList} -> imageVersionList) (\s@ListImagesResponse' {} a -> s {imageVersionList = a} :: ListImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The request ID that uniquely identifies this request.
listImagesResponse_requestId :: Lens.Lens' ListImagesResponse (Prelude.Maybe Prelude.Text)
listImagesResponse_requestId = Lens.lens (\ListImagesResponse' {requestId} -> requestId) (\s@ListImagesResponse' {} a -> s {requestId = a} :: ListImagesResponse)

-- | The response's http status code.
listImagesResponse_httpStatus :: Lens.Lens' ListImagesResponse Prelude.Int
listImagesResponse_httpStatus = Lens.lens (\ListImagesResponse' {httpStatus} -> httpStatus) (\s@ListImagesResponse' {} a -> s {httpStatus = a} :: ListImagesResponse)

instance Prelude.NFData ListImagesResponse where
  rnf ListImagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf imageVersionList
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
