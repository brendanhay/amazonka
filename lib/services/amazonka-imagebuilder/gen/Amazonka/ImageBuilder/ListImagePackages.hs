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
-- Module      : Amazonka.ImageBuilder.ListImagePackages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the Packages that are associated with an Image Build Version, as
-- determined by Amazon Web Services Systems Manager Inventory at build
-- time.
module Amazonka.ImageBuilder.ListImagePackages
  ( -- * Creating a Request
    ListImagePackages (..),
    newListImagePackages,

    -- * Request Lenses
    listImagePackages_nextToken,
    listImagePackages_maxResults,
    listImagePackages_imageBuildVersionArn,

    -- * Destructuring the Response
    ListImagePackagesResponse (..),
    newListImagePackagesResponse,

    -- * Response Lenses
    listImagePackagesResponse_nextToken,
    listImagePackagesResponse_requestId,
    listImagePackagesResponse_imagePackageList,
    listImagePackagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImagePackages' smart constructor.
data ListImagePackages = ListImagePackages'
  { -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maxiumum number of results to return from the ListImagePackages
    -- request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filter results for the ListImagePackages request by the Image Build
    -- Version ARN
    imageBuildVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImagePackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImagePackages_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'maxResults', 'listImagePackages_maxResults' - The maxiumum number of results to return from the ListImagePackages
-- request.
--
-- 'imageBuildVersionArn', 'listImagePackages_imageBuildVersionArn' - Filter results for the ListImagePackages request by the Image Build
-- Version ARN
newListImagePackages ::
  -- | 'imageBuildVersionArn'
  Prelude.Text ->
  ListImagePackages
newListImagePackages pImageBuildVersionArn_ =
  ListImagePackages'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      imageBuildVersionArn = pImageBuildVersionArn_
    }

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listImagePackages_nextToken :: Lens.Lens' ListImagePackages (Prelude.Maybe Prelude.Text)
listImagePackages_nextToken = Lens.lens (\ListImagePackages' {nextToken} -> nextToken) (\s@ListImagePackages' {} a -> s {nextToken = a} :: ListImagePackages)

-- | The maxiumum number of results to return from the ListImagePackages
-- request.
listImagePackages_maxResults :: Lens.Lens' ListImagePackages (Prelude.Maybe Prelude.Natural)
listImagePackages_maxResults = Lens.lens (\ListImagePackages' {maxResults} -> maxResults) (\s@ListImagePackages' {} a -> s {maxResults = a} :: ListImagePackages)

-- | Filter results for the ListImagePackages request by the Image Build
-- Version ARN
listImagePackages_imageBuildVersionArn :: Lens.Lens' ListImagePackages Prelude.Text
listImagePackages_imageBuildVersionArn = Lens.lens (\ListImagePackages' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@ListImagePackages' {} a -> s {imageBuildVersionArn = a} :: ListImagePackages)

instance Core.AWSRequest ListImagePackages where
  type
    AWSResponse ListImagePackages =
      ListImagePackagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImagePackagesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> ( x Data..?> "imagePackageList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImagePackages where
  hashWithSalt _salt ListImagePackages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` imageBuildVersionArn

instance Prelude.NFData ListImagePackages where
  rnf ListImagePackages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf imageBuildVersionArn

instance Data.ToHeaders ListImagePackages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListImagePackages where
  toJSON ListImagePackages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ( "imageBuildVersionArn"
                  Data..= imageBuildVersionArn
              )
          ]
      )

instance Data.ToPath ListImagePackages where
  toPath = Prelude.const "/ListImagePackages"

instance Data.ToQuery ListImagePackages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImagePackagesResponse' smart constructor.
data ListImagePackagesResponse = ListImagePackagesResponse'
  { -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The list of Image Packages returned in the response.
    imagePackageList :: Prelude.Maybe [ImagePackage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImagePackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImagePackagesResponse_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'requestId', 'listImagePackagesResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'imagePackageList', 'listImagePackagesResponse_imagePackageList' - The list of Image Packages returned in the response.
--
-- 'httpStatus', 'listImagePackagesResponse_httpStatus' - The response's http status code.
newListImagePackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImagePackagesResponse
newListImagePackagesResponse pHttpStatus_ =
  ListImagePackagesResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      imagePackageList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listImagePackagesResponse_nextToken :: Lens.Lens' ListImagePackagesResponse (Prelude.Maybe Prelude.Text)
listImagePackagesResponse_nextToken = Lens.lens (\ListImagePackagesResponse' {nextToken} -> nextToken) (\s@ListImagePackagesResponse' {} a -> s {nextToken = a} :: ListImagePackagesResponse)

-- | The request ID that uniquely identifies this request.
listImagePackagesResponse_requestId :: Lens.Lens' ListImagePackagesResponse (Prelude.Maybe Prelude.Text)
listImagePackagesResponse_requestId = Lens.lens (\ListImagePackagesResponse' {requestId} -> requestId) (\s@ListImagePackagesResponse' {} a -> s {requestId = a} :: ListImagePackagesResponse)

-- | The list of Image Packages returned in the response.
listImagePackagesResponse_imagePackageList :: Lens.Lens' ListImagePackagesResponse (Prelude.Maybe [ImagePackage])
listImagePackagesResponse_imagePackageList = Lens.lens (\ListImagePackagesResponse' {imagePackageList} -> imagePackageList) (\s@ListImagePackagesResponse' {} a -> s {imagePackageList = a} :: ListImagePackagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImagePackagesResponse_httpStatus :: Lens.Lens' ListImagePackagesResponse Prelude.Int
listImagePackagesResponse_httpStatus = Lens.lens (\ListImagePackagesResponse' {httpStatus} -> httpStatus) (\s@ListImagePackagesResponse' {} a -> s {httpStatus = a} :: ListImagePackagesResponse)

instance Prelude.NFData ListImagePackagesResponse where
  rnf ListImagePackagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf imagePackageList
      `Prelude.seq` Prelude.rnf httpStatus
