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
-- Module      : Network.AWS.CodeBuild.ListCuratedEnvironmentImages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about Docker images that are managed by AWS CodeBuild.
module Network.AWS.CodeBuild.ListCuratedEnvironmentImages
  ( -- * Creating a Request
    ListCuratedEnvironmentImages (..),
    newListCuratedEnvironmentImages,

    -- * Destructuring the Response
    ListCuratedEnvironmentImagesResponse (..),
    newListCuratedEnvironmentImagesResponse,

    -- * Response Lenses
    listCuratedEnvironmentImagesResponse_platforms,
    listCuratedEnvironmentImagesResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCuratedEnvironmentImages' smart constructor.
data ListCuratedEnvironmentImages = ListCuratedEnvironmentImages'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCuratedEnvironmentImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListCuratedEnvironmentImages ::
  ListCuratedEnvironmentImages
newListCuratedEnvironmentImages =
  ListCuratedEnvironmentImages'

instance Core.AWSRequest ListCuratedEnvironmentImages where
  type
    AWSResponse ListCuratedEnvironmentImages =
      ListCuratedEnvironmentImagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCuratedEnvironmentImagesResponse'
            Core.<$> (x Core..?> "platforms" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCuratedEnvironmentImages

instance Core.NFData ListCuratedEnvironmentImages

instance Core.ToHeaders ListCuratedEnvironmentImages where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListCuratedEnvironmentImages" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCuratedEnvironmentImages where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath ListCuratedEnvironmentImages where
  toPath = Core.const "/"

instance Core.ToQuery ListCuratedEnvironmentImages where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCuratedEnvironmentImagesResponse' smart constructor.
data ListCuratedEnvironmentImagesResponse = ListCuratedEnvironmentImagesResponse'
  { -- | Information about supported platforms for Docker images that are managed
    -- by AWS CodeBuild.
    platforms :: Core.Maybe [EnvironmentPlatform],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCuratedEnvironmentImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platforms', 'listCuratedEnvironmentImagesResponse_platforms' - Information about supported platforms for Docker images that are managed
-- by AWS CodeBuild.
--
-- 'httpStatus', 'listCuratedEnvironmentImagesResponse_httpStatus' - The response's http status code.
newListCuratedEnvironmentImagesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCuratedEnvironmentImagesResponse
newListCuratedEnvironmentImagesResponse pHttpStatus_ =
  ListCuratedEnvironmentImagesResponse'
    { platforms =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about supported platforms for Docker images that are managed
-- by AWS CodeBuild.
listCuratedEnvironmentImagesResponse_platforms :: Lens.Lens' ListCuratedEnvironmentImagesResponse (Core.Maybe [EnvironmentPlatform])
listCuratedEnvironmentImagesResponse_platforms = Lens.lens (\ListCuratedEnvironmentImagesResponse' {platforms} -> platforms) (\s@ListCuratedEnvironmentImagesResponse' {} a -> s {platforms = a} :: ListCuratedEnvironmentImagesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCuratedEnvironmentImagesResponse_httpStatus :: Lens.Lens' ListCuratedEnvironmentImagesResponse Core.Int
listCuratedEnvironmentImagesResponse_httpStatus = Lens.lens (\ListCuratedEnvironmentImagesResponse' {httpStatus} -> httpStatus) (\s@ListCuratedEnvironmentImagesResponse' {} a -> s {httpStatus = a} :: ListCuratedEnvironmentImagesResponse)

instance
  Core.NFData
    ListCuratedEnvironmentImagesResponse
