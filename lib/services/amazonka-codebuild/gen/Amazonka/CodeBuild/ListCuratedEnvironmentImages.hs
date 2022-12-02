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
-- Module      : Amazonka.CodeBuild.ListCuratedEnvironmentImages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about Docker images that are managed by CodeBuild.
module Amazonka.CodeBuild.ListCuratedEnvironmentImages
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

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCuratedEnvironmentImages' smart constructor.
data ListCuratedEnvironmentImages = ListCuratedEnvironmentImages'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCuratedEnvironmentImagesResponse'
            Prelude.<$> (x Data..?> "platforms" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCuratedEnvironmentImages
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ListCuratedEnvironmentImages where
  rnf _ = ()

instance Data.ToHeaders ListCuratedEnvironmentImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.ListCuratedEnvironmentImages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCuratedEnvironmentImages where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListCuratedEnvironmentImages where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCuratedEnvironmentImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCuratedEnvironmentImagesResponse' smart constructor.
data ListCuratedEnvironmentImagesResponse = ListCuratedEnvironmentImagesResponse'
  { -- | Information about supported platforms for Docker images that are managed
    -- by CodeBuild.
    platforms :: Prelude.Maybe [EnvironmentPlatform],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCuratedEnvironmentImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platforms', 'listCuratedEnvironmentImagesResponse_platforms' - Information about supported platforms for Docker images that are managed
-- by CodeBuild.
--
-- 'httpStatus', 'listCuratedEnvironmentImagesResponse_httpStatus' - The response's http status code.
newListCuratedEnvironmentImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCuratedEnvironmentImagesResponse
newListCuratedEnvironmentImagesResponse pHttpStatus_ =
  ListCuratedEnvironmentImagesResponse'
    { platforms =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about supported platforms for Docker images that are managed
-- by CodeBuild.
listCuratedEnvironmentImagesResponse_platforms :: Lens.Lens' ListCuratedEnvironmentImagesResponse (Prelude.Maybe [EnvironmentPlatform])
listCuratedEnvironmentImagesResponse_platforms = Lens.lens (\ListCuratedEnvironmentImagesResponse' {platforms} -> platforms) (\s@ListCuratedEnvironmentImagesResponse' {} a -> s {platforms = a} :: ListCuratedEnvironmentImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCuratedEnvironmentImagesResponse_httpStatus :: Lens.Lens' ListCuratedEnvironmentImagesResponse Prelude.Int
listCuratedEnvironmentImagesResponse_httpStatus = Lens.lens (\ListCuratedEnvironmentImagesResponse' {httpStatus} -> httpStatus) (\s@ListCuratedEnvironmentImagesResponse' {} a -> s {httpStatus = a} :: ListCuratedEnvironmentImagesResponse)

instance
  Prelude.NFData
    ListCuratedEnvironmentImagesResponse
  where
  rnf ListCuratedEnvironmentImagesResponse' {..} =
    Prelude.rnf platforms
      `Prelude.seq` Prelude.rnf httpStatus
