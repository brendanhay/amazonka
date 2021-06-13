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
-- Module      : Network.AWS.Lightsail.GetContainerImages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the container images that are registered to your Amazon
-- Lightsail container service.
--
-- If you created a deployment on your Lightsail container service that
-- uses container images from a public registry like Docker Hub, those
-- images are not returned as part of this action. Those images are not
-- registered to your Lightsail container service.
module Network.AWS.Lightsail.GetContainerImages
  ( -- * Creating a Request
    GetContainerImages (..),
    newGetContainerImages,

    -- * Request Lenses
    getContainerImages_serviceName,

    -- * Destructuring the Response
    GetContainerImagesResponse (..),
    newGetContainerImagesResponse,

    -- * Response Lenses
    getContainerImagesResponse_containerImages,
    getContainerImagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetContainerImages' smart constructor.
data GetContainerImages = GetContainerImages'
  { -- | The name of the container service for which to return registered
    -- container images.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'getContainerImages_serviceName' - The name of the container service for which to return registered
-- container images.
newGetContainerImages ::
  -- | 'serviceName'
  Prelude.Text ->
  GetContainerImages
newGetContainerImages pServiceName_ =
  GetContainerImages' {serviceName = pServiceName_}

-- | The name of the container service for which to return registered
-- container images.
getContainerImages_serviceName :: Lens.Lens' GetContainerImages Prelude.Text
getContainerImages_serviceName = Lens.lens (\GetContainerImages' {serviceName} -> serviceName) (\s@GetContainerImages' {} a -> s {serviceName = a} :: GetContainerImages)

instance Core.AWSRequest GetContainerImages where
  type
    AWSResponse GetContainerImages =
      GetContainerImagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerImagesResponse'
            Prelude.<$> ( x Core..?> "containerImages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContainerImages

instance Prelude.NFData GetContainerImages

instance Core.ToHeaders GetContainerImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetContainerImages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetContainerImages where
  toJSON GetContainerImages' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("serviceName" Core..= serviceName)]
      )

instance Core.ToPath GetContainerImages where
  toPath = Prelude.const "/"

instance Core.ToQuery GetContainerImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContainerImagesResponse' smart constructor.
data GetContainerImagesResponse = GetContainerImagesResponse'
  { -- | An array of objects that describe container images that are registered
    -- to the container service.
    containerImages :: Prelude.Maybe [ContainerImage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerImages', 'getContainerImagesResponse_containerImages' - An array of objects that describe container images that are registered
-- to the container service.
--
-- 'httpStatus', 'getContainerImagesResponse_httpStatus' - The response's http status code.
newGetContainerImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContainerImagesResponse
newGetContainerImagesResponse pHttpStatus_ =
  GetContainerImagesResponse'
    { containerImages =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe container images that are registered
-- to the container service.
getContainerImagesResponse_containerImages :: Lens.Lens' GetContainerImagesResponse (Prelude.Maybe [ContainerImage])
getContainerImagesResponse_containerImages = Lens.lens (\GetContainerImagesResponse' {containerImages} -> containerImages) (\s@GetContainerImagesResponse' {} a -> s {containerImages = a} :: GetContainerImagesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getContainerImagesResponse_httpStatus :: Lens.Lens' GetContainerImagesResponse Prelude.Int
getContainerImagesResponse_httpStatus = Lens.lens (\GetContainerImagesResponse' {httpStatus} -> httpStatus) (\s@GetContainerImagesResponse' {} a -> s {httpStatus = a} :: GetContainerImagesResponse)

instance Prelude.NFData GetContainerImagesResponse
